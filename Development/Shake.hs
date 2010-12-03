{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Development.Shake (
    -- * The top-level monadic interface
    Shake, shake,
    Rule, CreatesFiles, (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    want, oracle,
    
    -- * The monadic interface used by rule bodies
    Act, need, query,
    
    -- * Oracles, the default oracle and wrappers for the questions it can answer
    Oracle, Question, Answer, defaultOracle, ls
  ) where

import Development.Shake.Utilities

import Control.Applicative (Applicative)

import Control.Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class

import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

import System.Directory
import System.FilePath.Glob
import System.Time (CalendarTime, toCalendarTime)

import System.IO.Unsafe


readMaybe :: Read a => String -> Maybe a
readMaybe xs = case reads xs of
    [(x, "")] -> Just x
    _         -> Nothing


type CreatesFiles = [FilePath]
type Rule o = FilePath -> Maybe (CreatesFiles, Act o ())

data ShakeState o = SS {
    ss_rules :: [Rule o],
    ss_database :: Database o
  }

data ShakeEnv o = SE {
    se_oracle :: o
  }

-- TODO: should Shake really be an IO monad?
newtype Shake o a = Shake { unShake :: Reader.ReaderT (ShakeEnv o) (State.StateT (ShakeState o) IO) a }
                deriving (Functor, Applicative, Monad, MonadIO)

runShake :: ShakeEnv o -> ShakeState o -> Shake o a -> IO (a, ShakeState o)
runShake e s mx = State.runStateT (Reader.runReaderT (unShake mx) e) s

getShakeState :: Shake o (ShakeState o)
getShakeState = Shake (lift State.get)

putShakeState :: ShakeState o -> Shake o ()
putShakeState s = Shake (lift (State.put s))

modifyShakeState :: (ShakeState o -> ShakeState o) -> Shake o ()
modifyShakeState f = Shake (lift (State.modify f))

askShakeEnv :: Shake o (ShakeEnv o)
askShakeEnv = Shake Reader.ask

localShakeEnv :: (ShakeEnv o -> ShakeEnv o) -> Shake o a -> Shake o a
localShakeEnv f mx = Shake (Reader.local f (unShake mx))


type ModTime = CalendarTime

getModTime :: FilePath -> IO (Maybe ModTime)
getModTime fp = handleDoesNotExist (getModificationTime fp >>= toCalendarTime)


type History o = [QA o]
data QA o = Oracle (Question o) (Answer o)
          | Need [(FilePath, ModTime)]

deriving instance Oracle o => Show (QA o)
deriving instance Oracle o => Read (QA o)

type Database o = Map FilePath (Status o)
data Status o = Dirty (History o)
              | Clean (History o) ModTime
              deriving (Show, Read)


data ActState o = AS {
    as_this_history :: History o,
    as_database :: Database o
  }

data ActEnv o = AE {
    ae_global_rules :: [Rule o],
    ae_global_oracle :: o
  }

newtype Act o a = Act { unAct :: Reader.ReaderT (ActEnv o) (State.StateT (ActState o) IO) a }
              deriving (Functor, Applicative, Monad, MonadIO)

runAct :: ActEnv o -> ActState o -> Act o a -> IO (a, ActState o)
runAct e s mx = State.runStateT (Reader.runReaderT (unAct mx) e) s

getActState :: Act o (ActState o)
getActState = Act (lift State.get)

modifyActState :: (ActState o -> ActState o) -> Act o ()
modifyActState f = Act (lift (State.modify f))

askActEnv :: Act o (ActEnv o)
askActEnv = Act Reader.ask


shake :: Shake (LsT StringOracle) () -> IO ()
shake = shakeWithOracle defaultOracle

shakeWithOracle :: Oracle o => o -> Shake o () -> IO ()
shakeWithOracle orac mx = do
    mb_s <- handleDoesNotExist (readFile ".openshake-db")
    let mb_init_db = mb_s >>= readMaybe -- The database may not parse with a new oracle, so we will throw it away
    unless (isJust mb_init_db) $ putStrLn "Database not present or unreadable: doing a full rebuild"
    
    ((), final_s) <- runShake (SE { se_oracle = orac }) (SS { ss_rules = [], ss_database = fromMaybe M.empty mb_init_db }) mx
    
    writeFile ".openshake-db" (show $ ss_database final_s)


newtype StringOracle = SO { _unSO :: (String, String) -> [String] }

instance Oracle StringOracle where
    data Question StringOracle = SQ { _unSQ :: (String, String) }
                               deriving (Show, Read, Eq)
    data Answer StringOracle = SA { unSA :: [String] }
                             deriving (Show, Read, Eq)
    askOracle (SO o) (SQ q) = SA (o q)

defaultOracle :: LsT StringOracle
defaultOracle = QAT LsQ $ SO go
  where
    -- Doesn't work because we want to do things like "ls *.c", and since the shell does globbing we need to go through it
    --go ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
    go ("ls", what) = lines $ unsafePerformIO $ systemStdout' ["ls", what]
    go question     = error $ "The default oracle cannot answer the question " ++ show question


class OracleTrans t where
    liftQuestion :: Oracle o => Question o -> Question (t o)
    liftAnswer :: Oracle o => Answer (t o) -> Answer o
    liftQuery :: Oracle o => (Question (t o) -> Act (t o) (Answer (t o))) -> Question o -> Act (t o) (Answer o)
    liftQuery query = fmap liftAnswer . query . liftQuestion

instance OracleTrans (QAT qa) where
    liftQuestion q = QAQuestionLifted q
    liftAnswer (QAAnswerLifted a) = a


data QAT qa o = QAT { qat_data :: qa, dat_nested :: o }
type LsT = QAT LsQ
type CwdT = QAT CwdQ

class (Show (QAAnswer qa), Show (QAQuestion qa), Read (QAAnswer qa), Read (QAQuestion qa), Eq (QAAnswer qa), Eq (QAQuestion qa)) => QAC qa where
    type QAQuestion qa
    type QAAnswer qa
    answer :: qa -> QAQuestion qa -> QAAnswer qa

data LsQ = LsQ

instance QAC LsQ where
    type QAQuestion LsQ = FilePath
    type QAAnswer LsQ = [FilePath]
    answer LsQ what = lines $ unsafePerformIO $ systemStdout' ["ls", what]

data CwdQ = CwdQ

instance QAC CwdQ where
    type QAQuestion CwdQ = ()
    type QAAnswer CwdQ = FilePath
    answer CwdQ () = unsafePerformIO getCurrentDirectory

instance (QAC qa, Oracle o) => Oracle (QAT qa o) where
    data Question (QAT qa o) = QAQuestion (QAQuestion qa) | QAQuestionLifted (Question o)
    data Answer (QAT qa o) = QAAnswer (QAAnswer qa) | QAAnswerLifted (Answer o)
    askOracle (QAT _  o) (QAQuestionLifted q) = QAAnswerLifted $ askOracle o q
    askOracle (QAT qa _) (QAQuestion q)       = QAAnswer $ answer qa q


class Oracle o => OracleContained o o_top where
    liftQueryMany :: (Question o_top -> Act o_top (Answer o_top)) -> Question o -> Act o_top (Answer o)

instance (Oracle o, QAC qa) => OracleContained (QAT qa o) (QAT qa o) where
    liftQueryMany = id

instance OracleContained (QAT qa1 o1) o2 => OracleContained (QAT qa1 o1) (QAT qa2 o2) where
    liftQueryMany = liftQuery . liftQueryMany

  
ls :: (Oracle o, Oracle o', OracleContained (QAT LsQ o') o) => FilePath -> Act o [FilePath]
ls = fmap unLsAnswer . liftQueryMany query . QAQuestion
  where unLsAnswer (QAAnswer fps) = fps


deriving instance (Oracle o, QAC qa) => Eq (Question (QAT qa o))
deriving instance (Oracle o, QAC qa) => Show (Question (QAT qa o))
deriving instance (Oracle o, QAC qa) => Read (Question (QAT qa o))
deriving instance (Oracle o, QAC qa) => Eq (Answer (QAT qa o))
deriving instance (Oracle o, QAC qa) => Show (Answer (QAT qa o))
deriving instance (Oracle o, QAC qa) => Read (Answer (QAT qa o))


-- TODO: do files in parallel (Add "Building (MVar ())" constructor to the Database, and put Database into an MVar)
-- TODO: Neil's example from his presentation only works if want doesn't actually build anything until the end (he wants before setting up any rules)
want :: [FilePath] -> Shake o ()
want fps = do
    e <- askShakeEnv
    forM_ fps $ \fp -> do
      s <- getShakeState
      (_time, final_s) <- liftIO $ runAct (AE { ae_global_rules = ss_rules s, ae_global_oracle = se_oracle e }) (AS { as_this_history = [], as_database = ss_database s }) (runRule fp)
      putShakeState $ s { ss_database = as_database final_s }

(*>) :: String -> (FilePath -> Act o ()) -> Shake o ()
(*>) pattern action = (compiled `match`) ?> action
  where compiled = compile pattern

(*@>) :: (String, CreatesFiles) -> (FilePath -> Act o ()) -> Shake o ()
(*@>) (pattern, alsos) action = (\fp -> guard (compiled `match` fp) >> return alsos) ?@> action
  where compiled = compile pattern

(**>) :: (FilePath -> Maybe a) -> (FilePath -> a -> Act o ()) -> Shake o ()
(**>) p action = addRule $ \fp -> p fp >>= \x -> return ([fp], action fp x)

(**@>) :: (FilePath -> Maybe ([FilePath], a)) -> (FilePath -> a -> Act o ()) -> Shake o ()
(**@>) p action = addRule $ \fp -> p fp >>= \(creates, x) -> return (creates, action fp x)

(?>) :: (FilePath -> Bool) -> (FilePath -> Act o ()) -> Shake o ()
(?>) p action = addRule $ \fp -> guard (p fp) >> return ([fp], action fp)

(?@>) :: (FilePath -> Maybe CreatesFiles) -> (FilePath -> Act o ()) -> Shake o ()
(?@>) p action = addRule $ \fp -> p fp >>= \creates -> return (creates, action fp)


addRule :: Rule o -> Shake o ()
addRule rule = modifyShakeState $ \s -> s { ss_rules = rule : ss_rules s }

-- TODO: do subrules in parallel
need :: Oracle o => [FilePath] -> Act o ()
need fps = do
    init_db <- fmap as_database getActState
    let (uncleans, cleans) = partitionEithers $
          [case M.lookup fp init_db of Nothing           -> Left (fp, Nothing)
                                       Just (Dirty hist) -> Left (fp, Just hist)
                                       Just (Clean _ _)  -> Right fp
          | fp <- fps
          ]
    
    let history_requires_rerun (Oracle question old_answer) = do
            new_answer <- unsafeQuery question
            return (old_answer /= new_answer)
        history_requires_rerun (Need nested_fps) = flip anyM nested_fps $ \(fp, old_time) -> do
            new_time <- liftIO $ getModTime fp
            return (Just old_time /= new_time)
    
        get_clean_mod_time fp = fmap (expectJust ("The clean file " ++ fp ++ " was missing")) $ liftIO $ getModTime fp
    unclean_times <- forM uncleans $ \(unclean_fp, mb_hist) -> do
        mb_clean_hist <- case mb_hist of Nothing   -> return Nothing
                                         Just hist -> fmap (? (Nothing, Just hist)) $ anyM history_requires_rerun hist
        nested_time <- case mb_clean_hist of
          Nothing         -> runRule unclean_fp -- runRule will deal with marking the file clean
          Just clean_hist -> do
            -- We are actually Clean, though the history doesn't realise it yet..
            nested_time <- get_clean_mod_time unclean_fp
            markClean unclean_fp clean_hist nested_time
            return nested_time
        
        -- The file must now be Clean!
        return (unclean_fp, nested_time)
    
    clean_times <- forM cleans $ \clean_fp -> fmap ((,) clean_fp) (get_clean_mod_time clean_fp)
    
    appendHistory $ Need (unclean_times ++ clean_times)

markClean :: FilePath -> History o -> ModTime -> Act o ()
markClean fp nested_hist nested_time = modifyActState $ \s -> s { as_database = M.insert fp (Clean nested_hist nested_time) (as_database s) }

appendHistory :: QA o -> Act o ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

-- NB: when runRule returns, the input file will be clean (and probably some others, too..)
runRule :: FilePath -> Act o ModTime
runRule fp = do
    e <- askActEnv
    case [(creates_fps, action) | rule <- ae_global_rules e, Just (creates_fps, action) <- [rule fp]] of
        [(creates_fps, action)] -> do
            init_db <- fmap as_database getActState
            ((), final_nested_s) <- liftIO $ runAct e (AS { as_this_history = [], as_database = init_db }) action
            modifyActState $ \s -> s { as_database = as_database final_nested_s }
            
            creates_time <- forM creates_fps $ \creates_fp -> do
                nested_time <- fmap (expectJust $ "The matching rule did not create " ++ creates_fp) $ liftIO $ getModTime creates_fp
                markClean creates_fp (as_this_history final_nested_s) nested_time
                return (creates_fp, nested_time)
            return $ expectJust ("The rule didn't create the file that we originally asked for, " ++ fp) $ lookup fp creates_time
        [] -> do
            -- Not having a rule might still be OK, as long as there is some existing file here:
            mb_nested_time <- liftIO $ getModTime fp
            case mb_nested_time of
                Nothing          -> error $ "No rule to build " ++ fp
                Just nested_time -> do
                  markClean fp [] nested_time
                  return nested_time -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.
        _actions -> error $ "Ambiguous rules for " ++ fp -- TODO: disambiguate with a heuristic based on specificity of match/order in which rules were added?

class (Show (Question o), Show (Answer o), Read (Question o), Read (Answer o), Eq (Question o), Eq (Answer o)) => Oracle o where
    data Question o
    data Answer o
    askOracle :: o -> Question o -> Answer o
    --packQA :: Question o -> Answer o -> QA

-- TODO: allow the type of the oracle to change?
oracle :: Oracle o => o -> Shake o a -> Shake o a
oracle new_oracle = localShakeEnv (\e -> e { se_oracle = new_oracle }) -- TODO: some way to combine with previous oracle?

-- | Like 'query', but doesn't record that the question was asked
unsafeQuery :: Oracle o => Question o -> Act o (Answer o)
unsafeQuery question = do
    e <- askActEnv
    return $ askOracle (ae_global_oracle e) question

query :: Oracle o => Question o -> Act o (Answer o)
query question = do
    answer <- unsafeQuery question
    appendHistory $ Oracle question answer
    return answer
