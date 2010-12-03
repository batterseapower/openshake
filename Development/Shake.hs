{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Concurrent.ParallelIO.Global

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


type CreatesFiles = [FilePath]
type Rule = FilePath -> Maybe (CreatesFiles, Act ())

data ShakeState = SS {
    ss_rules :: [Rule],
    ss_database :: Database
  }

data ShakeEnv = SE {
    se_oracle :: Oracle
  }

-- TODO: should Shake really be an IO monad?
newtype Shake a = Shake { unShake :: Reader.ReaderT ShakeEnv (State.StateT ShakeState IO) a }
                deriving (Functor, Applicative, Monad, MonadIO)

runShake :: ShakeEnv -> ShakeState -> Shake a -> IO (a, ShakeState)
runShake e s mx = State.runStateT (Reader.runReaderT (unShake mx) e) s

getShakeState :: Shake ShakeState
getShakeState = Shake (lift State.get)

putShakeState :: ShakeState -> Shake ()
putShakeState s = Shake (lift (State.put s))

modifyShakeState :: (ShakeState -> ShakeState) -> Shake ()
modifyShakeState f = Shake (lift (State.modify f))

askShakeEnv :: Shake ShakeEnv
askShakeEnv = Shake Reader.ask

localShakeEnv :: (ShakeEnv -> ShakeEnv) -> Shake a -> Shake a
localShakeEnv f mx = Shake (Reader.local f (unShake mx))


type ModTime = CalendarTime

getModTime :: FilePath -> IO (Maybe ModTime)
getModTime fp = handleDoesNotExist (getModificationTime fp >>= toCalendarTime)


type History = [QA]
data QA = Oracle Question Answer
        | Need [(FilePath, ModTime)]
        deriving (Show, Read)

type Database = Map FilePath Status
data Status = Dirty History
            | Clean History ModTime
            deriving (Show, Read)


data ActState = AS {
    as_this_history :: History,
    as_database :: Database
  }

data ActEnv = AE {
    ae_global_rules :: [Rule],
    ae_global_oracle :: Oracle
  }

newtype Act a = Act { unAct :: Reader.ReaderT ActEnv (State.StateT ActState IO) a }
              deriving (Functor, Applicative, Monad, MonadIO)

runAct :: ActEnv -> ActState -> Act a -> IO (a, ActState)
runAct e s mx = State.runStateT (Reader.runReaderT (unAct mx) e) s

getActState :: Act ActState
getActState = Act (lift State.get)

--putActState :: ActState -> Act ()
--putActState s = Act (lift (State.put s))

modifyActState :: (ActState -> ActState) -> Act ()
modifyActState f = Act (lift (State.modify f))

askActEnv :: Act ActEnv
askActEnv = Act Reader.ask


-- NB: you can only use shake once per program run
shake :: Shake () -> IO ()
shake mx = do
    mb_db <- handleDoesNotExist (fmap read $ readFile ".openshake-db")
    ((), final_s) <- runShake (SE { se_oracle = defaultOracle }) (SS { ss_rules = [], ss_database = fromMaybe M.empty mb_db }) mx
    writeFile ".openshake-db" (show $ ss_database final_s)
    stopGlobalPool


defaultOracle :: Oracle
-- Doesn't work because we want to do things like "ls *.c", and since the shell does globbing we need to go through it
--default_oracle ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
defaultOracle ("ls", what) = lines $ unsafePerformIO $ systemStdout' ["ls", what]
defaultOracle question     = error $ "The default oracle cannot answer the question " ++ show question

ls :: FilePath -> Act [FilePath]
ls fp = query ("ls", fp)


-- TODO: do files in parallel (Add "Building (MVar ())" constructor to the Database, and put Database into an MVar)
-- TODO: Neil's example from his presentation only works if want doesn't actually build anything until the end (he wants before setting up any rules)
want :: [FilePath] -> Shake ()
want fps = do
    e <- askShakeEnv
    forM_ fps $ \fp -> do
      s <- getShakeState
      (_time, final_s) <- liftIO $ runAct (AE { ae_global_rules = ss_rules s, ae_global_oracle = se_oracle e }) (AS { as_this_history = [], as_database = ss_database s }) (runRule fp)
      putShakeState $ s { ss_database = as_database final_s }

(*>) :: String -> (FilePath -> Act ()) -> Shake ()
(*>) pattern action = (compiled `match`) ?> action
  where compiled = compile pattern

(*@>) :: (String, CreatesFiles) -> (FilePath -> Act ()) -> Shake ()
(*@>) (pattern, alsos) action = (\fp -> guard (compiled `match` fp) >> return alsos) ?@> action
  where compiled = compile pattern

(**>) :: (FilePath -> Maybe a) -> (FilePath -> a -> Act ()) -> Shake ()
(**>) p action = addRule $ \fp -> p fp >>= \x -> return ([fp], action fp x)

(**@>) :: (FilePath -> Maybe ([FilePath], a)) -> (FilePath -> a -> Act ()) -> Shake ()
(**@>) p action = addRule $ \fp -> p fp >>= \(creates, x) -> return (creates, action fp x)

(?>) :: (FilePath -> Bool) -> (FilePath -> Act ()) -> Shake ()
(?>) p action = addRule $ \fp -> guard (p fp) >> return ([fp], action fp)

(?@>) :: (FilePath -> Maybe CreatesFiles) -> (FilePath -> Act ()) -> Shake ()
(?@>) p action = addRule $ \fp -> p fp >>= \creates -> return (creates, action fp)


addRule :: Rule -> Shake ()
addRule rule = modifyShakeState $ \s -> s { ss_rules = rule : ss_rules s }

-- TODO: do subrules in parallel
need :: [FilePath] -> Act ()
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

markClean :: FilePath -> History -> ModTime -> Act ()
markClean fp nested_hist nested_time = modifyActState $ \s -> s { as_database = M.insert fp (Clean nested_hist nested_time) (as_database s) }

appendHistory :: QA -> Act ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

-- NB: when runRule returns, the input file will be clean (and probably some others, too..)
runRule :: FilePath -> Act ModTime
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

type Question = (String,String)
type Answer = [String]
type Oracle = Question -> Answer

oracle :: Oracle -> Shake a -> Shake a
oracle new_oracle = localShakeEnv (\e -> e { se_oracle = new_oracle }) -- TODO: some way to combine with previous oracle?

-- | Like 'query', but doesn't record that the question was asked
unsafeQuery :: Question -> Act Answer
unsafeQuery question = do
    e <- askActEnv
    return $ ae_global_oracle e question

query :: Question -> Act Answer
query question = do
    answer <- unsafeQuery question
    appendHistory $ Oracle question answer
    return answer
