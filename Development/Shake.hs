{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8

import Control.Applicative (Applicative)

import Control.Concurrent.MVar
import Control.Concurrent.ParallelIO.Local

import Control.DeepSeq
import qualified Control.Exception as Exception

import Control.Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class

import Data.Either
import Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import System.FilePath.Glob
import System.Time (CalendarTime, toCalendarTime)

import System.IO.Unsafe

import GHC.Conc (numCapabilities)


type CreatesFiles = [FilePath]
type Rule = FilePath -> Maybe (CreatesFiles, Act ())

data ShakeState = SS {
    ss_rules :: [Rule],
    ss_database :: Database
  }

data ShakeEnv = SE {
    se_pool :: Pool,
    se_oracle :: Oracle
  }

-- TODO: should Shake really be an IO monad?
newtype Shake a = Shake { unShake :: Reader.ReaderT ShakeEnv (State.StateT ShakeState IO) a }
                deriving (Functor, Applicative, Monad, MonadIO)

runShake :: ShakeEnv -> ShakeState -> Shake a -> IO (a, ShakeState)
runShake e s mx = State.runStateT (Reader.runReaderT (unShake mx) e) s

getShakeState :: Shake ShakeState
getShakeState = Shake (lift State.get)

-- putShakeState :: ShakeState -> Shake ()
-- putShakeState s = Shake (lift (State.put s))

modifyShakeState :: (ShakeState -> ShakeState) -> Shake ()
modifyShakeState f = Shake (lift (State.modify f))

askShakeEnv :: Shake ShakeEnv
askShakeEnv = Shake Reader.ask

localShakeEnv :: (ShakeEnv -> ShakeEnv) -> Shake a -> Shake a
localShakeEnv f mx = Shake (Reader.local f (unShake mx))


type ModTime = CalendarTime

-- TODO: more efficient normalisation
rnfModTime :: ModTime -> ()
rnfModTime mtime = rnf (show mtime)

-- TODO: more efficient serialisation
getModTime :: Get ModTime
getModTime = fmap read getUTF8String

putModTime :: ModTime -> Put
putModTime mtime = putUTF8String (show mtime)

getFileModTime :: FilePath -> IO (Maybe ModTime)
getFileModTime fp = handleDoesNotExist (return Nothing) (getModificationTime fp >>= (fmap Just . toCalendarTime))


type Database = MVar PureDatabase
type PureDatabase = Map FilePath Status

getPureDatabase :: Get PureDatabase
getPureDatabase = fmap M.fromList $ getList (liftM2 (,) getUTF8String get)

putPureDatabase :: PureDatabase -> Put
putPureDatabase db = putList (\(k, v) -> putUTF8String k >> put v) (M.toList $ M.mapMaybe sanitizeStatus db)

-- NB: we seralize Building as Dirty in case we ever want to serialize the database concurrently
-- with shake actually running. This might be useful to implement e.g. checkpointing...
sanitizeStatus :: Status -> Maybe Status
sanitizeStatus (Building mb_hist _) = fmap Dirty mb_hist
sanitizeStatus stat = Just stat

data Status = Dirty History
            | Clean History ModTime
            | Building (Maybe History) (MVar ())

instance NFData Status where
    rnf (Dirty a) = rnf a
    rnf (Clean a b) = rnf a `seq` rnfModTime b
    rnf (Building a b) = rnf a `seq` b `seq` ()

instance Binary Status where
    get = do
        tag <- getWord8
        case tag of
          0 -> liftM Dirty getHistory
          1 -> liftM2 Clean getHistory getModTime
          _ -> error $ "get{Status}: unknown tag " ++ show tag
    put (Dirty hist)       = putWord8 0 >> putHistory hist
    put (Clean hist mtime) = putWord8 1 >> putHistory hist >> putModTime mtime
    put (Building _ _) = error "Cannot serialize the Building status"

type History = [QA]

getHistory :: Get History
getHistory = getList get

putHistory :: History -> Put
putHistory = putList put

data QA = Oracle Question Answer
        | Need [(FilePath, ModTime)]

instance NFData QA where
    rnf (Oracle a b) = rnf a `seq` rnf b
    rnf (Need xys) = rnf [rnf x `seq` rnfModTime y | (x, y) <- xys]

instance Binary QA where
    get = do
        tag <- getWord8
        case tag of
          0 -> liftM2 Oracle getQuestion getAnswer
          1 -> liftM Need (getList (liftM2 (,) getUTF8String getModTime))
          _ -> error $ "get{QA}: unknown tag " ++ show tag
    put (Oracle q a) = putWord8 0 >> putQuestion q >> putAnswer a
    put (Need xes)   = putWord8 1 >> putList (\(fp, mtime) -> putUTF8String fp >> putModTime mtime) xes

getList :: Get a -> Get [a]
getList get_elt = do
    n <- getWord32le
    genericReplicateM n get_elt

putList :: (a -> Put) -> [a] -> Put
putList put_elt xs = do
    putWord32le (fromIntegral (length xs))
    mapM_ put_elt xs

getUTF8String :: Get String
getUTF8String = fmap UTF8.decode $ getList getWord8

putUTF8String :: String -> Put
putUTF8String = putList putWord8 . UTF8.encode


data ActState = AS {
    as_this_history :: History
  }

data ActEnv = AE {
    ae_global_database :: Database,
    ae_global_rules :: [Rule],
    ae_global_oracle :: Oracle,
    ae_global_pool :: Pool
  }

newtype Act a = Act { unAct :: Reader.ReaderT ActEnv (State.StateT ActState IO) a }
              deriving (Functor, Applicative, Monad, MonadIO)

runAct :: ActEnv -> ActState -> Act a -> IO (a, ActState)
runAct e s mx = State.runStateT (Reader.runReaderT (unAct mx) e) s

-- getActState :: Act ActState
-- getActState = Act (lift State.get)

-- putActState :: ActState -> Act ()
-- putActState s = Act (lift (State.put s))

modifyActState :: (ActState -> ActState) -> Act ()
modifyActState f = Act (lift (State.modify f))

askActEnv :: Act ActEnv
askActEnv = Act Reader.ask


-- NB: you can only use shake once per program run
shake :: Shake () -> IO ()
shake mx = withPool numCapabilities $ \pool -> do
    mb_bs <- handleDoesNotExist (return Nothing) $ fmap Just $ BS.readFile ".openshake-db"
    db <- case mb_bs of
        Nothing -> putStrLn "Database did not exist, doing full rebuild" >> return M.empty
        Just bs -> (Exception.evaluate (rnf db) >> return db) `Exception.catch` \(Exception.ErrorCall reason) -> do
                      putStrLn $ "Database unreadable (" ++ reason ++ "), doing full rebuild"
                      return M.empty
          where db = runGet getPureDatabase bs
    db_mvar <- newMVar db
    
    ((), _final_s) <- runShake (SE { se_pool = pool, se_oracle = defaultOracle }) (SS { ss_rules = [], ss_database = db_mvar }) mx
    
    final_db <- takeMVar db_mvar
    BS.writeFile ".openshake-db" (runPut $ putPureDatabase final_db)


defaultOracle :: Oracle
-- Doesn't work because we want to do things like "ls *.c", and since the shell does globbing we need to go through it
--default_oracle ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
defaultOracle ("ls", what) = lines $ unsafePerformIO $ systemStdout' ["ls", what]
defaultOracle question     = error $ "The default oracle cannot answer the question " ++ show question

ls :: FilePath -> Act [FilePath]
ls fp = query ("ls", fp)


-- TODO: Neil's example from his presentation only works if want doesn't actually build anything until the end (he wants before setting up any rules)
want :: [FilePath] -> Shake ()
want fps = do
    e <- askShakeEnv
    s <- getShakeState
    (_time, _final_s) <- liftIO $ runAct (AE { ae_global_rules = ss_rules s, ae_global_oracle = se_oracle e, ae_global_pool = se_pool e, ae_global_database = ss_database s }) (AS { as_this_history = [] }) (need fps)
    return ()

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

need :: [FilePath] -> Act ()
need fps = do
    (cleans, mvared_uncleans) <- modifyActDatabase $ \init_db -> do
        let (uncleans, cleans) = partitionEithers $
              [case M.lookup fp init_db of Nothing                     -> Left (fp, Nothing)
                                           Just (Dirty hist)           -> Left (fp, Just hist)
                                           Just (Clean _ _)            -> Right (fp, Nothing)
                                           Just (Building _ wait_mvar) -> Right (fp, Just wait_mvar) -- TODO: detect dependency cycles through Building
              | fp <- fps
              ]
        db <- (\f -> foldM f init_db uncleans) $ \db (unclean_fp, mb_hist) -> do
            wait_mvar <- newEmptyMVar
            return (M.insert unclean_fp (Building mb_hist wait_mvar) db)
        return (db, (cleans, uncleans))
    
    let history_requires_rerun (Oracle question old_answer) = do
            new_answer <- unsafeQuery question
            return (old_answer /= new_answer)
        history_requires_rerun (Need nested_fps) = flip anyM nested_fps $ \(fp, old_time) -> do
            new_time <- liftIO $ getFileModTime fp
            return (Just old_time /= new_time)
    
        get_clean_mod_time fp = fmap (expectJust ("The clean file " ++ fp ++ " was missing")) $ getFileModTime fp
    
    pool <- fmap ae_global_pool askActEnv
    
    unclean_times <- forM mvared_uncleans $ \(unclean_fp, mb_hist) -> do
        mb_clean_hist <- case mb_hist of Nothing   -> return Nothing
                                         Just hist -> fmap (? (Nothing, Just hist)) $ anyM history_requires_rerun hist
        nested_time <- case mb_clean_hist of
          Nothing         -> runRule unclean_fp -- runRule will deal with marking the file clean
          Just clean_hist -> do
            -- We are actually Clean, though the history doesn't realise it yet..
            nested_time <- liftIO $ get_clean_mod_time unclean_fp
            markClean unclean_fp clean_hist nested_time
            return nested_time
        
        -- The file must now be Clean
        return (unclean_fp, nested_time)
    
    clean_times <- liftIO $ forM cleans $ \(clean_fp, mb_wait_mvar) -> do
        case mb_wait_mvar of
          Nothing -> return ()
          Just mvar -> do
            -- NB: We must spawn a new pool worker while we wait, or we might get deadlocked
            -- NB: it is safe to use isEmptyMVar here because once the wait MVar is filled it will never be emptied
            empty <- isEmptyMVar mvar
            when empty $ extraWorkerWhileBlocked pool (takeMVar mvar)
        fmap ((,) clean_fp) (get_clean_mod_time clean_fp)
    
    appendHistory $ Need (unclean_times ++ clean_times)

markClean :: FilePath -> History -> ModTime -> Act ()
markClean fp nested_hist nested_time = modifyActDatabase_ $ \db -> do
    -- Ensure we notify any waiters that the file is now available:
    let (mb_removed, db') = M.insertLookupWithKey (\_ _ status' -> status') fp (Clean nested_hist nested_time) db
    case mb_removed of
        Just (Building _ wait_mvar) -> putMVar wait_mvar ()
        _                           -> return ()
    return db'


modifyActDatabase_ :: (PureDatabase -> IO PureDatabase) -> Act ()
modifyActDatabase_ f = modifyActDatabase (\db -> fmap (flip (,) ()) $ f db)

modifyActDatabase :: (PureDatabase -> IO (PureDatabase, a)) -> Act a
modifyActDatabase f = do
    db_mvar <- fmap ae_global_database askActEnv
    -- NB: this MVar operation does not block us because any thread only holds the database lock
    -- for a very short amount of time (and can only do IO stuff while holding it, not Act stuff)
    liftIO $ modifyMVar db_mvar f


appendHistory :: QA -> Act ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

-- NB: when runRule returns, the input file will be clean (and probably some others, too..)
runRule :: FilePath -> Act ModTime
runRule fp = do
    e <- askActEnv
    case [(creates_fps, action) | rule <- ae_global_rules e, Just (creates_fps, action) <- [rule fp]] of
        [(creates_fps, action)] -> do
            ((), final_nested_s) <- liftIO $ runAct e (AS { as_this_history = [] }) action
            
            creates_time <- forM creates_fps $ \creates_fp -> do
                nested_time <- fmap (expectJust $ "The matching rule did not create " ++ creates_fp) $ liftIO $ getFileModTime creates_fp
                markClean creates_fp (as_this_history final_nested_s) nested_time
                return (creates_fp, nested_time)
            return $ expectJust ("The rule didn't create the file that we originally asked for, " ++ fp) $ lookup fp creates_time
        [] -> do
            -- Not having a rule might still be OK, as long as there is some existing file here:
            mb_nested_time <- liftIO $ getFileModTime fp
            case mb_nested_time of
                Nothing          -> error $ "No rule to build " ++ fp
                Just nested_time -> do
                  markClean fp [] nested_time
                  return nested_time -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.
        _actions -> error $ "Ambiguous rules for " ++ fp -- TODO: disambiguate with a heuristic based on specificity of match/order in which rules were added?

type Question = (String,String)

getQuestion :: Get Question
getQuestion = liftM2 (,) getUTF8String getUTF8String

putQuestion :: Question -> Put
putQuestion (x, y) = putUTF8String x >> putUTF8String y

type Answer = [String]

getAnswer :: Get Answer
getAnswer = getList getUTF8String

putAnswer :: Answer -> Put
putAnswer = putList putUTF8String

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
