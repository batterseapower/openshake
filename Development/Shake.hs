{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Development.Shake (
    -- * The top-level monadic interface
    Shake, shake,
    Rule, CreatesFiles, (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    want, oracle,
    
    -- * Verbosity and command-line output from Shake
    Verbosity(..), actVerbosity, putStrLnAt,
    
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
-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List

import System.Directory
import System.Environment
import System.FilePath.Glob
import System.Time (ClockTime(..))

import System.IO.Unsafe

import GHC.Conc (numCapabilities)


-- | Verbosity level: higher is more verbose. Levels are as follows:
--
-- 0: Silent
-- 1: Quiet
-- 2: Normal
-- 3: Verbose
-- 4: Chatty
data Verbosity = SilentVerbosity | QuietVerbosity | NormalVerbosity | VerboseVerbosity | ChattyVerbosity
               deriving (Show, Enum, Bounded, Eq, Ord)


snocView :: [a] -> Maybe ([a], a)
snocView [] = Nothing
snocView ss = Just (init ss, last ss)

showStringList :: [String] -> String
showStringList ss = case snocView ss of
    Nothing       -> ""
    Just ([],  s) -> s
    Just (ss', s) -> intercalate ", " ss' ++ " and " ++ s

shakefileError :: String -> a
shakefileError s = error $ "Your Shakefile contained an error: " ++ s

internalError :: String -> a
internalError s = error $ "Internal Shake error: " ++ s


type CreatesFiles = [FilePath]
type Rule = FilePath -> Maybe (CreatesFiles, Act ())

data ShakeState = SS {
    ss_rules :: [Rule]
  }

data ShakeEnv = SE {
    se_database :: Database,
    se_pool :: Pool,
    se_oracle :: Oracle,
    se_verbosity :: Verbosity
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


type ModTime = ClockTime

rnfModTime :: ModTime -> ()
rnfModTime (TOD a b) = rnf a `seq` rnf b

getModTime :: Get ModTime
getModTime = liftM2 TOD get get

putModTime :: ModTime -> Put
putModTime (TOD a b) = put a >> put b

getFileModTime :: FilePath -> IO (Maybe ModTime)
getFileModTime fp = handleDoesNotExist (return Nothing) (fmap Just (getModificationTime fp))


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
          _ -> internalError $ "get{Status}: unknown tag " ++ show tag
    put (Dirty hist)       = putWord8 0 >> putHistory hist
    put (Clean hist mtime) = putWord8 1 >> putHistory hist >> putModTime mtime
    put (Building _ _) = internalError "Cannot serialize the Building status"

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
          _ -> internalError $ "get{QA}: unknown tag " ++ show tag
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
    ae_global_env :: ShakeEnv,
    ae_global_rules :: [Rule]
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

actVerbosity :: Act Verbosity
actVerbosity = fmap (se_verbosity . ae_global_env) askActEnv 

putStrLnAt :: Verbosity -> String -> Act ()
putStrLnAt at_verbosity msg = do
    verbosity <- actVerbosity
    liftIO $ when (verbosity >= at_verbosity) $ putStrLn msg


-- NB: you can only use shake once per program run
shake :: Shake () -> IO ()
shake mx = withPool numCapabilities $ \pool -> do
    -- TODO: when we have more command line options, use a proper command line argument parser.
    -- We should also work out whether shake should be doing argument parsing at all, given that it's
    -- meant to be used as a library function...
    verbosity <- fmap (\args -> fromMaybe NormalVerbosity $ listToMaybe $ reverse [ case rest of ""  -> VerboseVerbosity
                                                                                                 "v" -> ChattyVerbosity
                                                                                                 _   -> toEnum (fromEnum (minBound :: Verbosity) `max` read rest `min` fromEnum (maxBound :: Verbosity))
                                                                                  | '-':'v':rest <- args ]) getArgs
    
    mb_bs <- handleDoesNotExist (return Nothing) $ fmap Just $ BS.readFile ".openshake-db"
    db <- case mb_bs of
        Nothing -> do
            when (verbosity >= NormalVerbosity) $ putStrLn "Database did not exist, doing full rebuild"
            return M.empty
         -- NB: we force the input ByteString because we really want the database file to be closed promptly
        Just bs -> length (BS.unpack bs) `seq` (Exception.evaluate (rnf db) >> return db) `Exception.catch` \(Exception.ErrorCall reason) -> do
            when (verbosity >= NormalVerbosity) $ putStrLn $ "Database unreadable (" ++ reason ++ "), doing full rebuild"
            return M.empty
          where db = runGet getPureDatabase bs
    db_mvar <- newMVar db
    
    ((), _final_s) <- runShake (SE { se_database = db_mvar, se_pool = pool, se_oracle = defaultOracle, se_verbosity = verbosity }) (SS { ss_rules = [] }) mx
    
    final_db <- takeMVar db_mvar
    BS.writeFile ".openshake-db" (runPut $ putPureDatabase final_db)


defaultOracle :: Oracle
-- Doesn't work because we want to do things like "ls *.c", and since the shell does globbing we need to go through it
--default_oracle ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
defaultOracle ("ls", what) = lines $ unsafePerformIO $ systemStdout' ("ls " ++ what)
defaultOracle question     = shakefileError $ "The default oracle cannot answer the question " ++ show question

ls :: FilePath -> Act [FilePath]
ls fp = query ("ls", fp)


-- TODO: Neil's example from his presentation only works if want doesn't actually build anything until the end (he wants before setting up any rules)
want :: [FilePath] -> Shake ()
want fps = do
    e <- askShakeEnv
    s <- getShakeState
    (_time, _final_s) <- liftIO $ runAct (AE { ae_global_rules = ss_rules s, ae_global_env = e }) (AS { as_this_history = [] }) (need fps)
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
    e <- askActEnv
    verbosity <- actVerbosity
    
    let get_clean_mod_time fp = fmap (expectJust ("The clean file " ++ fp ++ " was missing")) $ getFileModTime fp

    -- NB: this MVar operation does not block us because any thread only holds the database lock
    -- for a very short amount of time (and can only do IO stuff while holding it, not Act stuff)
    (cleans, uncleans_cleaned_rules) <- liftIO $ modifyMVar (se_database (ae_global_env e)) $ \init_db -> do
        let (init_uncleans, cleans) = partitionEithers $
              [case M.lookup fp init_db of Nothing                     -> Left (fp, Nothing)
                                           Just (Dirty hist)           -> Left (fp, Just hist)
                                           Just (Clean _ _)            -> Right (fp, Nothing)
                                           Just (Building _ wait_mvar) -> Right (fp, Just wait_mvar) -- TODO: detect dependency cycles through Building
              | fp <- fps
              ]
        
            history_requires_rerun :: QA -> IO (Maybe String)
            history_requires_rerun (Oracle question old_answer) = do
                let new_answer = se_oracle (ae_global_env e) question
                return $ guard (old_answer /= new_answer) >> return ("oracle answer to " ++ show question ++ " has changed from " ++ show old_answer ++ " to " ++ show new_answer)
            history_requires_rerun (Need nested_fps) = flip firstJustM nested_fps $ \(fp, old_time) -> do
                new_time <- getFileModTime fp
                return $ guard (Just old_time /= new_time) >> return ("modification time of " ++ show fp ++ " has changed from " ++ show old_time ++ " to " ++ show new_time)

            find_all_rules [] = return []
            find_all_rules ((unclean_fp, mb_hist):uncleans) = do
                mb_clean_hist <- do
                    ei_clean_hist_dirty_reason <- case mb_hist of Nothing   -> return (Right "file was not in the database")
                                                                  Just hist -> fmap (maybe (Left hist) Right) $ firstJustM history_requires_rerun hist
                    case ei_clean_hist_dirty_reason of
                      Left clean_hist -> return (Just clean_hist)
                      Right dirty_reason -> do
                        when (verbosity >= ChattyVerbosity) $ putStrLn $ "Rebuild " ++ unclean_fp ++ " because " ++ dirty_reason
                        return Nothing
                (creates_fps, rule) <- case mb_clean_hist of
                  Nothing         -> findRule (ae_global_rules e) unclean_fp
                  Just clean_hist -> return ([unclean_fp], \_ -> do
                    nested_time <-  get_clean_mod_time unclean_fp
                    return (clean_hist, [(unclean_fp, nested_time)]))
                
                -- 0) Basic sanity check that the rule creates the file we actually need
                unless (unclean_fp `elem` creates_fps) $ shakefileError $ "A rule matched " ++ unclean_fp ++ " but claims not to create it, only the files " ++ showStringList creates_fps
                
                -- 1) Make sure that none of the files that the proposed rule will create are not Dirty/unknown to the system.
                --    This is because it would be unsafe to run a rule creating a file that might be in concurrent
                --    use (read or write) by another builder process.
                let non_dirty_fps = filter (\fp -> case M.lookup fp init_db of Nothing -> False; Just (Dirty _) -> False; _ -> True) creates_fps
                unless (null non_dirty_fps) $ shakefileError $ "A rule promised to yield the files " ++ showStringList creates_fps ++ " in the process of building " ++ unclean_fp ++
                                                               ", but the files " ++ showStringList non_dirty_fps ++ " have been independently built by someone else"
                
                -- 2) It is possible that we need two different files that are both created by the same rule. This is not an error!
                --    What we should do is remove from the remaning uncleans any files that are created by the rule we just added
                let (uncleans_satisifed_here, uncleans') = partition (\(next_unclean_fp, _) -> next_unclean_fp `elem` creates_fps) uncleans
                fmap ((unclean_fp : map fst uncleans_satisifed_here, mb_hist, creates_fps, rule) :) $ find_all_rules uncleans'
        
        -- Figure out the rules we need to use to create all the dirty files we need
        uncleans_rules <- find_all_rules init_uncleans
        
        -- Build the updated database that reflects the files that are going to be Building, and augment
        -- the rule so that when it is run it sets all of the things it built to Clean again
        (db, uncleans_cleaned_rules) <- (\f -> mapAccumLM f init_db uncleans_rules) $ \db (unclean_fps, mb_hist, creates_fps, rule) -> do
            -- People wanting any of the files created by this rule should wait on the same MVar
            wait_mvar <- newEmptyMVar
            return (foldr (\creates_fp -> M.insert creates_fp (Building mb_hist wait_mvar)) db creates_fps,
                   (unclean_fps, do { (nested_hist, mtimes) <- rule e;
                                      -- This is where we mark all of the files created by the rule as Clean:
                                      markCleans (se_database (ae_global_env e)) nested_hist creates_fps mtimes;
                                      -- Wake up all of the waiters on the old Building entry (if any)
                                      putMVar wait_mvar ();
                                      return mtimes }))
        
        return (db, (cleans, uncleans_cleaned_rules))
    
    -- Run the rules we have decided upon in parallel
    unclean_times <- fmap concat $ liftIO $ parallel (se_pool (ae_global_env e)) $ flip map uncleans_cleaned_rules $ \(unclean_fps, rule) -> do
        mtimes <- rule
        -- We restrict the list of modification times returned to just those files that were actually needed by the user:
        -- we don't want to add a a dependency on those files that were incidentally created by the rule
        return $ snd $ lookupMany (\unclean_fp -> internalError $ "We should have reported the modification time for the rule file " ++ unclean_fp) unclean_fps mtimes
    
    clean_times <- liftIO $ forM cleans $ \(clean_fp, mb_wait_mvar) -> do
        case mb_wait_mvar of
          Nothing -> return ()
          Just mvar -> do
            -- NB: We must spawn a new pool worker while we wait, or we might get deadlocked
            -- NB: it is safe to use isEmptyMVar here because once the wait MVar is filled it will never be emptied
            empty <- isEmptyMVar mvar
            when empty $ extraWorkerWhileBlocked (se_pool (ae_global_env e)) (readMVar mvar)
        fmap ((,) clean_fp) (get_clean_mod_time clean_fp)
    
    appendHistory $ Need (unclean_times ++ clean_times)

markCleans :: Database -> History -> [FilePath] -> [(FilePath, ModTime)] -> IO ()
markCleans db_mvar nested_hist fps nested_times = modifyMVar_ db_mvar (return . go)
  where (residual_nested_times, relevant_nested_times) = lookupMany (\fp -> internalError $ "Rule did not return modification time for the file " ++ fp ++ " that it claimed to create") fps nested_times
    
        go init_db | null residual_nested_times = foldr (\(fp, nested_time) db -> M.insert fp (Clean nested_hist nested_time) db) init_db relevant_nested_times
                   | otherwise                  = internalError $ "Rule returned modification times for the files " ++ showStringList (map fst residual_nested_times) ++ " that it never claimed to create"


appendHistory :: QA -> Act ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

-- NB: when the found rule returns, the input file will be clean (and probably some others, too..)
findRule :: [Rule] -> FilePath -> IO (CreatesFiles, ActEnv -> IO (History, [(FilePath, ModTime)]))
findRule rules fp = case [(creates_fps, action) | rule <- rules, Just (creates_fps, action) <- [rule fp]] of
  [(creates_fps, action)] -> return (creates_fps, \e -> do
      ((), final_nested_s) <- runAct e (AS { as_this_history = [] }) action
      
      creates_times <- forM creates_fps $ \creates_fp -> do
          nested_time <- fmap (fromMaybe $ shakefileError $ "The matching rule did not create " ++ creates_fp) $ liftIO $ getFileModTime creates_fp
          return (creates_fp, nested_time)
      return (as_this_history final_nested_s, creates_times))
  [] -> do
      -- Not having a rule might still be OK, as long as there is some existing file here:
      mb_nested_time <- getFileModTime fp
      case mb_nested_time of
          Nothing          -> shakefileError $ "No rule to build " ++ fp
          Just nested_time -> return ([fp], \_ -> return ([], [(fp, nested_time)])) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.
  _actions -> shakefileError $ "Ambiguous rules for " ++ fp -- TODO: disambiguate with a heuristic based on specificity of match/order in which rules were added?

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

-- TODO: lexically scoped semantics for the oracle
-- TODO: oracle polymorphism
-- TODO: supply old oracle to the new oracle function
oracle :: Oracle -> Shake a -> Shake a
oracle new_oracle = localShakeEnv (\e -> e { se_oracle = new_oracle }) -- TODO: some way to combine with previous oracle?

query :: Question -> Act Answer
query question = do
    e <- askActEnv
    let answer = se_oracle (ae_global_env e) question
    appendHistory $ Oracle question answer
    return answer
