{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, Rank2Types, DeriveDataTypeable, FlexibleInstances, FlexibleContexts #-}
module Development.Shake (
    -- * The top-level monadic interface
    Shake, shake,
    Rule, CreatesFiles, (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    want, oracle, modifyOracle,
    
    -- * Verbosity and command-line output from Shake
    Verbosity(..), actVerbosity, putStrLnAt,
    
    -- * The monadic interface used by rule bodies
    Act, need, query,
    
    -- * Oracles, the default oracle and wrappers for the questions it can answer
    Oracle(..), StringOracle(..), defaultOracle, stringOracle, queryStringOracle, ls
  ) where

import Development.Shake.WaitHandle
import Development.Shake.Utilities

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8

import Data.Typeable

import Control.Applicative (Applicative)
import Control.Arrow (second)

import Control.Concurrent.MVar
import Control.Concurrent.ParallelIO.Local

import Control.DeepSeq
import qualified Control.Exception as Exception

import Control.Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class

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


uncurry3 :: (a -> b -> c -> d)
         -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

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
type Rule o = FilePath -> Maybe (CreatesFiles, Act o ())

type SomeRule = FilePath -> Maybe (CreatesFiles, SomeRuleAct)
data SomeRuleAct = forall o. Oracle o => SomeRuleAct o (Act o ())

data ShakeState = SS {
    ss_rules :: [SomeRule]
  }

data ShakeEnv o = SE {
    se_database :: Database,
    se_pool :: Pool,
    se_oracle :: o,
    se_verbosity :: Verbosity
  }

instance Functor ShakeEnv where
    fmap f se = SE {
        se_database = se_database se,
        se_pool = se_pool se,
        se_oracle = f (se_oracle se),
        se_verbosity = se_verbosity se
      }

-- TODO: should Shake really be an IO monad?
newtype Shake o a = Shake { unShake :: Reader.ReaderT (ShakeEnv o) (State.StateT ShakeState IO) a }
                  deriving (Functor, Applicative, Monad, MonadIO)

runShake :: ShakeEnv o -> ShakeState -> Shake o a -> IO (a, ShakeState)
runShake e s mx = State.runStateT (Reader.runReaderT (unShake mx) e) s

getShakeState :: Shake o ShakeState
getShakeState = Shake (lift State.get)

-- putShakeState :: ShakeState -> Shake ()
-- putShakeState s = Shake (lift (State.put s))

modifyShakeState :: (ShakeState -> ShakeState) -> Shake o ()
modifyShakeState f = Shake (lift (State.modify f))

askShakeEnv :: Shake o (ShakeEnv o)
askShakeEnv = Shake Reader.ask

localShakeEnv :: (ShakeEnv o -> ShakeEnv o') -> Shake o' a -> Shake o a
localShakeEnv f mx = Shake (readerLocal f (unShake mx))

-- Reader.local has a restrictive type signature that prevents us from changing the environment type
readerLocal :: (e -> e') -> Reader.ReaderT e' m a -> Reader.ReaderT e m a
readerLocal f mx = Reader.ReaderT $ \e -> Reader.runReaderT mx (f e)


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
getPureDatabase = fmap M.fromList $ getList (liftM2 (,) getUTF8String (fmap Dirty getHistory))

putPureDatabase :: PureDatabase -> Put
putPureDatabase db = putList (\(k, v) -> putUTF8String k >> putHistory v) (M.toList $ M.mapMaybe prepareStatus db)

-- NB: we seralize Building as Dirty in case we ever want to serialize the database concurrently
-- with shake actually running. This might be useful to implement e.g. checkpointing...
--
-- NB: we serialize Clean as Dirty as well. This is because when we reload the database we cannot
-- assume that anything is clean, as one of the things it depends on may have been changed. We have to
-- verify all our assumptions again!
prepareStatus :: Status -> Maybe History
prepareStatus (Building mb_hist _) = mb_hist
prepareStatus (Dirty hist)         = Just hist
prepareStatus (Clean hist _ )      = Just hist

-- NB: use of the Clean constructor is just an optimisation that means we don't have to recursively recheck dependencies
-- whenever a file is need -- instead we can cut the checking process off if we discover than a file is marked as Clean.
-- Of course, this might go a bit wrong if the file becomes invalidated *during a Shake run*, but we accept that risk.
data Status = Dirty History
            | Clean History ModTime
            | Building (Maybe History) WaitHandle
            deriving (Show)

instance NFData Status where
    rnf (Dirty a) = rnf a
    rnf (Clean a b) = rnf a `seq` rnfModTime b
    rnf (Building a b) = rnf a `seq` b `seq` ()

type History = [QA]

getHistory :: Get History
getHistory = getList get

putHistory :: History -> Put
putHistory = putList put

data QA = Oracle String BS.ByteString BS.ByteString
        | Need [(FilePath, ModTime)]
        deriving (Show)

instance NFData QA where
    rnf (Oracle a b c) = rnf a `seq` rnf (BS.unpack b) `seq` rnf (BS.unpack c)
    rnf (Need xys) = rnf [rnf x `seq` rnfModTime y | (x, y) <- xys]

instance Binary QA where
    get = do
        tag <- getWord8
        case tag of
          0 -> liftM3 Oracle getUTF8String getSizedByteString getSizedByteString
          1 -> liftM Need (getList (liftM2 (,) getUTF8String getModTime))
          _ -> internalError $ "get{QA}: unknown tag " ++ show tag
    put (Oracle td bs_q bs_a) = putWord8 0 >> putUTF8String td >> putSizedByteString bs_q >> putSizedByteString bs_a
    put (Need xes)            = putWord8 1 >> putList (\(fp, mtime) -> putUTF8String fp >> putModTime mtime) xes

putOracle :: forall o. Oracle o
          => Question o -> Answer o
          -> (String, BS.ByteString, BS.ByteString)
putOracle q a = (show (typeOf (undefined :: o)), runPut $ put q, runPut $ put a)

-- TODO: catch any errors incurred when deserializing the question and answer
peekOracle :: forall o. Oracle o
           => String -> BS.ByteString -> BS.ByteString
           -> Maybe (Question o, Answer o)
peekOracle typerep bs_q bs_a = guard (show (typeOf (undefined :: o)) == typerep) >> return (runGet get bs_q, runGet get bs_a)

getSizedByteString :: Get BS.ByteString
getSizedByteString = do
    n <- getWord32le
    getLazyByteString (fromIntegral n)

putSizedByteString :: BS.ByteString -> Put
putSizedByteString bs = do
    putWord32le (fromIntegral (BS.length bs))
    putLazyByteString bs

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

data ActEnv o = AE {
    ae_oracle :: o, -- NB: Act *should not* (and cannot, thanks to the type system) access the Oracle from the ShakeEnv, because that is the "dynamically scoped" one
    ae_blocks_fps :: [FilePath],
    ae_global_env :: ShakeEnv (),
    ae_global_rules :: [SomeRule]
  }

instance Functor ActEnv where
    fmap f ae = AE {
        ae_oracle = f (ae_oracle ae),
        ae_blocks_fps = ae_blocks_fps ae,
        ae_global_env = ae_global_env ae,
        ae_global_rules = ae_global_rules ae
      }


newtype Act o a = Act { unAct :: Reader.ReaderT (ActEnv o) (State.StateT ActState IO) a }
              deriving (Functor, Applicative, Monad, MonadIO)

runAct :: ActEnv o -> ActState -> Act o a -> IO (a, ActState)
runAct e s mx = State.runStateT (Reader.runReaderT (unAct mx) e) s

-- getActState :: Act ActState
-- getActState = Act (lift State.get)

-- putActState :: ActState -> Act ()
-- putActState s = Act (lift (State.put s))

modifyActState :: (ActState -> ActState) -> Act o ()
modifyActState f = Act (lift (State.modify f))

askActEnv :: Act o (ActEnv o)
askActEnv = Act Reader.ask

actVerbosity :: Act o Verbosity
actVerbosity = fmap (se_verbosity . ae_global_env) askActEnv 

putStrLnAt :: Verbosity -> String -> Act o ()
putStrLnAt at_verbosity msg = do
    verbosity <- actVerbosity
    liftIO $ when (verbosity >= at_verbosity) $ putStrLn msg


-- NB: if you use shake in a nested way bad things will happen to parallelism
-- TODO: make parallelism configurable?
shake :: Shake StringOracle () -> IO ()
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
    
    when (verbosity >= ChattyVerbosity) $ putStr $ "Initial database:\n" ++ unlines [fp ++ ": " ++ show status | (fp, status) <- M.toList db]
    db_mvar <- newMVar db
    
    ((), _final_s) <- runShake (SE { se_database = db_mvar, se_pool = pool, se_oracle = defaultOracle, se_verbosity = verbosity }) (SS { ss_rules = [] }) mx
    
    final_db <- takeMVar db_mvar
    BS.writeFile ".openshake-db" (runPut $ putPureDatabase final_db)


class (Typeable o,
       Eq (Question o), Eq (Answer o),
       Binary (Question o), Binary (Answer o),
       Show (Question o), Show (Answer o)) => Oracle o where -- Show is only required for nice debugging output
    data Question o
    data Answer o
    queryOracle :: o -> Question o -> IO (Answer o)


-- The empty oracle is useful as a placeholder in a few places
instance Oracle () where
    data Question ()
    data Answer ()
    queryOracle = internalError "The empty oracle was queried"

instance Eq (Question ()) where
    (==) = internalError "The empty question was compared"

instance Eq (Answer ()) where
    (==) = internalError "The empty answer was compared"

instance Show (Question ()) where
    show = internalError "The empty question was shown"

instance Show (Answer ()) where
    show = internalError "The empty answer was shown"

instance Binary (Question ()) where
    get = internalError "The empty question was got"
    put = internalError "The empty question was put"

instance Binary (Answer ()) where
    get = internalError "The empty question was got"
    put = internalError "The empty question was put"


newtype StringOracle = SO ((String, String) -> IO [String])
                     deriving (Typeable)

instance Oracle StringOracle where
    data Question StringOracle = SQ { unSQ :: (String, String) }
                               deriving (Eq, Show)
    data Answer StringOracle = SA { unSA :: [String] }
                             deriving (Eq, Show)
    queryOracle (SO f) = fmap SA . f . unSQ

instance Binary (Question StringOracle) where
    get = fmap SQ $ liftM2 (,) getUTF8String getUTF8String
    put (SQ (x, y)) = putUTF8String x >> putUTF8String y

instance Binary (Answer StringOracle) where
    get = fmap SA $ getList getUTF8String
    put = putList putUTF8String . unSA


defaultOracle :: StringOracle
defaultOracle = SO go
  where
    -- Doesn't work because we want to do things like "ls *.c", and since the shell does globbing we need to go through it
    --go ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
    go ("ls", what) = fmap lines $ systemStdout' ("ls " ++ what)
    go question     = shakefileError $ "The default oracle cannot answer the question " ++ show question

queryStringOracle :: (String, String) -> Act StringOracle [String]
queryStringOracle = fmap unSA . query . SQ

stringOracle :: ((String, String) -> IO [String])
             -> Shake StringOracle a -> Shake o a
stringOracle = oracle . SO

ls :: FilePath -> Act StringOracle [FilePath]
ls fp = queryStringOracle ("ls", fp)


-- TODO: Neil's example from his presentation only works if want doesn't actually build anything until the end (he wants before setting up any rules)
want :: [FilePath] -> Shake o ()
want fps = do
    e <- askShakeEnv
    s <- getShakeState
    (_time, _final_s) <- liftIO $ runAct (AE { ae_blocks_fps = [], ae_global_rules = ss_rules s, ae_global_env = fmap (const ()) e, ae_oracle = () }) (AS { as_this_history = [] }) (need fps)
    return ()

(*>) :: Oracle o => String -> (FilePath -> Act o ()) -> Shake o ()
(*>) pattern action = (compiled `match`) ?> action
  where compiled = compile pattern

(*@>) :: Oracle o => (String, CreatesFiles) -> (FilePath -> Act o ()) -> Shake o ()
(*@>) (pattern, alsos) action = (\fp -> guard (compiled `match` fp) >> return alsos) ?@> action
  where compiled = compile pattern

(**>) :: Oracle o => (FilePath -> Maybe a) -> (FilePath -> a -> Act o ()) -> Shake o ()
(**>) p action = addRule $ \fp -> p fp >>= \x -> return ([fp], action fp x)

(**@>) :: Oracle o => (FilePath -> Maybe ([FilePath], a)) -> (FilePath -> a -> Act o ()) -> Shake o ()
(**@>) p action = addRule $ \fp -> p fp >>= \(creates, x) -> return (creates, action fp x)

(?>) :: Oracle o => (FilePath -> Bool) -> (FilePath -> Act o ()) -> Shake o ()
(?>) p action = addRule $ \fp -> guard (p fp) >> return ([fp], action fp)

(?@>) :: Oracle o => (FilePath -> Maybe CreatesFiles) -> (FilePath -> Act o ()) -> Shake o ()
(?@>) p action = addRule $ \fp -> p fp >>= \creates -> return (creates, action fp)


addRule :: Oracle o => Rule o -> Shake o ()
addRule rule = do
    -- NB: we store the oracle with the rule to implement "lexical scoping" for oracles.
    -- Basically, the oracle in effect when we run some rules action should be the oracle
    -- lexically above at the time the rule was created. Thus, we form the "lexical closure"
    -- of the oracle with the added rule.
    --
    -- Note the contrast with using the oracle from the point at which need was called to
    -- invoke the rule, which is more akin to a dynamic scoping scheme.
    o <- fmap se_oracle $ askShakeEnv
    modifyShakeState $ \s -> s { ss_rules = (fmap (second (SomeRuleAct o)) . rule) : ss_rules s }

need :: [FilePath] -> Act o ()
need fps = do
    e <- askActEnv
    -- NB: the initial list of blocked files is empty because whenever need is invoked (via runAct on a rule),
    -- the rule runs that will create the other files are already setup to run in parallel
    -- FIXME: an parallel Act execution blocks *its own file* from being created if it waits on a WaitHandle.
    -- Must take account of that!
    need_times <- liftIO $ need' e fps
    appendHistory $ Need need_times

withoutMVar :: MVar a -> a -> IO b -> IO (a, b)
withoutMVar mvar x act = putMVar mvar x >> act >>= \y -> takeMVar mvar >>= \x' -> return (x', y)

need' :: ActEnv o -> [FilePath] -> IO [(FilePath, ModTime)]
need' e init_fps = do
    let verbosity = se_verbosity (ae_global_env e)
        db_mvar = se_database (ae_global_env e)
        get_clean_mod_time fp = fmap (fromMaybe (internalError $ "The clean file " ++ fp ++ " was missing")) $ getFileModTime fp

    let -- We assume that the rules do not change to include new dependencies often: this lets
        -- us not rerun a rule as long as it looks like the dependencies of the *last known run*
        -- of the rule have not changed
        history_requires_rerun :: [FilePath] -> Oracle o => o -> QA -> IO (Maybe String)
        history_requires_rerun _ o (Oracle td bs_q bs_a) = 
            case peekOracle td bs_q bs_a of
                Nothing -> return $ Just "the type of the oracle associated with the rule has changed"
                Just (question, old_answer) -> do
                  new_answer <- queryOracle o question
                  return $ guard (old_answer /= new_answer) >> return ("oracle answer to " ++ show question ++ " has changed from " ++ show old_answer ++ " to " ++ show new_answer)
        history_requires_rerun blocks_fps _ (Need nested_fps_times) = do
            let (nested_fps, nested_old_times) = unzip nested_fps_times
            -- NB: if this Need is for a generated file we have to build it again if any of the things *it* needs have changed,
            -- so we recursively invoke need in order to check if we have any changes
            nested_new_times <- need' (e { ae_blocks_fps = blocks_fps ++ ae_blocks_fps e }) nested_fps
            let ([], relevant_nested_new_times) = lookupMany (\nested_fp -> internalError $ "The file " ++ nested_fp ++ " that we needed did not have a modification time in the output") nested_fps nested_new_times
            return $ firstJust $ (\f -> zipWith f relevant_nested_new_times nested_old_times) $
                \(fp, old_time) new_time -> guard (old_time /= new_time) >> return ("modification time of " ++ show fp ++ " has changed from " ++ show old_time ++ " to " ++ show new_time)
    
        find_all_rules pending_cleans pending_uncleans [] _ db = do
            -- Display a helpful message to the user explaining the rules that we have decided upon:
            let all_creates_fps = [creates_fp | (_, creates_fps, _) <- pending_uncleans, creates_fp <- creates_fps]
            when (not (null pending_uncleans) && verbosity >= ChattyVerbosity) $
                putStrLn $ "Using " ++ show (length pending_uncleans) ++ " rule instances to create the " ++
                           show (length all_creates_fps) ++ " files (" ++ showStringList all_creates_fps ++ ")"
            
            -- The rule-running code doesn't need to know *all* the files created by a rule run
            return (db, (pending_cleans, [(unclean_fps, rule) | (unclean_fps, _, rule) <- pending_uncleans]))
        find_all_rules pending_cleans pending_uncleans (fp:fps) blocks_fps db = do
            let ei_unclean_clean = case M.lookup fp db of
                  Nothing                     -> Left Nothing
                  Just (Dirty hist)           -> Left (Just hist)
                  Just (Clean _ _)            -> Right Nothing
                  Just (Building _ wait_mvar) -> Right (Just wait_mvar)
            
            case ei_unclean_clean of
                Right mb_mvar -> find_all_rules ((fp, mb_mvar) : pending_cleans) pending_uncleans fps blocks_fps db
                Left mb_hist -> do
                  -- 0) The artifact is *probably* going to be rebuilt, though we might still be able to skip a rebuild
                  -- if a check of its history reveals that we don't need to. Get the rule we would use to do the rebuild:
                  findRule (ae_global_rules e) fp $ \(potential_o, potential_creates_fps, potential_rule) -> do
                    -- 1) Basic sanity check that the rule creates the file we actually need
                    unless (fp `elem` potential_creates_fps) $ shakefileError $ "A rule matched " ++ fp ++ " but claims not to create it, only the files " ++ showStringList potential_creates_fps
    
                    -- 2) Make sure that none of the files that the proposed rule will create are not Dirty/unknown to the system.
                    --    This is because it would be unsafe to run a rule creating a file that might be in concurrent
                    --    use (read or write) by another builder process.
                    let non_dirty_fps = filter (\non_dirty_fp -> case M.lookup non_dirty_fp db of Nothing -> False; Just (Dirty _) -> False; _ -> True) potential_creates_fps
                    unless (null non_dirty_fps) $ shakefileError $ "A rule promised to yield the files " ++ showStringList potential_creates_fps ++ " in the process of building " ++ fp ++
                                                                   ", but the files " ++ showStringList non_dirty_fps ++ " have been independently built by someone else"
    
                    -- NB: we have to find the rule and mark the things it may create as Building *before* we determine whether the
                    -- file is actually dirty according to its history. This is because if the file *is* dirty according to that history
                    -- then we want to prevent any recursive invocations of need from trying to Build some file that we have added a
                    -- pending_unclean entry for already
                    --
                    -- NB: people wanting *any* of the files created by this rule should wait on the same WaitHandle
                    wait_handle <- newWaitHandle
                    db <- return $ foldr (\potential_creates_fp db -> M.insert potential_creates_fp (Building mb_hist wait_handle) db) db potential_creates_fps
                    blocks_fps <- return $ potential_creates_fps ++ blocks_fps
    
                    (db, ei_clean_hist_dirty_reason) <- case mb_hist of Nothing   -> return (db, Right "file was not in the database")
                                                                        Just hist -> withoutMVar db_mvar db $ fmap (maybe (Left hist) Right) $ firstJustM $ map (history_requires_rerun blocks_fps potential_o) hist
                    mb_clean_hist <- case ei_clean_hist_dirty_reason of
                      Left clean_hist -> return (Just clean_hist)
                      Right dirty_reason -> do
                        when (verbosity >= ChattyVerbosity) $ putStrLn $ "Rebuild " ++ fp ++ " because " ++ dirty_reason
                        return Nothing
                  
                    let (creates_fps, basic_rule) = case mb_clean_hist of
                          -- Each rule we execute will block the creation of some files if it waits. In particular, it blocks the
                          -- creation the files it *directly outputs*, and those files that will be created by the caller in the future
                          -- after our file is created and we return.
                          --
                          -- Note that a rule waiting *does not* block the creation of files built by other rules. This is because everything
                          -- gets executed in parallel.
                          Nothing         -> (potential_creates_fps, potential_rule (e { ae_blocks_fps = potential_creates_fps ++ ae_blocks_fps e }))
                          Just clean_hist -> ([fp], do
                            nested_time <- get_clean_mod_time fp
                            return (clean_hist, [(fp, nested_time)]))
                      
                        -- Augment the rule so that when it is run it sets all of the things it built to Clean again
                        rule = do
                            (nested_hist, mtimes) <- basic_rule
                            -- This is where we mark all of the files created by the rule as Clean:
                            markCleans (se_database (ae_global_env e)) nested_hist creates_fps mtimes
                            -- Wake up all of the waiters on the old Building entry (if any)
                            awakeWaiters wait_handle
                            return mtimes
    
                    -- 2) It is possible that we need two different files that are both created by the same rule. This is not an error!
                    --    What we should do is remove from the remaning uncleans any files that are created by the rule we just added
                    let (next_fps_satisifed_here, fps') = partition (`elem` creates_fps) fps
                    find_all_rules pending_cleans ((fp : next_fps_satisifed_here, creates_fps, rule) : pending_uncleans) fps' blocks_fps db
    
    -- Figure out the rules we need to use to create all the dirty files we need
    --
    -- NB: this MVar operation does not block us because any thread only holds the database lock
    -- for a very short amount of time (and can only do IO stuff while holding it, not Act stuff).
    -- When we have to recursively invoke need, we put back into the MVar before doing so.
    (cleans, uncleans) <- modifyMVar db_mvar $ find_all_rules [] [] init_fps []
    
    -- Run the rules we have decided upon in parallel
    unclean_times <- fmap concat $ parallel (se_pool (ae_global_env e)) $ flip map uncleans $ \(unclean_fps, rule) -> do
        mtimes <- rule
        -- We restrict the list of modification times returned to just those files that were actually needed by the user:
        -- we don't want to add a a dependency on those files that were incidentally created by the rule
        return $ snd $ lookupMany (\unclean_fp -> internalError $ "We should have reported the modification time for the rule file " ++ unclean_fp) unclean_fps mtimes
    
    -- TODO: could communicate ModTime of clean file via the WaitHandle... more elegant?
    clean_times <- forM cleans $ \(clean_fp, mb_wait_handle) -> do
        case mb_wait_handle of
          Nothing -> return ()
          Just wait_handle -> do
            -- NB: Waiting on the handle leads to a deadlock iff the worker responsible for triggering that wait handle
            -- eventually waits on one of the init_blocks_fps.
            --
            -- TODO: implement a less rubbish cycle detector based on this observation. The current one can fall down in situations
            -- where we have multiple threads working, because each thread will have only part of the dependency graph.
            -- One possible scheme:
            --  1) Add the init_blocks_fps to the WaitHandle somehow just before waiting on it
            --  2) Check if the wait handle has FilePaths attached that I am responsible for triggering in the future just before waiting on it
            when (clean_fp `elem` ae_blocks_fps e) $ shakefileError $ "Cyclic dependency detected: the file " ++ clean_fp ++ " depended on itself in a chain involving (some of) " ++ showStringList (ae_blocks_fps e)
            
            -- NB: We must spawn a new pool worker while we wait, or we might get deadlocked by depleting the pool of workers
            may_wait <- mayWaitOnWaitHandle wait_handle
            when may_wait $ extraWorkerWhileBlocked (se_pool (ae_global_env e)) (waitOnWaitHandle wait_handle)
        fmap ((,) clean_fp) (get_clean_mod_time clean_fp)
    
    return $ unclean_times ++ clean_times

markCleans :: Database -> History -> [FilePath] -> [(FilePath, ModTime)] -> IO ()
markCleans db_mvar nested_hist fps nested_times = modifyMVar_ db_mvar (return . go)
  where ([], relevant_nested_times) = lookupMany (\fp -> internalError $ "Rule did not return modification time for the file " ++ fp ++ " that it claimed to create") fps nested_times
    
        go init_db = foldr (\(fp, nested_time) db -> M.insert fp (Clean nested_hist nested_time) db) init_db relevant_nested_times


appendHistory :: QA -> Act o ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

-- NB: when the found rule returns, the input file will be clean (and probably some others, too..)
findRule :: [SomeRule] -> FilePath
         -> (forall o. Oracle o => (o, CreatesFiles, ActEnv o' -> (IO (History, [(FilePath, ModTime)]))) -> IO r)
         -> IO r
findRule rules fp k = case [(creates_fps, action) | rule <- rules, Just (creates_fps, action) <- [rule fp]] of
  [(creates_fps, SomeRuleAct o action)] -> k (o, creates_fps, \e -> do
      ((), final_nested_s) <- runAct (fmap (const o) e) (AS { as_this_history = [] }) action
      
      creates_times <- forM creates_fps $ \creates_fp -> do
          nested_time <- fmap (fromMaybe $ shakefileError $ "The matching rule did not create " ++ creates_fp) $ liftIO $ getFileModTime creates_fp
          return (creates_fp, nested_time)
      return (as_this_history final_nested_s, creates_times))
  [] -> do
      -- Not having a rule might still be OK, as long as there is some existing file here:
      mb_nested_time <- getFileModTime fp
      case mb_nested_time of
          Nothing          -> shakefileError $ "No rule to build " ++ fp
          -- NB: it is important that this fake oracle is not reachable if we change from having a rule for a file to not having one,
          -- but the file still exists. In that case we will try to recheck the old oracle answers against our new oracle and the type
          -- check will catch the change.
          Just nested_time -> k ((), [fp], \_ -> return ([], [(fp, nested_time)])) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.
  _actions -> shakefileError $ "Ambiguous rules for " ++ fp -- TODO: disambiguate with a heuristic based on specificity of match/order in which rules were added?

oracle :: o' -> Shake o' a -> Shake o a
oracle o' = modifyOracle (const o')

modifyOracle :: (o -> o') -> Shake o' a -> Shake o a
modifyOracle mk_o = localShakeEnv (\e -> e { se_oracle = mk_o (se_oracle e) })

query :: Oracle o => Question o -> Act o (Answer o)
query question = do
    e <- askActEnv
    answer <- liftIO $ queryOracle (ae_oracle e) question
    appendHistory $ uncurry3 Oracle $ putOracle question answer
    return answer
