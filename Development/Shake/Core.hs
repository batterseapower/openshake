{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, Rank2Types, DeriveDataTypeable, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving #-}
module Development.Shake.Core (
    -- * The top-level monadic interface
    Shake, shake,
    addRule, act,

    -- * Rules
    SomeRule, Generator,
    
    -- * Verbosity and command-line output from Shake
    Verbosity(..), actVerbosity, putStrLnAt,
    
    -- * The monadic interface used by rule bodies
    Act, need,
    
    -- * Namespaces
    Namespace(..),
    
    -- * Specialised errors
    shakefileError, internalError,
    
    -- * Used to add commands to the shake report
    reportCommand
  ) where

import Development.Shake.Core.Binary
import Development.Shake.Core.WaitHandle
import Development.Shake.Core.Utilities

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS

import Control.Applicative (Applicative)
import Control.Arrow (first, second)

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
import Data.Ord
import Data.List
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

import System.Environment

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


shakefileError :: String -> a
shakefileError s = error $ "Your Shakefile contained an error: " ++ s

internalError :: String -> a
internalError s = error $ "Internal Shake error: " ++ s


runGetAll :: Get a -> BS.ByteString -> a
runGetAll act bs = case runGetState act bs 0 of (x, bs', _) -> if BS.length bs' == 0 then x else error $ show (BS.length bs') ++ " unconsumed bytes after reading"


class (Ord n, Eq (Entry n), Show n, Show (Entry n), Binary n, Binary (Entry n), NFData n, NFData (Entry n)) => Namespace n where
    type Entry n
    
    -- | Tests whether the cached value for some Dirty entry still appears to be correct. If it is certainly incorrect,
    -- returns a human-readable reason as to why it should be discarded.
    --
    -- The default implementation of this function does no sanity checking.
    sanityCheck :: n -> Entry n -> IO (Maybe String)
    sanityCheck _ _ = return Nothing
    
    -- | The rule which we fall back on if there are no other options.
    --
    -- In order to get the same behaviour as Shake, we allow the default rule to depend on some IO computation (in particular,
    -- we need to know whether a file already exists in order to decide if can use the default rule for it).
    -- TODO: I could just do the IO in the Act monad and delay the error a little bit.
    --
    -- The default implementation is not to have a default rule.
    defaultRule :: n -> IO (Maybe (Generator n))
    defaultRule _ = return Nothing


type SomeRule n = n -> IO (Maybe (Generator n))

type Generator n = ([n], Act n [Entry n])

data ShakeState n = SS {
    ss_rules :: [SomeRule n],
    ss_acts :: [Act n ()]
  }

newtype Shake n a = Shake { unShake :: State.State (ShakeState n) a }
                  deriving (Functor, Applicative, Monad)

runShake :: ShakeState n -> Shake n a -> (a, ShakeState n)
runShake s mx = State.runState (unShake mx) s

-- getShakeState :: Shake n (ShakeState n)
-- getShakeState = Shake (lift State.get)

-- putShakeState :: ShakeState -> Shake ()
-- putShakeState s = Shake (lift (State.put s))

modifyShakeState :: (ShakeState n -> ShakeState n) -> Shake n ()
modifyShakeState f = Shake (State.modify f)


type Database n = MVar (PureDatabase n)
type PureDatabase n = Map n (Status n)

getPureDatabase :: Namespace n => Get (PureDatabase n)
getPureDatabase = fmap M.fromList $ getList (liftM2 (,) get (liftM2 Dirty getHistory get))

putPureDatabase :: Namespace n => PureDatabase n -> Put
putPureDatabase db = putList (\(fp, (hist, cached)) -> put fp >> putHistory hist >> put cached) (M.toList $ M.mapMaybe prepareStatus db)


-- NB: we seralize Building as Dirty in case we ever want to serialize the database concurrently
-- with shake actually running. This might be useful to implement e.g. checkpointing...
--
-- NB: we serialize Clean as Dirty as well. This is because when we reload the database we cannot
-- assume that anything is clean, as one of the things it depends on may have been changed. We have to
-- verify all our assumptions again!
prepareStatus :: Status n -> Maybe (History n, Entry n)
prepareStatus (Building mb_hist _) = mb_hist
prepareStatus (Dirty hist mtime)   = Just (hist, mtime)
prepareStatus (Clean hist mtime)   = Just (hist, mtime)

type BuildingWaitHandle n = WaitHandle (Entry n) -- TODO: record list of files created that we are actually waiting on, for better deadlock errors

-- NB: use of the Clean constructor is just an optimisation that means we don't have to recursively recheck dependencies
-- whenever a file is need -- instead we can cut the checking process off if we discover than a file is marked as Clean.
-- Of course, this might go a bit wrong if the file becomes invalidated *during a Shake run*, but we accept that risk.
data Status n = Dirty (History n) (Entry n) -- NB: the Dirty entry is only valid if the History has not been invalidated! (Key difference from standard Shake: we cache mtime for Dirty files as well...)
              | Clean (History n) (Entry n)
              | Building (Maybe (History n, Entry n)) (BuildingWaitHandle n)

deriving instance (Namespace n) => Show (Status n)

instance Namespace n => NFData (Status n) where
    rnf (Dirty a b) = rnf a `seq` rnf b
    rnf (Clean a b) = rnf a `seq` rnf b
    rnf (Building a b) = rnf a `seq` b `seq` ()

type History n = [QA n]

getHistory :: Namespace n => Get (History n)
getHistory = getList get

putHistory :: Namespace n => History n -> Put
putHistory = putList put

data QA n = Need [(n, Entry n)]

deriving instance Namespace n => Show (QA n)

instance Namespace n => NFData (QA n) where
    rnf (Need xys) = rnf [rnf x `seq` rnf y | (x, y) <- xys]

instance Namespace n => Binary (QA n) where
    get = liftM Need (getList (liftM2 (,) get get))
    put (Need xes) = putList (\(fp, mtime) -> put fp >> put mtime) xes

data ActState n = AS {
    as_this_history :: History n
  }

data ActEnv n = AE {
    ae_would_block_handles :: [WaitHandle ()], -- ^ A list of handles that would be incapable of awakening if the action were to
                                               --   block indefinitely here and now. This is used in the deadlock detector.
    ae_global_rules :: [SomeRule n],
    ae_database :: Database n,
    ae_wait_database :: MVar (WaitDatabase n),
    ae_report :: MVar ReportDatabase,
    ae_pool :: Pool,
    ae_verbosity :: Verbosity
  }


newtype Act n a = Act { unAct :: Reader.ReaderT (ActEnv n) (State.StateT (ActState n) IO) a }
              deriving (Functor, Applicative, Monad, MonadIO)

runAct :: ActEnv n -> ActState n -> Act n a -> IO (a, ActState n)
runAct e s mx = State.runStateT (Reader.runReaderT (unAct mx) e) s

-- getActState :: Act ActState
-- getActState = Act (lift State.get)

-- putActState :: ActState -> Act ()
-- putActState s = Act (lift (State.put s))

modifyActState :: (ActState n -> ActState n) -> Act n ()
modifyActState f = Act (lift (State.modify f))

askActEnv :: Act n (ActEnv n)
askActEnv = Act Reader.ask

actVerbosity :: Act n Verbosity
actVerbosity = fmap ae_verbosity askActEnv

putStrLnAt :: Verbosity -> String -> Act n ()
putStrLnAt at_verbosity msg = do
    verbosity <- actVerbosity
    liftIO $ when (verbosity >= at_verbosity) $ putStrLn msg


-- NB: if you use shake in a nested way bad things will happen to parallelism
-- TODO: make parallelism configurable?
shake :: Namespace n => Shake n () -> IO ()
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
          where db = runGetAll getPureDatabase bs
    
    when (verbosity >= ChattyVerbosity) $ putStr $ "Initial database:\n" ++ unlines [show fp ++ ": " ++ show status | (fp, status) <- M.toList db]
    db_mvar <- newMVar db
    
    wdb_mvar <- newMVar emptyWaitDatabase
    report_mvar <- emptyReportDatabase >>= newMVar

    -- Collect rules and wants, then execute the collected Act actions (in any order)
    let ((), final_s) = runShake (SS { ss_rules = [], ss_acts = [] }) mx
    parallel_ pool $ flip map (ss_acts final_s) $ \act -> do
        ((), _final_s) <- runAct (AE { ae_would_block_handles = [], ae_global_rules = ss_rules final_s, ae_database = db_mvar, ae_wait_database = wdb_mvar, ae_report = report_mvar, ae_pool = pool, ae_verbosity = verbosity }) (AS { as_this_history = [] }) act
        return ()
    
    -- TODO: put report under command-line control
    final_report <- takeMVar report_mvar
    writeFile "openshake-report.html" (produceReport final_report)
    
    final_db <- takeMVar db_mvar
    BS.writeFile ".openshake-db" (runPut $ putPureDatabase final_db)


-- | Perform the specified action once we are done collecting rules in the 'Shake' monad.
-- Just like 'want', there is no guarantee about the order in which the actions will be will be performed.
act :: Act n () -> Shake n ()
act what = modifyShakeState (\s -> s { ss_acts = what : ss_acts s })


addRule :: SomeRule n -> Shake n ()
addRule rule = modifyShakeState $ \s -> s { ss_rules = rule : ss_rules s }

need :: Namespace n => [n] -> Act n [Entry n]
need fps = do
    e <- askActEnv
    need_times <- liftIO $ need' e fps
    appendHistory $ Need (fps `zip` need_times)
    return need_times

withoutMVar :: MVar a -> a -> IO b -> IO (a, b)
withoutMVar mvar x act = putMVar mvar x >> act >>= \y -> takeMVar mvar >>= \x' -> return (x', y)

-- We assume that the rules do not change to include new dependencies often: this lets
-- us not rerun a rule as long as it looks like the dependencies of the *last known run*
-- of the rule have not changed
doesQARequireRerun :: Namespace n => ([n] -> IO [Entry n]) -> QA n -> IO (Maybe String)
doesQARequireRerun need (Need nested_fps_times) = do
    let (nested_fps, nested_old_times) = unzip nested_fps_times
    -- NB: if this Need is for a generated file we have to build it again if any of the things *it* needs have changed,
    -- so we recursively invoke need in order to check if we have any changes
    nested_new_times <- need nested_fps
    return $ firstJust $ (\f -> zipWith3 f nested_fps nested_new_times nested_old_times) $
        \fp old_time new_time -> guard (old_time /= new_time) >> return ("modification time of " ++ show fp ++ " has changed from " ++ show old_time ++ " to " ++ show new_time)

findAllRules :: Namespace n
             => ActEnv n
             -> [n]             -- ^ The files that we wish to find rules for
             -> [WaitHandle ()] -- ^ Handles that would be blocked if we blocked the thread right now
             -> PureDatabase n
             -> IO (PureDatabase n,
                    ([(n, IO (Entry n))],    -- Action that just waits for a build in progress elsewhere to complete
                     [([n], IO [Entry n])])) -- Action that creates (possibly several) of the files we asked for by invoking a user rule
findAllRules _ []       _                   db = return (db, ([], []))
findAllRules e (fp:fps) would_block_handles db = do
    let ei_unclean_clean = case M.lookup fp db of
           -- If the file is totally unknown to the database we're certainly going to have to build it
          Nothing                     -> Left Nothing
           -- Likewise if the file is known but we are the first to notice that the file is dirty, though in this case "building" it might just mean marking it as clean
          Just (Dirty hist mtime)     -> Left (Just (hist, mtime))
           -- We've previously discovered the file to be clean: return an action that just returns the computed entry directly
          Just (Clean _ mtime)        -> Right $ return mtime
           -- Someone else is in the process of making the file clean. Return an action that wait on the wait handle for it to complete
          Just (Building _ wait_mvar) -> Right $ do
            -- We can avoid a lot of fuss if the wait handle is already triggered, so there can be no waiting.
            -- This is purely a performance optimisation:
            may_wait <- mayWaitOnWaitHandle wait_mvar
            let wrapper | may_wait  = reportWorkerBlocked (ae_report e) .
                                      registerWait (ae_wait_database e) fp (fmap (const ()) wait_mvar) (ae_would_block_handles e) .
                                      extraWorkerWhileBlocked (ae_pool e) -- NB: We must spawn a new pool worker while we wait, or we might get deadlocked by depleting the pool of workers
                        | otherwise = id
            -- NB: we communicate the ModTimes of files that we were waiting on the completion of via the BuildingWaitHandle
            wrapper (waitOnWaitHandle wait_mvar)
    case ei_unclean_clean of
        Right clean_act -> fmap (second (first ((fp, clean_act) :))) $ findAllRules e fps would_block_handles db
        Left mb_hist -> do
          -- 0) The artifact is *probably* going to be rebuilt, though we might still be able to skip a rebuild
          -- if a check of its history reveals that we don't need to. Get the rule we would use to do the rebuild:
          findRule verbosity (ae_global_rules e) fp $ \(potential_creates_fps, potential_rule) -> do
            -- 1) Basic sanity check that the rule creates the file we actually need
            unless (fp `elem` potential_creates_fps) $ shakefileError $ "A rule matched " ++ show fp ++ " but claims not to create it, only the files " ++ showStringList (map show potential_creates_fps)

            -- 2) Make sure that none of the files that the proposed rule will create are not Dirty/unknown to the system.
            --    This is because it would be unsafe to run a rule creating a file that might be in concurrent
            --    use (read or write) by another builder process.
            let non_dirty_fps = filter (\non_dirty_fp -> case M.lookup non_dirty_fp db of Nothing -> False; Just (Dirty _ _) -> False; _ -> True) potential_creates_fps
            unless (null non_dirty_fps) $ shakefileError $ "A rule promised to yield the files " ++ showStringList (map show potential_creates_fps) ++ " in the process of building " ++ show fp ++
                                                           ", but the files " ++ showStringList (map show non_dirty_fps) ++ " have been independently built by someone else"

            -- NB: we have to find the rule and mark the things it may create as Building *before* we determine whether the
            -- file is actually dirty according to its history. This is because if the file *is* dirty according to that history
            -- then we want to prevent any recursive invocations of need from trying to Build some file that we have added a
            -- pending_unclean entry for already
            --
            -- NB: people wanting *any* of the files created by this rule should wait on the same BuildingWaitHandle.
            -- However, we fmap each instance of it so that it only reports the Entry information for exactly the file you care about.
            (wait_handle, awake_waiters) <- newWaitHandle
            db <- return $ foldr (\(potential_creates_fp, extractor) db -> M.insert potential_creates_fp (Building mb_hist (fmap extractor wait_handle)) db) db (potential_creates_fps `zip` listExtractors)
            
            -- If we block in recursive invocations of need' (if any), we will block the wait handle we just created from ever being triggered:
            would_block_handles <- return $ fmap (const ()) wait_handle : would_block_handles

            (db, ei_clean_hist_dirty_reason) <- case mb_hist of Nothing            -> return (db, Right "file was not in the database")
                                                                Just (hist, mtime) -> withoutMVar (ae_database e) db $ do
                                                                  mb_dirty_reason <- firstJustM $ map (doesQARequireRerun (need' (e { ae_would_block_handles = would_block_handles ++ ae_would_block_handles e }))) hist
                                                                  case mb_dirty_reason of
                                                                    Just dirty_reason -> return $ Right dirty_reason
                                                                    Nothing -> do
                                                                      -- The file wasn't dirty, but it might be "insane". For files, this occurs when the file
                                                                      -- has changed since we last looked at it even though its dependent files haven't changed.
                                                                      -- This usually indicates some sort of bad thing has happened (e.g. editing a generated file) --
                                                                      -- we just rebuild it directly, though we could make another choice:
                                                                      mb_insane_reason <- sanityCheck fp mtime
                                                                      return $ maybe (Left (hist, mtime)) Right mb_insane_reason
            
            -- Each rule we execute will block the creation of some files if it waits:
            --   * It blocks the creation the files it *directly outputs*
            --   * It blocks the creation of those files that will be created *by the caller* (after we return)
            --
            -- Note that any individual rule waiting *does not* block the creation of files built by other rules
            -- being run right. This is because everything gets executed in parallel.
            (creates_fps, basic_rule) <- case ei_clean_hist_dirty_reason of
              Left (clean_hist, clean_mtime) -> return ([fp], return (clean_hist, [clean_mtime])) -- NB: we checked that clean_mtime is still ok using sanityCheck above
              Right dirty_reason -> do
                when (verbosity >= ChattyVerbosity) $ putStrLn $ "Rebuild " ++ show fp ++ " because " ++ dirty_reason
                return (potential_creates_fps, potential_rule (e { ae_would_block_handles = fmap (const ()) wait_handle : ae_would_block_handles e }))
            
            let -- It is possible that we need two different files that are both created by the same rule. This is not an error!
                -- What we should do is remove from the remaning uncleans any files that are created by the rule we just added
                (next_fps_satisifed_here, fps') = partition (`elem` creates_fps) fps
                all_fps_satisfied_here = fp : next_fps_satisifed_here
              
                -- Augment the rule so that when it is run it sets all of the things it built to Clean again
                -- We also trim down the set of Entries it returns so that we only get entries for the *things
                -- we asked for*, not *the things the rule creates*
                rule = do
                    (nested_hist, mtimes) <- basic_rule
                    -- This is where we mark all of the files created by the rule as Clean:
                    markCleans (ae_database e) nested_hist (creates_fps `zip` mtimes)
                    -- Wake up all of the waiters on the old Building entry (if any)
                    awake_waiters mtimes
                    -- Trim unnecessary modification times before we continue
                    return $ fromRight (\fp -> internalError $ "A pending unclean rule did not create the file " ++ show fp ++ " that we thought it did") $ lookupMany all_fps_satisfied_here (creates_fps `zip` mtimes)

            -- Display a helpful message to the user explaining the rules that we have decided upon:
            when (verbosity >= ChattyVerbosity) $
                putStrLn $ "Using rule instance for " ++ showStringList (map show creates_fps) ++ " to create " ++ showStringList (map show all_fps_satisfied_here)
            
            fmap (second (second ((all_fps_satisfied_here, rule) :))) $ findAllRules e fps' would_block_handles db
  where
    verbosity = ae_verbosity e

need' :: Namespace n => ActEnv n -> [n] -> IO [Entry n]
need' e init_fps = do
    -- Figure out the rules we need to use to create all the dirty files we need
    --
    -- NB: this MVar operation does not block us because any thread only holds the database lock
    -- for a very short amount of time (and can only do IO stuff while holding it, not Act stuff).
    -- When we have to recursively invoke need, we put back into the MVar before doing so.
    (cleans, uncleans) <- modifyMVar (ae_database e) $ findAllRules e init_fps []
    
    -- Run the rules we have decided upon in parallel
    --
    -- NB: we report that the thread using parallel is blocked because it may go on to actually
    -- execute one of the parallel actions, which will bump the parallelism count without any
    -- extra parallelism actually occuring.
    unclean_times <- fmap concat $ reportWorkerBlocked (ae_report e) $ parallel (ae_pool e) $ flip map uncleans $ \(unclean_fps, rule) -> reportWorkerRunning (ae_report e) $ fmap (unclean_fps `zip`) rule

    -- For things that are being built by someone else we only do trivial work, so don't have to spawn any thread
    clean_times <- forM cleans $ \(clean_fp, rule) -> fmap ((,) clean_fp) rule
    
    let ([], reordered_times) = fromRight (\fp -> internalError $ "A call to need' didn't return a modification time for the input file " ++ show fp) $ lookupRemoveMany init_fps $ unclean_times ++ clean_times
    return reordered_times

-- | Just a unique number to identify each update we make to the 'WaitDatabase'
type WaitNumber = Int

-- | A 'WaitHandle's that cannot be awoken because the thread that
-- would do the awaking are blocked on another 'WaitHandle'. With each blocked 'WaitHandle'
-- we record the reason that we did the blocking in the first place in the form of a 'String'.
--
-- We record a 'WaitNumber' with each entry so that we can unregister a wait that we previously
-- added without interfering with information that has been added in the interim.
type BlockedWaitHandle n = (WaitNumber, n, WaitHandle ())

-- | Mapping from 'WaitHandle's being awaited upon to the 'WaitHandle's blocked
-- from being awoken as a consequence of that waiting.
data WaitDatabase n = WDB {
    wdb_next_waitno :: WaitNumber,
    wdb_waiters :: [(WaitHandle (), [BlockedWaitHandle n])]
  }

emptyWaitDatabase :: WaitDatabase n
emptyWaitDatabase = WDB {
    wdb_next_waitno = 0,
    wdb_waiters = []
  }

-- | This function is responsible for deadlock detection.
--
-- The way the scheme works is that we have a global MVar containing a WaitDatabase. This database records
-- all of the current waits in the application, along with:
--   * The wait handles that cannot be triggered at the moment due to the outstanding wait (if any)
--   * The reason that we are waiting at all
--
-- Now, before we allow the actual wait to happen we check the database of outstanding waits. If we are in
-- a situation where there is an outstanding wait on one of the handles that would become blocked by the pending
-- wait, and we are waiting on a handle already blocked by that outstanding wait, then we have a deadlock.
--
-- In this situation we throw an error instead of actually performing the wait, including in the error a descripton
-- of the dependency chain that lead to the error reconstructed from the individual wait "why" information.
registerWait :: forall n a. (Show n, Eq n) => MVar (WaitDatabase n) -> n -> WaitHandle () -> [WaitHandle ()] -> IO a -> IO a
registerWait mvar_wdb new_why new_handle new_will_block_handles act = Exception.bracket register unregister (\_ -> act)
  where
    register = modifyMVar mvar_wdb (Exception.evaluate . register')
    register' (WDB new_waitno waiters)
      = case [why_chain | (why_chain, handle) <- transitive [([new_why], new_will_block_handle) | new_will_block_handle <- new_will_block_handles], new_handle == handle] of
          why_chain:_ -> shakefileError $ "Cyclic dependency detected through the chain " ++ showStringList (map show why_chain)
          []          -> (wdb', new_waitno)
      where
        -- Update the database with the new waiters on this WaitHandle. We are careful to ensure that any
        -- existing waiters on the handle are preserved and put into the same entry in the association list.
        wdb' = WDB (new_waitno + 1) $ (new_handle, [ (new_waitno, new_why, new_will_block_handle)
                                                   | new_will_block_handle <- new_will_block_handles ] ++
                                                   find_blocked_wait_handles new_handle) :
                                      filter ((/= new_handle) . fst) waiters
        
        find_blocked_wait_handles :: WaitHandle () -> [BlockedWaitHandle n]
        find_blocked_wait_handles wait_handle = fromMaybe [] (wait_handle `lookup` waiters)
        
        -- When we compute whether we are blocked, we need to do a transitive closure. This is necessary for situations where
        -- e.g. A.o -> B.o -> C.o, because we need to see that A.o is waiting on C.o's WaitHandle through B.o's WaitHandle.
        transitive :: [([n], WaitHandle ())] -> [([n], WaitHandle ())]
        transitive init_blocked = flip fixEq init_blocked $ \blocked -> nub $ blocked ++ [ (why : why_chain, next_blocked_handle)
                                                                                         | (why_chain, blocked_handle) <- blocked
                                                                                         , (_waitno, why, next_blocked_handle) <- find_blocked_wait_handles blocked_handle ]

    -- When we have completed the wait, remove all information about it from the wait database.
    -- Since we inserted it all with a unique integer, this is rather easy to do. To prevent the
    -- database growing unnecessarily, we carefully remove any wdb_waiters entries that don't block
    -- any handles at all after the removal.
    unregister unreg_waitno = modifyMVar_ mvar_wdb (Exception.evaluate . unregister' unreg_waitno)
    unregister' unreg_waitno wdb
      = wdb { wdb_waiters = [(waiting_on, blocked') | (waiting_on, blocked) <- wdb_waiters wdb
                            , let blocked' = filter (\(waitno, _, _) -> unreg_waitno /= waitno) blocked
                            , not (null blocked')] }


data ReportDatabase = RDB {
    rdb_observed_commands :: [(String, NominalDiffTime)],
    rdb_observed_concurrency :: [(UTCTime, Int)],
    rdb_concurrency :: Int,
    rdb_start_at :: UTCTime
  }

emptyReportDatabase :: IO ReportDatabase
emptyReportDatabase = do
    ts <- getCurrentTime
    return $ RDB {
        rdb_observed_commands = [],
        rdb_observed_concurrency = [(ts, 1)],
        rdb_concurrency = 1,
        rdb_start_at = ts
      }

reportWorkerBlocked, reportWorkerRunning :: MVar ReportDatabase -> IO a -> IO a
reportWorkerBlocked = reportConcurrencyBump (-1)
reportWorkerRunning = reportConcurrencyBump 1

reportConcurrencyBump :: Int -> MVar ReportDatabase -> IO a -> IO a
reportConcurrencyBump bump mvar_rdb act = Exception.bracket (bump_concurrency bump) (\() -> bump_concurrency (negate bump)) (\() -> act)
  where bump_concurrency directed_bump = modifyMVar_ mvar_rdb $ \rdb -> getCurrentTime >>= \ts -> return $ rdb { rdb_concurrency = rdb_concurrency rdb + directed_bump, rdb_observed_concurrency = (ts, rdb_concurrency rdb - directed_bump) : rdb_observed_concurrency rdb }

reportCommand :: String -> IO a -> Act n a
reportCommand cmd act = do
    mvar_rdb <- fmap ae_report askActEnv
    liftIO $ reportCommandIO mvar_rdb cmd act

reportCommandIO :: MVar ReportDatabase -> String -> IO a -> IO a
reportCommandIO mvar_rdb cmd act = do
    start_ts <- getCurrentTime
    res <- act
    end_ts <- getCurrentTime
    
    modifyMVar_ mvar_rdb $ \rdb -> return $ rdb { rdb_observed_commands = (cmd, end_ts `diffUTCTime` start_ts) : rdb_observed_commands rdb }
    return res

produceReport :: ReportDatabase -> String
produceReport rdb = "<html><head><title>OpenShake report</title></head><body>" ++
                    "<h1>Parallelism over time</h1>" ++ parallelism ++
                    "<h1>Long-running commands</h1><table><tr><th>Command</th><th>Time</th></tr>" ++ long_running_commands ++ "</table>" ++
                    "</body></html>"
  where
    -- TODO: encode string suitably for enclosing in quotes in attribute
    attributeEncode = id
    -- TODO: encode string suitably for using as text in HTML
    htmlEncode = id
    
    parallelism = "<img src=\"" ++ attributeEncode (concurrencyChartURL (600, 200) concurrency_xy) ++ "\" />"
    -- NB: concurrency sometimes becomes negative for very small periods of time. We should probably filter these out, but
    -- for now I'll just make them to 0. It is essential that we don't let values like -1 get into the chart data sent to
    -- Google, because Charts interprets a y-range minimum of -1 as "no minimum"...
    concurrency_xy = [ (realToFrac (time `diffUTCTime` rdb_start_at rdb) :: Double, 0 `max` concurrency)
                     | (time, concurrency) <- reverse $ rdb_observed_concurrency rdb]
    
    long_running_commands = unlines ["<tr><td>" ++ htmlEncode cmd ++ "</td><td>" ++ htmlEncode (show runtime) ++ "</td></tr>" | (cmd, runtime) <- command_data]
    command_data = take 50 $ reverse $ sortBy (comparing snd) $ rdb_observed_commands rdb

-- See <http://code.google.com/apis/chart/docs/data_formats.html>, <http://code.google.com/apis/chart/docs/chart_params.html>
concurrencyChartURL :: (Int, Int) -> [(Double, Int)] -> String
concurrencyChartURL (width, height) xys
  = "http://chart.apis.google.com/chart?cht=lxy&chd=t:" ++ encode_series xs ++ "|" ++ encode_series ys ++
    "&chds=" ++ range xs ++ "," ++ range ys ++                  -- Setup data range for the text encoding
    "&chxt=x,y&chxr=0," ++ range xs ++ "|1," ++ range (0:ys) ++ -- Setup axis range (we force the y axis to start at 0 even if minimum parallelism was 1)
    "&chco=3674FB" ++                                           -- Color of line
    "&chm=B,76A4FB,0,0,0" ++                                    -- Color underneath the drawn line
    "&chs=" ++ show width ++ "x" ++ show height                 -- Image size
  where (xs, ys) = unzip xys
        
        encode_series :: Show a => [a] -> String
        encode_series = intercalate "," . map show
        
        range :: (Ord a, Show a) => [a] -> String
        range zs = show (minimum zs) ++ "," ++ show (maximum zs)


markCleans :: Namespace n => Database n -> History n -> [(n, Entry n)] -> IO ()
markCleans db_mvar nested_hist relevant_nested_times = modifyMVar_ db_mvar (return . go)
  where go init_db = foldr (\(fp, nested_time) db -> M.insert fp (Clean nested_hist nested_time) db) init_db relevant_nested_times


appendHistory :: QA n -> Act n ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

-- NB: when the found rule returns, the input file will be clean (and probably some others, too..)
type RuleFinder n = forall r. Verbosity -> [SomeRule n] -> n
                           -> (([n], ActEnv n -> (IO (History n, [Entry n]))) -> IO r)
                           -> IO r
findRule :: Namespace n => RuleFinder n
findRule verbosity rules fp k = do
    possibilities <- mapMaybeM ($ fp) rules
    -- To make sure we choose the first rule, we need to reverse the list of matches (we add them in reverse order)
    (creates_fps, action) <- case reverse possibilities of
      generator:other_matches -> do
          unless (null other_matches) $
            when (verbosity > NormalVerbosity) $
              putStrLn $ "Ambiguous rules for " ++ show fp ++ ": choosing the first one"
          return generator
      [] -> do
          mb_generator <- defaultRule fp
          case mb_generator of
            Nothing        -> shakefileError $ "No rule to build " ++ show fp
            Just generator -> return generator

    k (creates_fps, \e -> do
        (creates_times, final_nested_s) <- runAct e (AS { as_this_history = [] }) action
        return (as_this_history final_nested_s, creates_times))
