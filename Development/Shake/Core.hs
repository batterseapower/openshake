{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, Rank2Types, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-} -- For Exception only
module Development.Shake.Core (
    -- * The top-level monadic interface
    Shake, shake, shakeWithOptions,
    ShakeOptions(..), defaultShakeOptions,
    
    -- * Adding rules in the Shake monad and controlling their visibility
    addRule, privateTo, privateTo_,
    
    -- * Setting up initial actions
    act,

    -- * Rules
    Rule, Rule', Generator, Generator',
    
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

import Data.Typeable (Typeable) -- For Exception only

import Control.Applicative (Applicative(..))
import Control.Arrow (first, second)

import Control.Concurrent.MVar
import Control.Concurrent.ParallelIO.Local (Pool)
import qualified Control.Concurrent.ParallelIO.Local as Parallel

import Control.DeepSeq
import qualified Control.Exception.Peel as Exception

import Control.Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class
import Control.Monad.IO.Peel

-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.List
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Foldable (traverse_)

import System.Environment
import System.IO
import System.IO.Unsafe (unsafePerformIO) -- For command line parsing hack only

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


data ShakefileException = RuleError String | forall e. Exception.Exception e => ActionError e | RecursiveError [([String], ShakefileException)]
                        deriving (Typeable)

instance Show ShakefileException where
    show = unlines . showShakefileException

showShakefileException :: ShakefileException -> [String]
showShakefileException (RuleError s)         = ["Error in rule definition: " ++ s]
showShakefileException (ActionError e)       = ["Error in rule action: " ++ show e]
showShakefileException (RecursiveError sfes) = "Error due to dependents:" : concatMap (\(fps, sfe) -> (' ' : showStringList fps ++ ":") : map ("  " ++) (showShakefileException sfe)) sfes

instance NFData ShakefileException where
    rnf (RuleError a) = rnf a
    rnf (ActionError a) = a `seq` ()
    rnf (RecursiveError a) = rnf a

instance Exception.Exception ShakefileException


shakefileError :: String -> IO a
shakefileError s = Exception.throwIO $ RuleError s

internalError :: String -> a
internalError s = error $ "Internal Shake error: " ++ s


runGetAll :: Get a -> BS.ByteString -> a
runGetAll act bs = case runGetState act bs 0 of (x, bs', _) -> if BS.length bs' == 0 then x else error $ show (BS.length bs') ++ " unconsumed bytes after reading"


class (Ord n, Eq (Entry n),
       Show n, Show (Entry n),
       Binary n, Binary (Entry n),
       NFData n, NFData (Entry n)) => Namespace n where
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
    defaultRule :: Rule' ntop n
    defaultRule _ = return Nothing

    data Snapshot n

    takeSnapshot :: IO (Snapshot n)

    lintSnapshots :: [n]                             -- ^ Files that the rule claims to build. An empty list if linting a top-level action.
                  -> [(Snapshot n, Snapshot n, [n])] -- ^ Sequence of snapshots taken just before and after running rule code, and the list of files needed by the rule code as it exits.
                                                     -- The list is in sequential order: earlier fragments of rule code make snapshot tranisitions that appear earlier in the list.
                                                     -- The last list of files will always be empty because the rule exits for the last time by returning normally rather than needing anything.
                  -> [String]                        -- ^ Rule lint errors, if any


type Rule' ntop n = n -> IO (Maybe (Generator' ntop n))
type Rule n = Rule' n n

data RuleClosure n = RC {
    rc_closure :: [[RuleClosure n]], -- ^ Rules closed over. Outermost list represents levels of nested, closly bound rules are near the front. Innermost list represents multiplicity of rules at a level.
    rc_rule :: Rule n
  }

type Generator' ntop n = ([n], Act ntop [Entry n])
type Generator n = Generator' n n

data ShakeOptions = ShakeOptions {
    shakeVerbosity :: Verbosity,   -- ^ Verbosity of logging
    shakeThreads :: Int,           -- ^ Number of simultaneous build actions to run
    shakeReport :: Maybe FilePath, -- ^ File to write build report to, if any
    shakeContinue :: Bool,         -- ^ Attempt to build as much as possible, even if we get exceptions during building
    shakeLint :: Bool              -- ^ Run the build sequentially, sanity checking user rules at each step
  }

defaultShakeOptions :: ShakeOptions
defaultShakeOptions = ShakeOptions {
    shakeVerbosity = unsafePerformIO verbosity,
    shakeThreads = numCapabilities,
    shakeReport = Just "openshake-report.html",
    shakeContinue = unsafePerformIO continue,
    shakeLint = unsafePerformIO lint
  }
  where
    -- TODO: when we have more command line options, use a proper command line argument parser.
    -- We should also work out whether shake should be doing argument parsing at all, given that it's
    -- meant to be used as a library function...
    continue = fmap ("-k" `elem`) getArgs
    lint = fmap ("--lint" `elem`) getArgs
    verbosity = fmap (\args -> fromMaybe NormalVerbosity $ listToMaybe $ reverse [ case rest of ""  -> VerboseVerbosity
                                                                                                "v" -> ChattyVerbosity
                                                                                                _   -> toEnum (fromEnum (minBound :: Verbosity) `max` read rest `min` fromEnum (maxBound :: Verbosity))
                                                                                 | '-':'v':rest <- args ]) getArgs


newtype ShakeEnv n = SE {
    se_available_rules :: [[RuleClosure n]]    -- ^ Presented in corect (non-reversed) order
  }

data ShakeState n = SS {
    ss_rules :: [RuleClosure n],               -- ^ Accumulated in reverse order
    ss_acts :: [([[RuleClosure n]], Act n ())]
  }

newtype Shake n a = Shake { unShake :: State.StateT (ShakeState n) (Reader.Reader (ShakeEnv n)) a }
                  deriving (Functor, Applicative, Monad)

runShake :: ShakeEnv n -> ShakeState n -> Shake n a -> (a, ShakeState n)
runShake e s mx = Reader.runReader (State.runStateT (unShake mx) s) e

-- getShakeState :: Shake n (ShakeState n)
-- getShakeState = Shake (lift State.get)

-- putShakeState :: ShakeState -> Shake ()
-- putShakeState s = Shake (lift (State.put s))

asksShakeEnv :: (ShakeEnv n -> a) -> Shake n a
asksShakeEnv extract = Shake $ lift $ Reader.asks extract

modifyShakeState :: (ShakeState n -> ShakeState n) -> Shake n ()
modifyShakeState f = Shake (State.modify f)


-- | The rules created by the first action supplied to 'privateTo' will be visible only to
-- themselves and the second action supplied to 'privateTo'. However, any rules created
-- by the second action will be visible both in the outside world and within the first action.
--
-- Thus, the first action creates rules that are "private" and do not leak out. This can be
-- helpful if you want to override particular 'need' calls with specialised actions.
privateTo :: Shake n a -> (a -> Shake n b) -> Shake n b
privateTo privates private_to = Shake $ State.StateT $ \s -> Reader.reader $ \e -> let (a, s') = Reader.runReader (State.runStateT (unShake privates) (s { ss_rules = [] })) e_private
                                                                                       e_private = e { se_available_rules = reverse (ss_rules s') : se_available_rules e }
                                                                                   in Reader.runReader (State.runStateT (unShake (private_to a)) (s' { ss_rules = ss_rules s })) e_private

-- | Version of 'privateTo' where the two nested actions don't return anything
privateTo_ :: Shake n () -> Shake n () -> Shake n ()
privateTo_ privates private_to = privateTo privates (\() -> private_to)


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
prepareStatus (Failed _)           = Nothing

type BuildingWaitHandle n = WaitHandle (Either ShakefileException (Entry n)) -- TODO: record list of files created that we are actually waiting on, for better deadlock errors

-- NB: use of the Clean constructor is just an optimisation that means we don't have to recursively recheck dependencies
-- whenever a file is need -- instead we can cut the checking process off if we discover than a file is marked as Clean.
-- Of course, this might go a bit wrong if the file becomes invalidated *during a Shake run*, but we accept that risk.
data Status n = Dirty (History n) (Entry n) -- NB: the Dirty entry is only valid if the History has not been invalidated! (Key difference from standard Shake: we cache mtime for Dirty files as well...)
              | Clean (History n) (Entry n)
              | Building (Maybe (History n, Entry n)) (BuildingWaitHandle n)
              | Failed ShakefileException

deriving instance (Namespace n) => Show (Status n)

instance Namespace n => NFData (Status n) where
    rnf (Dirty a b) = rnf a `seq` rnf b
    rnf (Clean a b) = rnf a `seq` rnf b
    rnf (Building a b) = rnf a `seq` b `seq` ()
    rnf (Failed a) = rnf a

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
    as_this_history :: History n,
    as_snapshots :: [(Snapshot n, Snapshot n, [n])]
  }

data ActEnv n = AE {
    ae_would_block_handles :: [WaitHandle ()], -- ^ A list of handles that would be incapable of awakening if the action were to
                                               --   block indefinitely here and now. This is used in the deadlock detector.
    ae_rules :: [[RuleClosure n]],
    ae_database :: Database n,
    ae_wait_database :: MVar (WaitDatabase n),
    ae_report :: MVar ReportDatabase,
    ae_pool :: Pool,
    ae_options :: ShakeOptions
  }


data Act n a = Act { unAct :: forall m. (MonadLint m, LintNamespace m ~ n) => Reader.ReaderT (ActEnv n) (State.StateT (ActState n) m) a }

instance Functor (Act n) where
    fmap = liftM

instance Applicative (Act n) where
    pure = return
    (<*>) = ap

instance Monad (Act n) where
    return x = Act (return x)
    Act mx >>= fxmy = Act $ mx >>= \x -> case fxmy x of Act it -> it

instance MonadIO (Act n) where
    liftIO io = Act (liftIO io)

instance MonadPeelIO (Act n) where
    -- Thanks to Anders Kaseorg for this definition (I added a bit of NoLint wrapping to work around the LintNamespace constraint)
    peelIO = toAct (liftM (\k (Act mx) -> liftM toAct (k mx)) peelIO)
      where
        toAct :: Reader.ReaderT (ActEnv n) (State.StateT (ActState n) (NoLint n)) a -> Act n a
        toAct mx = Act (Reader.mapReaderT (State.mapStateT (liftIO . unNoLint)) mx)


runAct :: (MonadLint m, LintNamespace m ~ n) => ActEnv n -> ActState n -> Act n a -> m (a, ActState n)
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
actVerbosity = fmap (shakeVerbosity . ae_options) askActEnv

putLog :: MonadIO m => String -> m ()
putLog = liftIO . hPutStrLn stderr

putStrLnAt :: Verbosity -> String -> Act n ()
putStrLnAt at_verbosity msg = do
    verbosity <- actVerbosity
    when (verbosity >= at_verbosity) $ putLog msg


-- NB: if you use shake in a nested way bad things will happen to parallelism
-- TODO: make parallelism configurable?
shake :: Namespace n => Shake n () -> IO ()
shake = shakeWithOptions defaultShakeOptions

shakeWithOptions :: forall n. Namespace n => ShakeOptions -> Shake n () -> IO ()
shakeWithOptions opts mx = Parallel.withPool (shakeThreads opts) $ \pool -> do
    mb_bs <- handleDoesNotExist (return Nothing) $ fmap Just $ BS.readFile ".openshake-db"
    db <- case mb_bs of
        Nothing -> do
            when (shakeVerbosity opts >= NormalVerbosity) $ putLog "Database did not exist, doing full rebuild"
            return M.empty
         -- NB: we force the input ByteString because we really want the database file to be closed promptly
        Just bs -> length (BS.unpack bs) `seq` (Exception.evaluate (rnf db) >> return db) `Exception.catch` \(Exception.ErrorCall reason) -> do
            when (shakeVerbosity opts >= NormalVerbosity) $ putLog $ "Database unreadable (" ++ reason ++ "), doing full rebuild"
            return M.empty
          where db = runGetAll getPureDatabase bs
    
    when (shakeVerbosity opts >= ChattyVerbosity) $ putStr $ "Initial database:\n" ++ unlines [show fp ++ ": " ++ show status | (fp, status) <- M.toList db]
    db_mvar <- newMVar db
    
    wdb_mvar <- newMVar emptyWaitDatabase
    report_mvar <- emptyReportDatabase >>= newMVar

    -- Collect rules and wants, then execute the collected Act actions (in any order)
    let ((), complete_s) = runShake (SE { se_available_rules = [reverse (ss_rules complete_s)] }) (SS { ss_rules = [], ss_acts = [] }) mx

        -- You might think that we could lose the type signature here, and then inline mk_e into its sole use site.
        -- Unfortunately, that doesn't type check properly on GHC 7.0.1.20101215 (i.e. RC2), and I have no idea why.
        mk_e :: [[RuleClosure n]] -> ActEnv n
        mk_e act_rules = AE { ae_would_block_handles = [], ae_rules = act_rules, ae_database = db_mvar, ae_wait_database = wdb_mvar, ae_report = report_mvar, ae_pool = pool, ae_options = opts }
        
        run_acts :: forall m. (MonadLint m, LintNamespace m ~ n) => m ()
        run_acts = void $ parallel pool $ flip map (ss_acts complete_s) $ \(act_rules, act) -> runActLinted [] (mk_e act_rules) act
    
    if shakeLint opts
     then do
      ss_mvar <- newEmptyMVar
      ss <- takeSnapshot
      (x, _) <- (flip State.runStateT ss . flip Reader.runReaderT ss_mvar) $ unLint' run_acts
      return x
     else unNoLint run_acts
    
    final_report <- takeMVar report_mvar
    traverse_ (\report_fp -> writeFile report_fp (produceReport final_report)) (shakeReport opts)
    
    final_db <- takeMVar db_mvar
    BS.writeFile ".openshake-db" (runPut $ putPureDatabase final_db)


-- | Perform the specified action once we are done collecting rules in the 'Shake' monad.
-- Just like 'want', there is no guarantee about the order in which the actions will be will be performed.
act :: Act n () -> Shake n ()
act what = do
    rules <- asksShakeEnv se_available_rules
    modifyShakeState (\s -> s { ss_acts = (rules, what) : ss_acts s })


addRule :: Rule n -> Shake n ()
addRule rule = do
    rules <- asksShakeEnv se_available_rules
    modifyShakeState $ \s -> s { ss_rules = RC rules rule : ss_rules s }

need :: Namespace n => [n] -> Act n [Entry n]
need fps = do
    e <- askActEnv
    
    need_times <- Act $ Reader.ReaderT $ \_ -> State.StateT $ \s -> do
        mb_sss <- retakeSnapshot fps
        need_times <- need' e fps
        return (need_times, s { as_snapshots = maybe id (:) mb_sss (as_snapshots s) })
    
    appendHistory $ Need (fps `zip` need_times)
    return need_times

withoutMVar :: MonadPeelIO m => MVar a -> a -> m b -> m (a, b)
withoutMVar mvar x act = do
    liftIO (putMVar mvar x)
    -- Suprisingly, it is important that we take from the MVar if there is an exception from act.
    -- The reason is that we might have something like this:
    --    modfiyMVar mvar $ \x -> withoutMVar mvar x $ throwIO e
    --
    -- If we don't take from the MVar when we get the exception, modifyMVar will block because
    -- its onException handler tries to put into the (full) MVar.
    y <- act `Exception.onException` liftIO (takeMVar mvar)
    x' <- liftIO (takeMVar mvar)
    return (x', y)

-- We assume that the rules do not change to include new dependencies often: this lets
-- us not rerun a rule as long as it looks like the dependencies of the *last known run*
-- of the rule have not changed
doesQARequireRerun :: (Namespace n, Monad m) => ([n] -> m [Entry n]) -> QA n -> m (Maybe String)
doesQARequireRerun need (Need nested_fps_times) = do
    let (nested_fps, nested_old_times) = unzip nested_fps_times
    -- NB: if this Need is for a generated file we have to build it again if any of the things *it* needs have changed,
    -- so we recursively invoke need in order to check if we have any changes
    nested_new_times <- need nested_fps
    return $ firstJust $ (\f -> zipWith3 f nested_fps nested_new_times nested_old_times) $
        \fp old_time new_time -> guard (old_time /= new_time) >> return ("modification time of " ++ show fp ++ " has changed from " ++ show old_time ++ " to " ++ show new_time)


class (Functor m, Monad m, MonadIO m, MonadPeelIO m) => MonadLint m where
    type LintNamespace m
    parallel :: Pool -> [m a] -> m [a]
    modifyMVarLint :: MVar a -> (a -> m (a, b)) -> m b
    retakeSnapshot :: [LintNamespace m] -> m (Maybe (Snapshot (LintNamespace m), Snapshot (LintNamespace m), [LintNamespace m]))
    liftBlockingIO :: IO a -> m a


newtype NoLint n a = NoLint { unNoLint :: IO a }
                  deriving (Functor, Monad, MonadIO, MonadPeelIO)

instance MonadLint (NoLint n) where
    type LintNamespace (NoLint n) = n
    parallel pool = NoLint . Parallel.parallel pool . map unNoLint
    modifyMVarLint mvar f = NoLint $ modifyMVar mvar (unNoLint . f)
    retakeSnapshot _ = return Nothing
    liftBlockingIO = liftIO


newtype Lint' n a = Lint' { unLint' :: Reader.ReaderT (MVar (Snapshot n)) (State.StateT (Snapshot n) IO) a }
                  deriving (Functor, Monad, MonadIO, MonadPeelIO)

lintIO :: ((Lint' n a -> IO a) -> IO b) -- ^ Supplies the IO action with a way to convert Lint actions into IO actions for the duration
       -> Lint' n b
lintIO f = Lint' $ Reader.ReaderT $ \ss_mvar -> State.StateT $ \ss -> do
    -- Restore the most recent Snapshot to the MVar while running an outside action, in case
    -- that outside IO action schedules another Lint' action that will update the current Snapshot.
    putMVar ss_mvar ss
    res <- f $ \lint -> modifyMVar ss_mvar (\ss -> liftM swap (State.runStateT (Reader.runReaderT (unLint' lint) ss_mvar) ss))
    -- If we scheduled another Lint action during that last call, the Snapshot will have changed.
    ss <- takeMVar ss_mvar
    return (res, ss)

instance Namespace n => MonadLint (Lint' n) where
    type LintNamespace (Lint' n) = n
    -- My first thought was that if in non-linting mode, we could just run actions in parallel. If in linting mode, we could run them sequentially
    -- so we could validate the changes made at every step.
    --
    -- Unfortunately, this isn't very cool because a rule might need something that is already being built by another branch above. E.g. I could
    -- need ["a", "b"], and the rule for ["a"] could need ["b"]. Now I'm screwed because the entry for "b" will be a WaitHandle, but waiting on it will
    -- deadlock.
    --
    -- So I still need to keep around the mechanism of parallelism in lint mode, even though I only permit one thread to run at a time.
    parallel pool acts = lintIO $ \lint_to_io -> Parallel.parallel pool (map lint_to_io acts)
    
    modifyMVarLint mvar f = Lint' $ Reader.ReaderT $ \e -> State.StateT $ \s -> modifyMVar mvar (\x -> liftM (\((a, b), s) -> (a, (b, s))) $ State.runStateT (Reader.runReaderT (unLint' (f x)) e) s)
    
    retakeSnapshot fps = Lint' $ Reader.ReaderT $ \_e -> State.StateT $ \ss -> do
        -- Record data so we can lint the IO done in between entering a user rule and it invoking need
        ss' <- takeSnapshot
        -- Rule code tranisitioned from ss to ss' before needing fps
        return (Just (ss, ss', fps), ss')

    liftBlockingIO io = lintIO (const io)


findAllRules :: (Namespace n, MonadLint m, LintNamespace m ~ n)
             => ActEnv n
             -> [n]             -- ^ The files that we wish to find rules for
             -> [WaitHandle ()] -- ^ Handles that would be blocked if we blocked the thread right now
             -> PureDatabase n
             -> m (PureDatabase n,
                   ([(n,   m (Either ShakefileException (Entry n)))],  -- Action that just waits for a build in progress elsewhere to complete
                    [([n], m (Either ShakefileException [Entry n]))])) -- Action that creates (possibly several) of the files we asked for by invoking a user rule
findAllRules _ []       _                   db = return (db, ([], []))
findAllRules e (fp:fps) would_block_handles db = do
    (fps, would_block_handles, db, res_transformer) <- do
      let ei_unclean_clean = case M.lookup fp db of
             -- If the file is totally unknown to the database we're certainly going to have to build it
            Nothing                     -> Left Nothing
             -- Likewise if the file is known but we are the first to notice that the file is dirty, though in this case "building" it might just mean marking it as clean
            Just (Dirty hist mtime)     -> Left (Just (hist, mtime))
             -- We've previously discovered the file to be clean: return an action that just returns the computed entry directly
            Just (Clean _ mtime)        -> Right $ return (Right mtime)
             -- Someone else is in the process of making the file clean. Return an action that wait on the wait handle for it to complete
            Just (Building _ wait_mvar) -> Right $ liftBlockingIO $ do
              -- We can avoid a lot of fuss if the wait handle is already triggered, so there can be no waiting.
              -- This is purely a performance optimisation:
              may_wait <- mayWaitOnWaitHandle wait_mvar
              let wrapper | may_wait  = reportWorkerBlocked (ae_report e) .
                                        registerWait (ae_wait_database e) fp (fmap (const ()) wait_mvar) (ae_would_block_handles e) .
                                        Parallel.extraWorkerWhileBlocked (ae_pool e) -- NB: We must spawn a new pool worker while we wait, or we might get deadlocked by depleting the pool of workers
                          | otherwise = id
              -- NB: we communicate the ModTimes of files that we were waiting on the completion of via the BuildingWaitHandle
              wrapper (waitOnWaitHandle wait_mvar)
             -- The file we depended on has completed building and it failed to do so: rethrow later. My guiding principle here is that
             -- this should behave the same as if building the file had been in progress when we got here, so delay the exception for a bit.
            Just (Failed sfe)           -> Right $ return (Left sfe) -- TODO: common up with Clean?
      case ei_unclean_clean of
          Right clean_act -> return (fps, would_block_handles, db, second (first ((fp, clean_act) :)))
          Left mb_hist -> do
             -- 0) The artifact is *probably* going to be rebuilt, though we might still be able to skip a rebuild
             -- if a check of its history reveals that we don't need to. Get the rule we would use to do the rebuild.
             -- If this throws an exception, it is the fault of the **caller** of need so DON'T catch it
            (potential_creates_fps, potential_rule) <- liftIO $ findRule verbosity (ae_rules e) fp
            
            let ei_why_rule_insane_unit = do
                    -- 1) Basic sanity check that the rule creates the file we actually need
                    unless (fp `elem` potential_creates_fps) $ Left $ "A rule matched " ++ show fp ++ " but claims not to create it, only the files " ++ showStringList (map show potential_creates_fps)
            
                    -- 2) Make sure that none of the files that the proposed rule will create are not Dirty/unknown to the system.
                    --    This is because it would be unsafe to run a rule creating a file that might be in concurrent
                    --    use (read or write) by another builder process.
                    let non_dirty_fps = filter (\non_dirty_fp -> case M.lookup non_dirty_fp db of Nothing -> False; Just (Dirty _ _) -> False; _ -> True) potential_creates_fps
                    unless (null non_dirty_fps) $ Left $ "A rule promised to yield the files " ++ showStringList (map show potential_creates_fps) ++ " in the process of building " ++ show fp ++
                                                         ", but the files " ++ showStringList (map show non_dirty_fps) ++ " have been independently built by someone else"
                    
                    -- Everything is OK!
                    return ()
            
            case ei_why_rule_insane_unit of
               -- If the rule is busted, record the failed build attempt in the DB (may as well) and create a "clean" action that actually just raises an error.
               -- By raising the error in the returned actions rather than right away we ensure that the exception gets reported as a problem in the files that
               -- we needed, rather than a problem in the guy doing the needing
              Left why_rule_insane -> return (fps, would_block_handles, M.insert fp (Failed sfe) db, second (first ((fp, return (Left sfe)) :)))
                where sfe = RuleError why_rule_insane
              Right () -> do
                -- NB: we have to find the rule and mark the things it may create as Building *before* we determine whether the
                -- file is actually dirty according to its history. This is because if the file *is* dirty according to that history
                -- then we want to prevent any recursive invocations of need from trying to Build some file that we have added a
                -- pending_unclean entry for already
                --
                -- NB: people wanting *any* of the files created by this rule should wait on the same BuildingWaitHandle.
                -- However, we fmap each instance of it so that it only reports the Entry information for exactly the file you care about.
                (wait_handle, awake_waiters) <- liftIO newWaitHandle
                db <- return $ foldr (\(potential_creates_fp, extractor) db -> M.insert potential_creates_fp (Building mb_hist (fmap (liftM extractor) wait_handle)) db) db (potential_creates_fps `zip` listExtractors)
            
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
                                                                          mb_insane_reason <- liftIO $ sanityCheck fp mtime
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
                    when (verbosity >= ChattyVerbosity) $ putLog $ "Rebuild " ++ show fp ++ " because " ++ dirty_reason
                    return (potential_creates_fps, potential_rule (\rules -> e { ae_rules = rules, ae_would_block_handles = fmap (const ()) wait_handle : ae_would_block_handles e }))
            
                let -- It is possible that we need two different files that are both created by the same rule. This is not an error!
                    -- What we should do is remove from the remaning uncleans any files that are created by the rule we just added
                    (next_fps_satisifed_here, fps') = partition (`elem` creates_fps) fps
                    all_fps_satisfied_here = fp : next_fps_satisifed_here
              
                    -- Augment the rule so that when it is run it sets all of the things it built to Clean again
                    -- We also trim down the set of Entries it returns so that we only get entries for the *things
                    -- we asked for*, not *the things the rule creates*
                    rule = do
                        -- Report any standard IO errors as ShakefileExceptions so we can delay them until the end
                        -- At the same time, be careful not to wrap ShakefileExceptions from any nested needs.
                        putLog $ "Running rule code to create " ++ showStringList (map show all_fps_satisfied_here)
                        ei_sfe_result <- if shakeContinue (ae_options e)
                                         then fmap (either (\e -> Left (ActionError (e :: Exception.SomeException))) id) $
                                                   Exception.try (Exception.try basic_rule)
                                         else fmap Right basic_rule
                        -- Everything else does not need to be monitored by the linter
                        liftIO $ do
                            let (ei_sfe_mtimes, creates_statuses) = case ei_sfe_result of
                                   -- Building the files succeeded, we should mark them as clean
                                  Right (nested_hist, mtimes) -> (Right mtimes, map (Clean nested_hist) mtimes)
                                   -- Building the files failed, so we need to mark it as such
                                  Left sfe -> (Left sfe, repeat (Failed sfe))
                            -- This is where we mark all of the files created by the rule as Clean/Failed:
                            updateStatus (ae_database e) (creates_fps `zip` creates_statuses)
                            -- Wake up all of the waiters on the old Building entry (if any)
                            awake_waiters ei_sfe_mtimes
                            -- Trim unnecessary modification times before we continue
                            return $ fmap (\mtimes -> fromRight (\fp -> internalError $ "A pending unclean rule did not create the file " ++ show fp ++ " that we thought it did") $ lookupMany all_fps_satisfied_here (creates_fps `zip` mtimes)) ei_sfe_mtimes
            
                -- Display a helpful message to the user explaining the rules that we have decided upon:
                when (verbosity >= ChattyVerbosity) $
                    putLog $ "Using rule instance for " ++ showStringList (map show creates_fps) ++ " to create " ++ showStringList (map show all_fps_satisfied_here)
            
                return (fps', would_block_handles, db, second (second ((all_fps_satisfied_here, rule) :)))
    fmap res_transformer $ findAllRules e fps would_block_handles db
  where
    verbosity = shakeVerbosity (ae_options e)

need' :: (Namespace n, MonadLint m, LintNamespace m ~ n) => ActEnv n -> [n] -> m [Entry n]
need' e init_fps = do
    -- Figure out the rules we need to use to create all the dirty files we need
    --
    -- NB: this MVar operation does not block us because any thread only holds the database lock
    -- for a very short amount of time (and can only do IO stuff while holding it, not Act stuff).
    -- When we have to recursively invoke need, we put back into the MVar before doing so.
    (cleans, uncleans) <- modifyMVarLint (ae_database e) $ findAllRules e init_fps []
    
    -- Run the rules we have decided upon in parallel
    --
    -- NB: we report that the thread using parallel is blocked because it may go on to actually
    -- execute one of the parallel actions, which will bump the parallelism count without any
    -- extra parallelism actually occuring.
    unclean_times <- reportWorkerBlocked (ae_report e) $ parallel (ae_pool e) $ flip map uncleans $ \(unclean_fps, rule) -> reportWorkerRunning (ae_report e) $ liftM (fmapEither (map show unclean_fps,) (unclean_fps `zip`)) rule
    
    -- For things that are being built by someone else we only do trivial work, so don't have to spawn any thread
    clean_times <- forM cleans $ \(clean_fp, rule) -> liftM (fmapEither ([show clean_fp],) (\mtime -> [(clean_fp, mtime)])) rule
    
    -- Gather up any failures experienced in recursive needs, and the modtimes for files that were built succesfully
    let (failures, all_timess) = partitionEithers $ unclean_times ++ clean_times
        ([], reordered_times) = fromRight (\fp -> internalError $ "A call to need' didn't return a modification time for the input file " ++ show fp) $ lookupRemoveMany init_fps (concat all_timess)
    
    if null failures
     then return reordered_times
     else liftIO $ Exception.throwIO $ RecursiveError failures

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
    register = modifyMVar mvar_wdb register'
    register' (WDB new_waitno waiters)
      = case [why_chain | (why_chain, handle) <- transitive [([new_why], new_will_block_handle) | new_will_block_handle <- new_will_block_handles], new_handle == handle] of
          why_chain:_ -> shakefileError $ "Cyclic dependency detected through the chain " ++ showStringList (map show why_chain)
          []          -> return (wdb', new_waitno)
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

reportWorkerBlocked, reportWorkerRunning :: MonadPeelIO m => MVar ReportDatabase -> m a -> m a
reportWorkerBlocked = reportConcurrencyBump (-1)
reportWorkerRunning = reportConcurrencyBump 1

reportConcurrencyBump :: MonadPeelIO m => Int -> MVar ReportDatabase -> m a -> m a
reportConcurrencyBump bump mvar_rdb act = Exception.bracket (liftIO $ bump_concurrency bump) (\() -> liftIO $ bump_concurrency (negate bump)) (\() -> act)
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


updateStatus :: Namespace n => Database n -> [(n, Status n)] -> IO ()
updateStatus db_mvar fp_statuses = modifyMVar_ db_mvar (return . go)
  where go init_db = foldr (\(fp, status) db -> M.insert fp status db) init_db fp_statuses


appendHistory :: QA n -> Act n ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

findRule :: (Namespace n, MonadLint m, LintNamespace m ~ n)
         => Verbosity -> [[RuleClosure n]] -> n
         -> IO ([n], ([[RuleClosure n]] -> ActEnv n) -> m (History n, [Entry n]))
findRule verbosity ruless fp = do
    possibilities <- flip mapMaybeM ruless $ \rules -> do
        generators <- mapMaybeM (\rc -> liftM (fmap ((,) (rc_closure rc))) $ rc_rule rc fp) rules
        return (guard (not (null generators)) >> Just generators)
    (clo_rules, (creates_fps, action)) <- case possibilities of
      (generator:other_matches):_next_level -> do
          unless (null other_matches) $
            when (verbosity > NormalVerbosity) $
              putLog $ "Ambiguous rules for " ++ show fp ++ ": choosing the first one"
          return generator
      [] -> do
          mb_generator <- defaultRule fp
          case mb_generator of
            Nothing        -> shakefileError $ "No rule to build " ++ show fp
            Just generator -> return $ ([], generator) -- TODO: generalise to allow default rules to refer to others?

    return (creates_fps, \mk_e -> runActLinted creates_fps (mk_e clo_rules) action)

runActLinted :: (Namespace n, MonadLint m, LintNamespace m ~ n) => [n] -> ActEnv n -> Act n a -> m (History n, a)
runActLinted creates_fps e action = do
    (res, final_nested_s) <- runAct e (AS { as_this_history = [], as_snapshots = [] }) action
    -- User code transitioned from ss to ss' before returning without needing anything else
    mb_sss <- retakeSnapshot []
    -- FIXME: accumulate errors rather than showing them eagerly like this
    liftIO $ mapM_ (hPutStrLn stderr) $ lintSnapshots creates_fps (reverse $ maybe id (:) mb_sss (as_snapshots final_nested_s))
    return (as_this_history final_nested_s, res)
