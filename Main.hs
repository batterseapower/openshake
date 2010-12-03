{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative (Applicative)
import Control.Arrow (first)

import qualified Control.Exception as Exception

import Control.Monad
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class

import Data.Char
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List

import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.Time (CalendarTime, toCalendarTime)

import qualified System.Process as Process
import System.Exit

import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe
import System.IO.Temp


handleDoesNotExist :: IO a -> IO (Maybe a)
handleDoesNotExist act = Exception.handleJust (guard . isDoesNotExistError) (\() -> return Nothing) (fmap Just act)

expectJust :: String -> Maybe a -> a
expectJust _   (Just x) = x
expectJust msg Nothing  = error $ "expectJust: " ++ msg

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = go
  where go []     = return False
        go (x:xs) = do
            b <- p x
            if b then return True
                 else go xs

(?) :: Bool -> (a, a) -> a
True  ? (t, _) = t
False ? (_, f) = f


data Rule = R {
    r_pattern :: Pattern,
    r_action :: FilePath -> Act ()
  }

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

putActState :: ActState -> Act ()
putActState s = Act (lift (State.put s))

modifyActState :: (ActState -> ActState) -> Act ()
modifyActState f = Act (lift (State.modify f))

askActEnv :: Act ActEnv
askActEnv = Act Reader.ask


shake :: Shake () -> IO ()
shake mx = do
    mb_db <- handleDoesNotExist (fmap read $ readFile ".openshake-db")
    ((), final_s) <- runShake (SE { se_oracle = default_oracle }) (SS { ss_rules = [], ss_database = fromMaybe M.empty mb_db }) mx
    writeFile ".openshake-db" (show $ ss_database final_s)
  where
    -- Doesn't work because we want to do things like "ls *.c", and since the shell does globbing we need to go through it
    --default_oracle ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
    default_oracle ("ls", what) = lines $ unsafePerformIO $ systemStdout' ["ls", what]
    default_oracle question     = error $ "The default oracle cannot answer the question " ++ show question

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
(*>) pattern action = do
    s <- getShakeState
    putShakeState $ s { ss_rules = R { r_pattern = compile pattern, r_action = action } : ss_rules s }

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
        history_requires_rerun (Need fps) = flip anyM fps $ \(fp, old_time) -> do
            new_time <- liftIO $ getModTime fp
            return (Just old_time /= new_time)
    
        get_clean_mod_time fp = fmap (expectJust ("The clean file " ++ fp ++ " was missing")) $ liftIO $ getModTime fp
    unclean_times <- forM uncleans $ \(unclean_fp, mb_hist) -> do
        mb_clean_hist <- case mb_hist of Nothing   -> return Nothing
                                         Just hist -> fmap (? (Nothing, Just hist)) $ anyM history_requires_rerun hist
        (nested_hist, nested_time) <- case mb_clean_hist of
          Nothing         -> runRule unclean_fp
          Just clean_hist -> fmap ((,) clean_hist) $ get_clean_mod_time unclean_fp -- We are actually Clean, though the history doesn't realise it yet..
        
        -- The file must now be clean, so make sure we record that fact in the database:
        modifyActState $ \s -> s { as_database = M.insert unclean_fp (Clean nested_hist nested_time) (as_database s) }
        return (unclean_fp, nested_time)
    
    clean_times <- forM cleans $ \clean_fp -> fmap ((,) clean_fp) (get_clean_mod_time clean_fp)
    
    appendHistory $ Need (unclean_times ++ clean_times)

appendHistory :: QA -> Act ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

runRule :: FilePath -> Act (History, ModTime)
runRule fp = do
    e <- askActEnv
    case [rule | rule <- ae_global_rules e, r_pattern rule `match` fp] of
        [rule] -> do
            init_db <- fmap as_database getActState
            ((), final_nested_s) <- liftIO $ runAct e (AS { as_this_history = [], as_database = init_db }) (r_action rule fp)
            modifyActState $ \s -> s { as_database = as_database final_nested_s }
            
            nested_time <- fmap (expectJust $ "The matching rule did not create " ++ fp) $ liftIO $ getModTime fp
            return (as_this_history final_nested_s, nested_time)
        [] -> do
            -- Not having a rule might still be OK, as long as there is some existing file here:
            mb_nested_time <- liftIO $ getModTime fp
            case mb_nested_time of
                Nothing          -> error $ "No rule to build " ++ fp
                Just nested_time -> return ([], nested_time) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.
        rules -> error $ "Ambiguous rules for " ++ fp ++ " (matched the patterns " ++ intercalate ", " (map (show . r_pattern) rules) ++ ")" -- TODO: disambiguate with a heuristic based on specificity of match/order in which rules were added?

type Question = (String,String)
type Answer = [String]
type Oracle = Question -> Answer

oracle :: Oracle -> Shake a -> Shake a
oracle oracle = localShakeEnv (\e -> e { se_oracle = oracle }) -- TODO: some way to combine with previous oracle?

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


checkExitCode :: (Show a, Monad m) => a -> ExitCode -> m ()
checkExitCode cmd ec = case ec of
    ExitSuccess   -> return ()
    ExitFailure i -> error $ "system': system command " ++ show cmd ++ " failed with exit code " ++ show i

system' :: MonadIO m => [String] -> m ()
system' prog = do
    ec <- system prog
    checkExitCode prog ec

system :: MonadIO m => [String] -> m ExitCode
system prog = liftIO $ Process.system $ intercalate " " prog

systemStdout' :: MonadIO m => [String] -> m String
systemStdout' prog = liftIO $ withSystemTempDirectory "openshake" $ \tmpdir -> do
    let stdout_fp = tmpdir </> "stdout" <.> "txt"
    ec <- Process.system $ intercalate " " prog ++ " > " ++ stdout_fp
    checkExitCode prog ec
    readFile stdout_fp


-- Example build system

readFileLines :: FilePath -> Act [String]
readFileLines x = do
    need [x]
    liftIO $ fmap lines $ readFile x

quote :: String -> String
quote x = "\"" ++ x ++ "\"" -- TODO: this is probably wrong

copy :: FilePath -> FilePath -> Act ()
copy from to = do
    mkdir $ takeDirectory to
    need [from]
    system' ["cp", quote from, quote to]

mkdir :: FilePath -> Act ()
mkdir fp = liftIO $ createDirectoryIfMissing True fp

cIncludes :: FilePath -> Act [FilePath]
cIncludes fp = fmap (mapMaybe takeInclude) $ readFileLines fp
  where
    -- TODO: should probably do better than this quick and dirty hack
    -- FIXME: transitive dependencies
    trim p = dropWhile p . reverse . dropWhile p . reverse
    takeInclude xs = guard ("#include" `isPrefixOf` map toLower xs) >> stripQuotes (trim isSpace (drop (length "#include") xs))
    stripQuotes ('\"':xs) = guard (not (null xs) && last xs == '\"') >> return (init xs)
    stripQuotes _ = Nothing

main :: IO ()
main = shake $ do
    "Main" *> \x -> do
        cs <- ls "*.c"
        let os = map (`replaceExtension` "o") cs
        need os
        system' $ ["gcc","-o",x] ++ os
    "*.o" *> \x -> do
        let c = replaceExtension x "c"
        need [c]
        need =<< cIncludes c
        system' ["gcc","-c",c,"-o",x]
    want ["Main"]
        