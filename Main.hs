{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative (Applicative)
import Control.Arrow (first)

import Control.Exception

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
import System.Time (CalendarTime, toCalendarTime)

import qualified System.Process as Process
import System.Exit

import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe

import Text.Regex.Posix


handleDoesNotExist :: IO a -> IO (Maybe a)
handleDoesNotExist act = handleJust (guard . isDoesNotExistError) (\() -> return Nothing) (fmap Just act)

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


data Rule = R {
    r_pattern :: String,
    r_action :: FilePath -> Act ()
  }

patternMatches :: String -> FilePath -> Bool
patternMatches pattern fp = pattern =~ fp

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
    writeFile ".openshake-db" (show mb_db)
  where
    -- Doesn't work because we want to do things like "ls *.c"
    --default_oracle ("ls", fp) = unsafePerformIO $ getDirectoryContents fp 
    default_oracle ("ls", fp) = lines $ unsafePerformIO $ Process.readProcess "ls" [fp] ""
    default_oracle question   = error $ "The default oracle cannot answer the question " ++ show question

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
    putShakeState $ s { ss_rules = R { r_pattern = pattern, r_action = action } : ss_rules s }

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
        must_rerun <- case mb_hist of Nothing   -> return True
                                      Just hist -> anyM history_requires_rerun hist
        this_time <- if must_rerun
                      then runRule unclean_fp -- NB: runRule will set unclean_fp to Clean in the resulting database (along with possibily more stuff!)
                      else get_clean_mod_time unclean_fp -- Because we are actually Clean, though the history doesn't realise it yet..
        
        -- FIXME: must switch to Clean with new modtime
        return (unclean_fp, this_time)
    
    clean_times <- forM cleans $ \clean_fp -> fmap ((,) clean_fp) (get_clean_mod_time clean_fp)
    
    appendHistory $ Need (unclean_times ++ clean_times)

appendHistory :: QA -> Act ()
appendHistory extra_qa = modifyActState $ \s -> s { as_this_history = as_this_history s ++ [extra_qa] }

runRule :: FilePath -> Act ModTime
runRule fp = do
    e <- askActEnv
    case [rule | rule <- ae_global_rules e, r_pattern rule `patternMatches` fp] of
        [rule] -> do
            s <- getActState
            ((), s) <- liftIO $ runAct e s (r_action rule fp)
            putActState s
            
            -- FIXME: record as clean
            this_time <- fmap (expectJust $ "The matching rule did not create " ++ fp) $ liftIO $ getModTime fp
            return this_time
        [] -> do
            -- Not having a rule might still be OK, as long as there is some existing file here:
            mb_this_time <- liftIO $ getModTime fp
            case mb_this_time of
                Nothing        -> error $ "No rule to build " ++ fp
                Just this_time -> return this_time -- FIXME: record as Clean then
                                                   -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.
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

system' :: [String] -> Act ()
system' prog = do
    ec <- system prog
    case ec of
        ExitSuccess   -> return ()
        ExitFailure i -> error $ "system': system command " ++ show prog ++ " failed with exit code " ++ show i

system :: [String] -> Act ExitCode
system prog = liftIO $ Process.system $ intercalate " " prog

cIncludes :: FilePath -> Act [FilePath]
cIncludes fp = fmap (mapMaybe takeInclude) $ readFileLines fp
  where
    -- TODO: should probably do better than this quick and dirty hack
    -- FIXME: transitive dependencies
    trim p = dropWhile p . reverse . dropWhile p . reverse
    strip = trim (\x -> isSpace x || x == '\"')
    takeInclude xs | "#include" `isPrefixOf` map toLower xs = Just $ strip (drop (length "#include") xs)
                   | otherwise = Nothing

main :: IO ()
main = shake $ do
    "Main.exe" *> \x -> do
        cs <- ls "*.c"
        let os = map (`replaceExtension` "obj") cs
        need os
        system' $ ["gcc","-o",x] ++ os
    "*.obj" *> \x -> do
        let c = replaceExtension x "c"
        need [c]
        need =<< cIncludes c
        system' ["gcc","-c",c,"-o",x]
    want ["Main.exe"]
        