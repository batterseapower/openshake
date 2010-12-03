module Development.Shake.Utilities where

import qualified Control.Exception as Exception

import Control.Monad
import Control.Monad.IO.Class

import Data.List

import qualified System.Process as Process
import System.Exit

import System.FilePath
import System.IO.Error (isDoesNotExistError)
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
