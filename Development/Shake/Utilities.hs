{-# LANGUAGE Rank2Types #-}
module Development.Shake.Utilities where

import qualified Control.Exception as Exception

import Control.Arrow (second)
import Control.Monad

import Data.List
import Data.Maybe

import System.Exit
import qualified System.Process as Process

import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp


handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist = handleIf isDoesNotExistError

handleIf :: Exception.Exception e => (e -> Bool) -> IO a -> IO a -> IO a
handleIf p handler act = Exception.handleJust (guard . p) (\() -> handler) act

expectJust :: String -> Maybe a -> a
expectJust _   (Just x) = x
expectJust msg Nothing  = error $ "expectJust: " ++ msg

lookupRemove :: Eq k => k -> [(k, v)] -> Maybe (v, [(k, v)])
lookupRemove _      []           = Nothing
lookupRemove want_k ((k, v):kvs) | want_k == k = Just (v, kvs)
                                 | otherwise   = fmap (second ((k, v) :)) $ lookupRemove want_k kvs

lookupMany :: Eq k
           => (forall r. k -> r)
           -> [k] -> [(k, v)] -> ([(k, v)], [(k, v)])
lookupMany missing_error ks init_kvs
  = mapAccumL (\kvs k -> case lookupRemove k kvs of Nothing -> missing_error k
                                                    Just (v, kvs') -> (kvs', (k, v))) init_kvs ks

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x | x == x'   = x
          | otherwise = fixEq f x'
  where x' = f x

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = go
  where go []     = return False
        go (x:xs) = do
            b <- p x
            if b then return True
                 else go xs

firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM = go
  where go []     = return Nothing
        go (mmb_x:xs) = do
            mb_x <- mmb_x
            maybe (go xs) (return . Just) mb_x

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

replicateM :: Monad m => Int -> m b -> m [b]
replicateM = genericReplicateM

genericReplicateM :: (Integral a, Monad m) => a -> m b -> m [b]
genericReplicateM init_n act = go init_n []
  where
    go 0 accum = return (reverse accum)
    go n accum = act >>= \x -> go (n - 1) (x:accum)

mapAccumLM :: Monad m => (acc -> a -> m (acc, b)) -> acc -> [a] -> m (acc, [b])
mapAccumLM f = go []
  where go ys acc []     = return (acc, reverse ys)
        go ys acc (x:xs) = do
            (acc', y) <- f acc x
            go (y:ys) acc' xs

(?) :: Bool -> (a, a) -> a
True  ? (t, _) = t
False ? (_, f) = f


checkExitCode :: (Show a, Monad m) => a -> ExitCode -> m ()
checkExitCode cmd ec = case ec of
    ExitSuccess   -> return ()
    ExitFailure i -> error $ "system': system command " ++ show cmd ++ " failed with exit code " ++ show i

systemStdout :: String -> IO (ExitCode, String)
systemStdout cmd = withSystemTempDirectory "openshake" $ \tmpdir -> do
    let stdout_fp = tmpdir </> "stdout" <.> "txt"
    ec <- Process.system $ cmd ++ " > " ++ stdout_fp
    fmap ((,) ec) $ readFile stdout_fp

systemStdout' :: String -> IO String
systemStdout' cmd = do
    (ec, stdout) <- systemStdout cmd
    checkExitCode cmd ec
    return stdout
