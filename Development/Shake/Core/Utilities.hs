module Development.Shake.Core.Utilities where

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

handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist = handleIf isDoesNotExistError

handleIf :: Exception.Exception e => (e -> Bool) -> IO a -> IO a -> IO a
handleIf p handler act = Exception.handleJust (guard . p) (\() -> handler) act

fromRight :: (a -> b) -> Either a b -> b
fromRight f (Left  a) = f a
fromRight _ (Right b) = b

expectJust :: String -> Maybe a -> a
expectJust _   (Just x) = x
expectJust msg Nothing  = error $ "expectJust: " ++ msg

lookupRemove :: Eq k => k -> [(k, v)] -> Maybe (v, [(k, v)])
lookupRemove _      []           = Nothing
lookupRemove want_k ((k, v):kvs) | want_k == k = Just (v, kvs)
                                 | otherwise   = fmap (second ((k, v) :)) $ lookupRemove want_k kvs

lookupRemoveMany :: Eq k => [k] -> [(k, v)] -> Either k ([(k, v)], [v])
lookupRemoveMany ks init_kvs
  = mapAccumLM (\kvs k -> case lookupRemove k kvs of Nothing        -> Left k
                                                     Just (v, kvs') -> Right (kvs', v)) init_kvs ks

lookupMany :: Eq k => [k] -> [(k, v)] -> Either k [v]
lookupMany ks = fmap snd . lookupRemoveMany ks

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

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = go
  where go []     = return []
        go (x:xs) = do
          mb_y <- f x
          case mb_y of
            Nothing ->             go xs
            Just y  -> liftM (y:) (go xs)

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
