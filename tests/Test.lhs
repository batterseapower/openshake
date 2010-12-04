#! /usr/bin/env runhaskell

\begin{code}
import Control.Concurrent
import Control.Concurrent.MVar

import qualified Control.Exception as Exception
import Control.Monad

import System.Directory
import System.Exit
import System.Process
import System.Timeout

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory new_cwd act = Exception.bracket (do { old_cwd <- getCurrentDirectory; setCurrentDirectory new_cwd; return old_cwd }) setCurrentDirectory (\_ -> act)

isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure _ = False


assertEqualM :: (Eq a, Show a, Monad m) => a -> a -> m ()
assertEqualM expected actual = if expected == actual then return () else fail $ show expected ++ " /= " ++ show actual

assertIsM :: (Show a, Monad m) => (a -> Bool) -> a -> m ()
assertIsM expectation actual = if expectation actual then return () else fail $ show actual ++ " did not match our expectations"

clean :: [FilePath] -> IO ()
clean = mapM_ (\fp -> doesFileExist fp >>= \exists -> when exists (removeFile fp))

-- | Allows us to timeout even blocking that is not due to the Haskell RTS, by running the action to time out on
-- another thread.
timeoutForeign :: Int -> IO a -> IO (Maybe a)
timeoutForeign microsecs act = do
    mvar <- newEmptyMVar
    forkIO $ act >>= putMVar mvar -- NB: leaves the foreign thing running even once the timeout has passed!
    timeout microsecs $ takeMVar mvar

shake :: IO ExitCode
shake = do
    ph <- runProcess "runghc" ["-i../../", "Shakefile.hs"] Nothing Nothing Nothing Nothing Nothing
    let seconds = (*1000000)
    mb_ec <- timeoutForeign (seconds 10) $ waitForProcess ph
    case mb_ec of
      Nothing -> error "shake took too long to run!"
      Just ec -> return ec

main :: IO ()
main = do
    withCurrentDirectory "simple-c" $ do
        clean [".openshake-db", "Main", "main.o", "constants.h"]
        
        -- NB: the first time around is a clean build, the second time we have to rebuild even though we already have Main
        forM_ [42, 43] $ \constant -> do
            writeFile "constants.h" $ "#define MY_CONSTANT " ++ show constant
            
            ec <- shake
            ExitSuccess `assertEqualM` ec
        
            out <- readProcess "./Main" [] ""
            ("The magic number is " ++ show constant ++ "\n") `assertEqualM` out
        
        -- One last run, without changing any files, to make sure that nothing gets spuriously rebuilt:
        let interesting_files = ["Main", "main.o"]
        old_mtimes <- mapM getModificationTime interesting_files
        ec <- shake
        ExitSuccess `assertEqualM` ec
        new_mtimes <- mapM getModificationTime interesting_files
        old_mtimes `assertEqualM` new_mtimes

    withCurrentDirectory "cyclic" $ do
        ec <- shake
        isExitFailure `assertIsM` ec

\end{code}