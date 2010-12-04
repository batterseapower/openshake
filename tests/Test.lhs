#! /usr/bin/env runhaskell

\begin{code}
import qualified Control.Exception as Exception
import Control.Monad

import System.Directory
import System.Exit
import System.Process

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
clean = mapM_ removeFile

shake :: IO ExitCode
shake = do
    ph <- runProcess "runghc" ["-i../../", "Shakefile.hs"] Nothing Nothing Nothing Nothing Nothing
    waitForProcess ph

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

\end{code}    