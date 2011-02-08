#! /usr/bin/env runhaskell

\begin{code}
import Control.Concurrent
import Control.Concurrent.MVar

import qualified Control.Exception as Exception
import Control.Monad

import System.Environment
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.Timeout

import System.IO
import System.IO.Temp

import Data.List

import Debug.Trace


withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory new_cwd act = Exception.bracket (do { old_cwd <- getCurrentDirectory; setCurrentDirectory new_cwd; return old_cwd }) setCurrentDirectory (\_ -> act)

isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure _ = False

doWhile_ :: IO a -> IO Bool -> IO ()
doWhile_ what test = go
  where go = what >> test >>= \b -> if b then go else return ()

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = doesFileExist fp >>= \exists -> when exists (removeFile fp)

touch :: FilePath -> IO ()
touch fp = runProcess "touch" [fp] Nothing Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()

ms = (*1000)
seconds = (*1000000)

traceShowM :: (Monad m, Show a) => m a -> m a
traceShowM mx = mx >>= \x -> trace (show x) (return x)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


assertEqualM :: (Eq a, Show a, Monad m) => a -> a -> m ()
assertEqualM expected actual = if expected == actual then return () else fail $ show expected ++ " /= " ++ show actual

assertEqualFileM :: FilePath -> String -> IO ()
assertEqualFileM fp_expected actual = readFile fp_expected >>= \expected -> assertEqualM expected actual

assertIsM :: (Show a, Monad m) => (a -> Bool) -> a -> m ()
assertIsM expectation actual = if expectation actual then return () else fail $ show actual ++ " did not match our expectations"

clean :: [FilePath] -> IO ()
clean = mapM_ removeFileIfExists

-- | Allows us to timeout even blocking that is not due to the Haskell RTS, by running the action to time out on
-- another thread.
timeoutForeign :: Int -> IO () -> IO a -> IO (Maybe a)
timeoutForeign microsecs cleanup act = flip Exception.finally cleanup $ do
    mvar <- newEmptyMVar
    forkIO $ act >>= putMVar mvar -- NB: leaves the foreign thing running even once the timeout has passed!
    timeout microsecs $ takeMVar mvar

shake_ :: FilePath -> [String] -> IO ExitCode
shake_ fp args = fmap fst3 $ shake fp args

shake :: FilePath -> [String] -> IO (ExitCode, String, String)
shake fp args = {- (\res@(ec, stdout, stderr) -> putStrLn stdout >> putStrLn stderr >> return res) =<< -} do
   extra_args <- getArgs -- NB: this is a bit of a hack!
   
   (_h_stdin, h_stdout, h_stderr, ph) <- runInteractiveProcess "runghc" (["-i../../", fp] ++ args ++ extra_args) Nothing Nothing
   mb_ec <- timeoutForeign (seconds 10) (terminateProcess ph) $ waitForProcess ph
   case mb_ec of
     Nothing -> error "shake took too long to run!"
     Just ec -> liftM2 ((,,) ec) (hGetContents h_stdout) (hGetContents h_stderr)

-- | Shake can only detect changes that are reflected by changes to the modification time.
-- Thus if we expect a rebuild we need to wait for the modification time used by the system to actually change.
waitForModificationTimeToChange :: IO ()
waitForModificationTimeToChange = withSystemTempDirectory "openshake-test" $ \tmpdir -> do
    let testfile = tmpdir </> "modtime.txt"
    writeFile testfile ""
    init_mod_time <- getModificationTime testfile
    mb_unit <- timeout (seconds 5) $ (threadDelay (seconds 1) >> writeFile testfile "") `doWhile_` (fmap (== init_mod_time) (getModificationTime testfile))
    case mb_unit of
      Nothing -> error "The modification time doesn't seem to be changing"
      Just () -> return ()

mtimeSanityCheck :: IO ()
mtimeSanityCheck = flip Exception.finally (removeFileIfExists "delete-me") $ do
    writeFile "delete-me" ""
    mtime1 <- getModificationTime "delete-me"
    threadDelay (seconds 2)
    
    writeFile "delete-me" ""
    mtime2 <- getModificationTime "delete-me"
    threadDelay (seconds 2)
    
    touch "delete-me"
    mtime3 <- getModificationTime "delete-me"
    threadDelay (seconds 2)
    
    True `assertEqualM` (mtime1 /= mtime2 && mtime2 /= mtime3 && mtime1 /= mtime3)

withTest :: FilePath -> [FilePath] -> IO a -> IO a
withTest dir clean_fps act = do
    putStr $ dir ++ ": "
    res <- withCurrentDirectory dir $ do
        clean (".openshake-db":clean_fps)
        act
    putStrLn "[OK]"
    return res

main :: IO ()
main = do
    mtimeSanityCheck
    
    withTest "lexical-scope" ["examplefile"] $ do
        ec <- shake_ "Shakefile.hs" []
        ExitSuccess `assertEqualM` ec
    
    withTest "simple-c" ["Main", "main.o", "constants.h"] $ do
        -- 1) Try a normal build. The first time around is a clean build, the second time we
        --    have to rebuild even though we already have Main:
        forM_ [42, 43] $ \constant -> do
            writeFile "constants.h" $ "#define MY_CONSTANT " ++ show constant
            
            ec <- shake_ "Shakefile.hs" []
            ExitSuccess `assertEqualM` ec
        
            out <- readProcess "./Main" [] ""
            ("The magic number is " ++ show constant ++ "\n") `assertEqualM` out
            
            waitForModificationTimeToChange
        
        -- 2) Run without changing any files, to make sure that nothing gets spuriously rebuilt:
        let interesting_files = ["Main", "main.o"]
        old_mtimes <- mapM getModificationTime interesting_files
        ec <- shake_ "Shakefile.hs" []
        ExitSuccess `assertEqualM` ec
        new_mtimes <- mapM getModificationTime interesting_files
        old_mtimes `assertEqualM` new_mtimes
        
        -- 3) Corrupt the database and check that Shake recovers
        writeFile ".openshake-db" "Junk!"
        ec <- shake_ "Shakefile.hs" []
        ExitSuccess `assertEqualM` ec

    -- TODO: test that nothing goes wrong if we change the type of oracle between runs

    withTest "deserialization-changes" ["examplefile"] $ do
        -- 1) First run has no database, so it is forced to create the file
        ec <- shake_ "Shakefile-1.hs" []
        ExitSuccess `assertEqualM` ec
        
        x <- readFile "examplefile"
        "OK1" `assertEqualM` x
        
        -- 2) The second run has a "corrupt" database because answer serialisation is shorter
        ec <- shake_ "Shakefile-2.hs" []
        ExitSuccess `assertEqualM` ec
        
        x <- readFile "examplefile"
        "OK2" `assertEqualM` x
        
        -- 2) The second run has a "corrupt" database because question serialisation is longer
        ec <- shake_ "Shakefile-3.hs" []
        ExitSuccess `assertEqualM` ec
        
        x <- readFile "examplefile"
        "OK3" `assertEqualM` x

    withTest "cyclic" [] $ do
        ec <- shake_ "Shakefile.hs" []
        isExitFailure `assertIsM` ec
    
    withTest "cyclic-harder" [] $ do
        ec <- shake_ "Shakefile.hs" []
        isExitFailure `assertIsM` ec

    withTest "creates-directory-implicitly" ["subdirectory" </> "foo"] $ do
        -- Even though our rule does not create the directory it is building into it should succeed
        ec <- shake_ "Shakefile.hs" []
        ExitSuccess `assertEqualM` ec

    withTest "lazy-exceptions" ["foo-dependency3"] $ do
        (ec, _stdout, stderr) <- shake "Shakefile.hs" ["-k"]
        
        -- All exceptions should be reported
        isExitFailure `assertIsM` ec
        (\x -> all (`isInfixOf` x) ["No rule to build", "User error in foo-dependency2", "User error in bar rule"]) `assertIsM` stderr
        
        -- We should have managed to build one of the things needed even though everything else died
        doesFileExist "foo-dependency3" >>= assertEqualM True

    withTest "lint" ["access-without-need", "access-before-need", "need-without-access"] $ do
        (ec, _stdout, stderr) <- shake "Shakefile.hs" ["--lint"]
        
        -- All exceptions should be reported
        ExitSuccess `assertEqualM` ec
        "lint.stderr" `assertEqualFileM` stderr

\end{code}