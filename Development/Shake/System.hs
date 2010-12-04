module Development.Shake.System (
    system, system', systemStdout',
    copy, mkdir, readFileLines
  ) where

import Development.Shake
import qualified Development.Shake.Utilities as Utilities

import Control.Monad.IO.Class

import Data.List

import qualified System.Process as Process
import System.Exit

import System.Directory
import System.FilePath


system' :: [String] -> Act ()
system' prog = do
    ec <- system prog
    Utilities.checkExitCode prog ec

system :: [String] -> Act ExitCode
system prog = do
    putStrLnAt VerboseVerbosity cmd
    liftIO $ Process.system cmd
  where cmd = intercalate " " prog

systemStdout' :: [String] -> Act String
systemStdout' prog = do
    (ec, stdout) <- systemStdout prog
    Utilities.checkExitCode prog ec
    return stdout

systemStdout :: [String] -> Act (ExitCode, String)
systemStdout prog = do
    putStrLnAt VerboseVerbosity cmd
    liftIO $ Utilities.systemStdout cmd
  where cmd = intercalate " " prog


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
