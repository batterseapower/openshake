module Development.Shake.System (
    system, system', systemStdout',
    copy, mkdir, readFileLines,
    quote
  ) where

import Development.Shake
import qualified Development.Shake.Utilities as Utilities

import Control.Monad.IO.Class

import Data.List

import qualified System.Process as Process
import System.Exit

import System.Directory
import System.FilePath


system' :: [String] -> Act o ()
system' prog = do
    ec <- system prog
    Utilities.checkExitCode prog ec

system :: [String] -> Act o ExitCode
system prog = do
    putStrLnAt VerboseVerbosity cmd
    reportCommand cmd $ Process.system cmd
  where cmd = intercalate " " prog

systemStdout' :: [String] -> Act o String
systemStdout' prog = do
    (ec, stdout) <- systemStdout prog
    Utilities.checkExitCode prog ec
    return stdout

systemStdout :: [String] -> Act o (ExitCode, String)
systemStdout prog = do
    putStrLnAt VerboseVerbosity cmd
    reportCommand cmd $ Utilities.systemStdout cmd
  where cmd = intercalate " " prog


readFileLines :: FilePath -> Act o [String]
readFileLines x = do
    need [x]
    liftIO $ fmap lines $ readFile x

quote :: String -> String
quote x = "\"" ++ x ++ "\"" -- TODO: this is probably wrong

copy :: FilePath -> FilePath -> Act o ()
copy from to = do
    mkdir $ takeDirectory to
    need [from]
    system' ["cp", quote from, quote to]

mkdir :: FilePath -> Act o ()
mkdir fp = liftIO $ createDirectoryIfMissing True fp
