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


system' :: [String] -> Act n o ()
system' prog = do
    ec <- system prog
    Utilities.checkExitCode prog ec

system :: [String] -> Act n o ExitCode
system prog = do
    putStrLnAt VerboseVerbosity cmd
    reportCommand cmd $ Process.system cmd
  where cmd = intercalate " " prog

systemStdout' :: [String] -> Act n o String
systemStdout' prog = do
    (ec, stdout) <- systemStdout prog
    Utilities.checkExitCode prog ec
    return stdout

systemStdout :: [String] -> Act n o (ExitCode, String)
systemStdout prog = do
    putStrLnAt VerboseVerbosity cmd
    reportCommand cmd $ Utilities.systemStdout cmd
  where cmd = intercalate " " prog


readFileLines :: FilePath -> Act FileName o [String] -- TODO: more polymorphism
readFileLines x = do
    need [x]
    liftIO $ fmap lines $ readFile x

quote :: String -> String
quote x = "\"" ++ x ++ "\"" -- TODO: this is probably wrong

copy :: FilePath -> FilePath -> Act FileName o () -- TODO: more polymorphism!
copy from to = do
    mkdir $ takeDirectory to
    need [from]
    system' ["cp", quote from, quote to]

mkdir :: FilePath -> Act n o ()
mkdir fp = liftIO $ createDirectoryIfMissing True fp
