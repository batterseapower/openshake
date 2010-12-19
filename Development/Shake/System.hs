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

control_characters, meta_characters :: [Char]
control_characters = ['$', '`']
meta_characters = [' ', '\t', '|', '&', ';', '(', ')', '<', '>']

-- | Shell escaping by double-quoting the argument.
--
-- See 3.1.2.3 of <http://www.gnu.org/software/bash/manual/bashref.html>
quote :: String -> String
quote x = "\"" ++ concatMap escape x ++ "\""
  where must_escape = control_characters ++ ['\"', '\\']
        escape c | c `elem` must_escape = ['\\', c]
                 | otherwise            = [c]

-- | Shell escaping by backslash-encoding the argument.
--
-- See 3.1.2.1 of <http://www.gnu.org/software/bash/manual/bashref.html#Definitions>
escape :: String -> String
escape x = concatMap escape x
  where must_escape = control_characters ++ meta_characters ++ ['\\', '\'', '\"']
        escape c | c `elem` must_escape = ['\\', c] -- TODO: I'm not using this at the moment
                 | otherwise            = [c]

readFileLines :: FilePath -> Act o [String]
readFileLines x = do
    need [x]
    liftIO $ fmap lines $ readFile x

copy :: FilePath -> FilePath -> Act o ()
copy from to = do
    mkdir $ takeDirectory to
    need [from]
    system' ["cp", quote from, quote to]

mkdir :: FilePath -> Act o ()
mkdir fp = liftIO $ createDirectoryIfMissing True fp
