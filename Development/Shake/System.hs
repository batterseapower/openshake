{-# LANGUAGE FlexibleContexts #-}
module Development.Shake.System (
    system, system', systemStdout',
    copy, mkdir, readFileLines,
    quote
  ) where

import Development.Shake
import qualified Development.Shake.Core.Utilities as Utilities

import Control.Monad.IO.Class

import Data.List

import qualified System.Process as Process
import System.Exit

import System.Directory
import System.FilePath


system' :: [String] -> Act n ()
system' prog = do
    ec <- system prog
    Utilities.checkExitCode prog ec

system :: [String] -> Act n ExitCode
system prog = do
    putStrLnAt VerboseVerbosity cmd
    reportCommand cmd $ Process.system cmd
  where cmd = intercalate " " prog

systemStdout' :: [String] -> Act n String
systemStdout' prog = do
    (ec, stdout) <- systemStdout prog
    Utilities.checkExitCode prog ec
    return stdout

systemStdout :: [String] -> Act n (ExitCode, String)
systemStdout prog = do
    putStrLnAt VerboseVerbosity cmd
    reportCommand cmd $ Utilities.systemStdout cmd
  where cmd = intercalate " " prog

control_characters :: [Char]
control_characters = ['$', '`']
--meta_characters :: [Char]
--meta_characters = [' ', '\t', '|', '&', ';', '(', ')', '<', '>']

-- | Shell escaping by double-quoting the argument.
--
-- See 3.1.2.3 of <http://www.gnu.org/software/bash/manual/bashref.html>
quote :: String -> String
quote x = "\"" ++ concatMap escape x ++ "\""
  where must_escape = control_characters ++ ['\"', '\\']
        escape c | c `elem` must_escape = ['\\', c]
                 | otherwise            = [c]

-- TODO: I'm not using this at the moment
-- 
-- -- | Shell escaping by backslash-encoding the argument.
-- --
-- -- See 3.1.2.1 of <http://www.gnu.org/software/bash/manual/bashref.html#Definitions>
-- escape :: String -> String
-- escape x = concatMap escape x
--   where must_escape = control_characters ++ meta_characters ++ ['\\', '\'', '\"']
--         escape c | c `elem` must_escape = ['\\', c]
--                  | otherwise            = [c]

readFileLines :: (CanonicalFilePath :< n, Namespace n)
              => FilePath -> Act n [String]
readFileLines x = do
    need [x]
    liftIO $ fmap lines $ readFile x

copy :: (CanonicalFilePath :< n, Namespace n)
     => FilePath -> FilePath -> Act n ()
copy from to = do
    mkdir $ takeDirectory to
    need [from]
    system' ["cp", quote from, quote to]

mkdir :: FilePath -> Act n ()
mkdir fp = liftIO $ createDirectoryIfMissing True fp
