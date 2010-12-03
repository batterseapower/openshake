module Development.Shake.System (
    system, system', systemStdout',
    copy, mkdir, readFileLines
  ) where

import Development.Shake
import Development.Shake.Utilities

import Control.Monad.IO.Class

import System.Directory
import System.FilePath


readFileLines :: Oracle o q a => FilePath -> Act o [String]
readFileLines x = do
    need [x]
    liftIO $ fmap lines $ readFile x

quote :: String -> String
quote x = "\"" ++ x ++ "\"" -- TODO: this is probably wrong

copy :: Oracle o q a => FilePath -> FilePath -> Act o ()
copy from to = do
    mkdir $ takeDirectory to
    need [from]
    system' ["cp", quote from, quote to]

mkdir :: FilePath -> Act o ()
mkdir fp = liftIO $ createDirectoryIfMissing True fp
