module Development.Shake.System (
    system, system', systemStdout',
    copy, mkdir, readFileLines
  ) where

import Development.Shake
import Development.Shake.Utilities

import Control.Monad.IO.Class

import System.Directory
import System.FilePath


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
