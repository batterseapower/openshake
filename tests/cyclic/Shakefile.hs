import Development.Shake
import Development.Shake.C
import Development.Shake.System

import System.FilePath


main :: IO ()
main = shake $ do
    "a" *> \x -> do
        need ["b"]
    "b" *> \x -> do
        need ["a"]
    want ["a"]
