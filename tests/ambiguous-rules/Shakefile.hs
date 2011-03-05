import Development.Shake
import Development.Shake.System

import System.FilePath


main :: IO ()
main = shake $ do
    "foo" *> \x -> do
        writeFile x "First rule"
    "foo" *> \x -> do
        writeFile x "Second rule"
    want ["foo"]
