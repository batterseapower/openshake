import Development.Shake
import Development.Shake.System

import Control.Monad.IO.Class

import System.FilePath


main :: IO ()
main = shake $ do
    "foo" *> \x -> do
        liftIO $ writeFile x "First rule"
    "foo" *> \x -> do
        liftIO $ writeFile x "Second rule"
    want ["foo"]
