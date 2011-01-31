import Development.Shake
import Development.Shake.System

import System.FilePath

import Control.Exception
import Control.Monad.IO.Class


main :: IO ()
main = shake $ do
    "access-without-need" *> \x -> do
        liftIO $ readFile "accessed-without-need" >>= putStrLn -- It's very important we actually use the contents of the file, or it doesn't count as an access!
        system' ["touch", "access-without-need"]
    
    "access-before-need" *> \x -> do
        liftIO $ readFile "accessed-before-need" >>= putStrLn -- Ditto
        need ["accessed-before-need"]
        system' ["touch", "access-before-need"]
    
    "need-without-access" *> \x -> do
        need ["dummy-file"]
        system' ["touch", "need-without-access"]
    
    want ["access-without-need", "access-before-need", "need-without-access"]
