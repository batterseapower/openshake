import Development.Shake
import Development.Shake.System

import System.FilePath

import Control.Exception
import Control.Monad.IO.Class


main :: IO ()
main = shake $ do
    "access-without-need" *> \x -> do
        readFile "accessed-without-need"
        system' ["touch", "access-without-need"]
    
    "access-before-need" *> \x -> do
        readFile "accessed-before-need"
        need ["accessed-before-need"]
        system' ["touch", "access-before-need"]
    
    "need-without-access" *> \x -> do
        need ["dummy-file"]
        system' ["touch", "need-without-access"]
    
    want ["access-without-need", "access-before-need", "need-without-access"]
