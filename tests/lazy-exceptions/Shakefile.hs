import Development.Shake
import Development.Shake.System

import System.FilePath

import Control.Exception
import Control.Monad.IO.Class


main :: IO ()
main = shake $ do
    "foo-dependency1" *> \x -> do
        need ["impossible-file"]
    
    "foo-dependency2" *> \x -> do
        liftIO $ throwIO $ ErrorCall "User error in foo-dependency2 rule"
    
    "foo-dependency3" *> \x -> do
        system' ["touch", "foo-dependency3"]
    
    "foo" *> \x -> do
        need ["foo-dependency1", "foo-dependency2", "foo-dependency3"]
        need ["unreachable-need"]
    
    "bar" *> \x -> do
        liftIO $ throwIO $ ErrorCall "User error in bar rule"
    
    want ["foo", "bar"]
