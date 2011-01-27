import Development.Shake
import Development.Shake.System

import System.FilePath


main :: IO ()
main = shake $ do
    ("subdirectory" </> "foo") *> \x -> do
        system' $ ["touch",x]
    want ["subdirectory/foo"]
