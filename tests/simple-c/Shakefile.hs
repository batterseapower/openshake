import Development.Shake
import Development.Shake.C
import Development.Shake.System

import System.FilePath


main :: IO ()
main = shake $ do
    "Main" *> \x -> do
        cs <- ls "*.c"
        let os = map (`replaceExtension` "o") cs
        need os
        system' $ ["gcc","-o",x] ++ os
    "*.o" *> \x -> do
        let c = replaceExtension x "c"
        need =<< cppIncludes c
        system' ["gcc","-c",c,"-o",x]
    want ["Main"]
