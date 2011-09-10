import Development.Shake
import Development.Shake.C
import Development.Shake.System

import System.FilePath

import Control.Monad

main :: IO ()
main = shake $ do
    "Main" *> \x -> do
        need ["Main.o", "Utility.o"]
        system' ["ghc", "-o", x,
                 "Utility.o", "Main.o"]
    (\x -> guard (takeExtension x == ".o") >> return [x, replaceExtension x "hi"]) ?@> \o -> do
        let hs = replaceExtension o "hs"
        need $ [hs] ++ [y | hs == "Main.hs", y <- ["Utility.hi", "Utility.o"]]
        system' ["ghc", "-c", hs]
    want ["Main"]
