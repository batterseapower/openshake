{-# LANGUAGE FlexibleContexts #-}
import Development.Shake
import Development.Shake.C
import Development.Shake.System

import qualified System.FilePath as FilePath

import qualified Control.Monad.IO.Class as Trans
import qualified Data.Map as Map
import qualified Data.List as List

main :: IO ()
main = shake $ do
   "main" *> \fn -> do
       need ["App/Main.o", "Util/Regex.o"]
       system' ["ghc", "-o", fn, "-main-is", "App.Main",
           "Util/Regex.o", "App/Main.o"]
   hs_rule
   want ["main"]

hs_rule :: (CanonicalFilePath :< ntop, Namespace ntop) => Shake ntop ()
hs_rule = match ?@> \fn -> do
   let deps = Map.findWithDefault [] fn hs_deps
   need deps
   Trans.liftIO $ putStrLn $ "********* fn: " ++ show (fn, deps)
   compile_hs fn
   where
   match fn
       | ".o" `List.isSuffixOf` fn =
           Just [fn, FilePath.replaceExtension fn "hi"]
       -- The above two lines should be corrected as below, but Evan Laforge
       -- found that using the definition aboved caused a hang, which I'm testing here.
       -- | FilePath.takeExtension fn `elem` [".o", ".hi"] =
       --     Just [FilePath.replaceExtension fn "o", FilePath.replaceExtension fn "hi"]
       
       | otherwise = Nothing

compile_hs :: (CanonicalFilePath :< ntop, Namespace ntop) =>
   FilePath -> Act ntop ()
compile_hs fn = do
   need [hs_of fn]
   system' ["ghc", "-c", "-main-is", "App.Main", hs_of fn]

hs_of :: FilePath -> FilePath
hs_of fn = FilePath.replaceExtension fn "hs"

hs_deps :: Map.Map FilePath [FilePath]
hs_deps = Map.fromList
   [ ("App/Main.o", ["Util/Regex.hi"])
   ]
