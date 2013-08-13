{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Development.Shake.C(
    cppIncludes
  ) where

import Development.Shake
import Development.Shake.System

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.FilePath

cppIncludes :: (CanonicalFilePath :< n, Namespace n)
            => FilePath -> Act n [FilePath]
cppIncludes fp = fmap (map (takeDirectory fp </>) . mapMaybe takeInclude) $ readFileLines fp
  where
    -- TODO: should probably do better than this quick and dirty hack
    -- FIXME: transitive dependencies
    trim p = dropWhile p . reverse . dropWhile p . reverse
    takeInclude xs = guard ("#include" `isPrefixOf` map toLower xs) >> stripQuotes (trim isSpace (drop (length "#include") xs))
    stripQuotes ('\"':xs) = guard (not (null xs) && last xs == '\"') >> return (init xs)
    stripQuotes _ = Nothing
