{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Development.Shake.Oracles.FileSystem (
    -- * Oracle type
    FileSystemOracle,
    
    -- * Oracle operations
    ls
  ) where

import Development.Shake.Core
import Development.Shake.Core.Binary
import Development.Shake.Composition
import Development.Shake.Oracles.Common

import Data.Binary

import Control.DeepSeq

import System.Directory
import System.FilePath.Glob


data FileSystemOracle = FSO

instance Oracle FileSystemOracle where
    newtype Question FileSystemOracle = Ls String
                                      deriving (Eq, Ord, Show, NFData)
    newtype Answer FileSystemOracle = LsAnswer [FilePath]
                                    deriving (Eq, Show, NFData)
    
    queryOracle FSO (Ls pattern) = fmap LsAnswer $ getCurrentDirectory >>= globDir1 (compile pattern)
    
    defaultOracle = Just FSO


instance Binary (Question FileSystemOracle) where
    get = fmap Ls getUTF8String
    put (Ls x) = putUTF8String x

instance Binary (Answer FileSystemOracle) where
    get = fmap LsAnswer $ getList getUTF8String
    put (LsAnswer xs) = putList putUTF8String xs


ls :: (Question FileSystemOracle :< ntop, Namespace ntop) => String -> Act ntop [FilePath]
ls pattern = fmap (\(LsAnswer fps) -> fps) $ query (Ls pattern)

