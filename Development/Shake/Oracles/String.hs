{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Development.Shake.Oracles.String (
    -- * Oracle type
    StringOracle(..),
    
    -- * Oracle operations
    queryStringOracle
  ) where

import Development.Shake.Core
import Development.Shake.Core.Binary
import Development.Shake.Composition
import Development.Shake.Oracles.Common

import Data.Binary

import Control.DeepSeq

import Control.Monad


newtype StringOracle = StringOracle ((String, String) -> IO [String])

instance Oracle StringOracle where
    newtype Question StringOracle = SQ { unSQ :: (String, String) }
                                  deriving (Eq, Ord, Show, NFData)
    newtype Answer StringOracle = SA { unSA :: [String] }
                                deriving (Eq, Show, NFData)
    queryOracle (StringOracle f) = fmap SA . f . unSQ

instance Binary (Question StringOracle) where
    get = fmap SQ $ liftM2 (,) getUTF8String getUTF8String
    put (SQ (x, y)) = putUTF8String x >> putUTF8String y

instance Binary (Answer StringOracle) where
    get = fmap SA $ getList getUTF8String
    put = putList putUTF8String . unSA


queryStringOracle :: (Question StringOracle :< ntop, Namespace ntop) => (String, String) -> Act ntop [String]
queryStringOracle = fmap unSA . query . SQ
