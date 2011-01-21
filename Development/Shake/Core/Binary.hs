module Development.Shake.Core.Binary where

import Development.Shake.Core.Utilities

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as UTF8


getSizedByteString :: Get BS.ByteString
getSizedByteString = do
    n <- getWord32le
    getLazyByteString (fromIntegral n)

putSizedByteString :: BS.ByteString -> Put
putSizedByteString bs = do
    putWord32le (fromIntegral (BS.length bs))
    putLazyByteString bs

getList :: Get a -> Get [a]
getList get_elt = do
    n <- getWord32le
    genericReplicateM n get_elt

putList :: (a -> Put) -> [a] -> Put
putList put_elt xs = do
    putWord32le (fromIntegral (length xs))
    mapM_ put_elt xs

getUTF8String :: Get String
getUTF8String = fmap UTF8.decode $ getList getWord8

putUTF8String :: String -> Put
putUTF8String = putList putWord8 . UTF8.encode
