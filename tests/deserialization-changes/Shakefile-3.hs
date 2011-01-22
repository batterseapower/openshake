{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, TypeFamilies, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, TypeOperators #-}
import Development.Shake
import qualified Development.Shake.Core as Core

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Typeable


#include "MyOracle.inc"


-- Make the question longer (tests that we check that question deserialization is tested)
instance Binary (Question MyOracle) where
    get = do { 0 <- getWord8; return (MOQ ()) }
    put (MOQ ()) = putWord8 0

instance Binary (Answer MyOracle) where
    get = fmap (MOA . fromIntegral) getWord16le
    put (MOA i) = putWord16le (fromIntegral i)


main :: IO ()
main = (Core.shake :: Shake (Question MyOracle :+: CanonicalFilePath) () -> IO ()) $ do
    installOracle (MO 1)
    
    "examplefile" *> \x -> do
        MOA 1 <- query $ MOQ ()
        liftIO $ writeFile "examplefile" "OK3"

    want ["examplefile"]
