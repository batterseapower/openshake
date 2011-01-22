{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, TypeFamilies, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, TypeOperators #-}
import Development.Shake
import qualified Development.Shake.Core as Core

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Binary
import Data.Typeable


#include "MyOracle.inc"


instance Binary (Question MyOracle) where
    get = return (MOQ ())
    put (MOQ ()) = return ()

instance Binary (Answer MyOracle) where
    get = fmap MOA get
    put (MOA i) = put i


main :: IO ()
main = (Core.shake :: Shake (Question MyOracle :+: CanonicalFilePath) () -> IO ()) $ do
    installOracle (MO 1)
    
    "examplefile" *> \x -> do
        MOA 1 <- query $ MOQ ()
        liftIO $ writeFile "examplefile" "OK1"
    
    want ["examplefile"]
