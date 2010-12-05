{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, TypeFamilies, FlexibleInstances, FlexibleContexts, DeriveDataTypeable #-}
import Development.Shake

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
main = shake $ do
    oracle (MO 1) $
      "examplefile" *> \x -> do
          MOA 1 <- query $ MOQ ()
          liftIO $ writeFile "examplefile" "OK1"
    want ["examplefile"]
