{-# LANGUAGE TypeOperators #-}
import Development.Shake
import qualified Development.Shake.Core as Core
import Development.Shake.Oracles.String

import Control.Monad.IO.Class


main :: IO ()
main = (Core.shake :: Shake (Question StringOracle :+: CanonicalFilePath) () -> IO ()) $ do
    installOracle (StringOracle (const $ return ["foo"])) `privateTo_` do
        "examplefile" *> \x -> do
              ["foo"] <- queryStringOracle ("silly", "question")
              liftIO $ writeFile "examplefile" "Dummy contents"
    
    installOracle (StringOracle (const $ return ["bar"])) `privateTo_` want ["examplefile"]
