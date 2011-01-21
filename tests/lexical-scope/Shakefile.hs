import Development.Shake

import Control.Monad.IO.Class


main :: IO ()
main = shake $ do
    stringOracle (const $ return ["foo"]) $
      "examplefile" *> \x -> do
          ["foo"] <- queryStringOracle ("silly", "question")
          liftIO $ writeFile "examplefile" "Dummy contents"
    stringOracle (const $ return ["bar"]) $ 
        want ["examplefile"]
