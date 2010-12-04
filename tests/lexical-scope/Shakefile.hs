import Development.Shake


main :: IO ()
main = shake $ do
    stringOracle (const $ return ["foo"]) $
      "examplefile" *> \x -> do
          ["foo"] <- queryStringOracle ("silly", "question")
          return ()
    stringOracle (const $ return ["bar"]) $ 
        want ["examplefile"]
