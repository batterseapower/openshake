import Development.Shake


main :: IO ()
main = shake $ do
    oracle (const ["foo"]) $
      "examplefile" *> \x -> do
          ["foo"] <- query ("silly", "question")
          return ()
    oracle (const ["bar"]) $ 
        want ["examplefile"]
