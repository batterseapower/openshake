import Development.Shake


main :: IO ()
main = shake $ do
    "a" *> \x -> do
        need ["b"]
    "b" *> \x -> do
        need ["a"]
    want ["a"]
