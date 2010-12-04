import Development.Shake


main :: IO ()
main = shake $ do
    "a" *> \x -> do
        need ["b"]
    "b" *> \x -> do
        need ["a"]
    -- Because we "want" the two files simultaneously, they will each get
    -- built on a different thread. This causes the current (simpleminded) cycle
    -- detector to not detect a cycle.
    want ["a", "b"]
