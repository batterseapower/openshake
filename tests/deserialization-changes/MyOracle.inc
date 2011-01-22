newtype MyOracle = MO { unMO :: Int }
                 deriving (Typeable)

instance Oracle MyOracle where
    newtype Question MyOracle = MOQ { unMOQ :: () }
                              deriving (Eq, Ord, Show, NFData)
    newtype Answer MyOracle = MOA { unOA :: Int }
                            deriving (Eq, Show, NFData)
    queryOracle (MO o) (MOQ ()) = return (MOA o)
