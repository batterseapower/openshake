{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Development.Shake.Oracles.Common (
    -- * Defining oracles
    Oracle(..),
    
    -- * Installing oracles
    installOracle
  ) where

import Development.Shake.Core
import Development.Shake.Composition

import Data.Binary

import Control.DeepSeq

import Control.Monad.IO.Class


class (Ord (Question o), Eq (Answer o),
       Show (Question o), Show (Answer o),
       Binary (Question o), Binary (Answer o),
       NFData (Question o), NFData (Answer o)) => Oracle o where
    data Question o
    data Answer o
    
    queryOracle :: o -> Question o -> IO (Answer o)
    
    -- | The oracle that will be used if no other oracle of the right type gets explicitly installed
    defaultOracle :: Maybe o
    defaultOracle = Nothing

instance Oracle o => Namespace (Question o) where
    type Entry (Question o) = Answer o

    sanityCheck _ _ = return Nothing -- No way to sanity check oracle question without running it
    
    defaultRule q = case defaultOracle of
        Nothing -> return Nothing
        Just o  -> liftIO $ oracleRule o q
    
    data Snapshot (Question o) = OracleSnapshot -- Nothing to sanity check: how could we tell if user code had used the result of a query?
    
    takeSnapshot = return OracleSnapshot
    lintSnapshots _ _ = []


oracleRule :: Oracle o => o -> Question o -> IO (Maybe ([Question o], Act ntop [Answer o]))
oracleRule o q = return $ Just ([q], fmap return $ liftIO $ queryOracle o q)

installOracle :: (Oracle o, Question o :< ntop) => o -> Shake ntop ()
installOracle o = addRule . liftRule $ oracleRule o
