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

instance Oracle o => Namespace (Question o) where
    type Entry (Question o) = Answer o

    sanityCheck _ _ = return Nothing -- No way to sanity check oracle question without running it
    defaultRule _ = return Nothing -- No default way to answer oracle questions


installOracle :: (Oracle o, Question o :< ntop) => o -> Shake ntop ()
installOracle o = addRule $ liftRule $ \q -> return $ Just ([q], fmap return $ liftIO $ queryOracle o q)
