{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Development.Shake.Core.WaitHandle (
    WaitHandle, newWaitHandle, waitOnWaitHandle, mayWaitOnWaitHandle
  ) where

import Control.Concurrent.MVar

import Unsafe.Coerce (unsafeCoerce)


-- | A 'WaitHandle' is basically just an 'MVar' that can only be put into once, and
-- then never gets anything removed from it
data WaitHandle a = forall b. WH (b -> a) (MVar b)

instance Eq (WaitHandle a) where
    WH _ mvar1 == WH _ mvar2 = mvar1 == unsafeCoerce mvar2

instance Show (WaitHandle a) where
    show (WH _ _) = "WaitHandle"

instance Functor WaitHandle where
    fmap f (WH g mvar) = WH (f . g) mvar

newWaitHandle :: IO (WaitHandle a, a -> IO ())
newWaitHandle = fmap (\mvar -> (WH id mvar, \x -> tryPutMVar mvar x >> return ())) newEmptyMVar

waitOnWaitHandle :: WaitHandle a -> IO a
waitOnWaitHandle (WH f mvar) = fmap f $ readMVar mvar

-- | Looks ahead to see if the caller is likely to have to wait on the wait handle.
-- If this function returns 'True' then they may or may not actually have to wait,
-- but if the function returns 'False' then they certainly won't have to wait.
mayWaitOnWaitHandle :: WaitHandle a -> IO Bool
mayWaitOnWaitHandle (WH _ mvar) = isEmptyMVar mvar
