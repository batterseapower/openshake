{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Shake.Core.WaitHandle (
    WaitHandle, newWaitHandle, waitOnWaitHandle, mayWaitOnWaitHandle, awakeWaiters
  ) where

import Control.Concurrent.MVar


-- | A 'WaitHandle' is basically just an 'MVar' that can only be put into once, and
-- then never gets anything removed from it
newtype WaitHandle a = WH { unWH :: MVar a }
                     deriving (Eq)

instance Show (WaitHandle a) where
    show (WH _) = "WaitHandle"

newWaitHandle :: IO (WaitHandle a)
newWaitHandle = fmap WH newEmptyMVar

waitOnWaitHandle :: WaitHandle a -> IO a
waitOnWaitHandle = readMVar . unWH

-- | Looks ahead to see if the caller is likely to have to wait on the wait handle.
-- If this function returns 'True' then they may or may not actually have to wait,
-- but if the function returns 'False' then they certainly won't have to wait.
mayWaitOnWaitHandle :: WaitHandle a -> IO Bool
mayWaitOnWaitHandle = isEmptyMVar . unWH

awakeWaiters :: WaitHandle a -> a -> IO ()
awakeWaiters (WH mvar) x = tryPutMVar mvar x >> return ()
