{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Shake.WaitHandle (
    WaitHandle, newWaitHandle, waitOnWaitHandle, mayWaitOnWaitHandle, awakeWaiters
  ) where

import Control.Concurrent.MVar


newtype WaitHandle = WH { unWH :: MVar () }
                   deriving (Eq)

instance Show WaitHandle where
    show (WH _) = "WaitHandle"

newWaitHandle :: IO WaitHandle
newWaitHandle = fmap WH newEmptyMVar

waitOnWaitHandle :: WaitHandle -> IO ()
waitOnWaitHandle = readMVar . unWH

-- | Looks ahead to see if the caller is likely to have to wait on the wait handle.
-- If this function returns 'True' then they may or may not actually have to wait,
-- but if the function returns 'False' then they certainly won't have to wait.
mayWaitOnWaitHandle :: WaitHandle -> IO Bool
mayWaitOnWaitHandle = isEmptyMVar . unWH

awakeWaiters :: WaitHandle -> IO ()
awakeWaiters (WH mvar) = tryPutMVar mvar () >> return ()
