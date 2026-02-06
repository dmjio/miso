-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Concurrent
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Concurrent
  ( -- ** Synchronization primitives
    Waiter (..)
  , waiter
  , oneshot
  ) where
-----------------------------------------------------------------------------
import Control.Concurrent
-----------------------------------------------------------------------------
-- | Synchronization primitive for event loop
data Waiter
  = Waiter
  { wait :: IO ()
    -- ^ Blocks on MVar
  , notify :: IO ()
    -- ^ Unblocks threads waiting on MVar
  }
-----------------------------------------------------------------------------
-- | Creates a new @Waiter@
--
-- Useful for multiple threads to wake-up / notify a single thread running in an
-- infinite loop, waiting for work (e.g. to process an event queue).
--
waiter :: IO Waiter
waiter = do
  mvar <- newEmptyMVar
  pure Waiter
    { wait = takeMVar mvar
    , notify = do
        _ <- tryPutMVar mvar ()
        pure ()
    }
-----------------------------------------------------------------------------
-- | Creates a new @Waiter@
--
-- Useful for a single thread to wake-up multiple threads that are waiting 
-- to run a oneshot task (e.g. like forking a thread).
--
oneshot :: IO Waiter
oneshot = do
  mvar <- newEmptyMVar
  pure Waiter
    { wait = readMVar mvar
    , notify = putMVar mvar ()
    }
-----------------------------------------------------------------------------
