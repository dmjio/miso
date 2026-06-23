-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Concurrent
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Concurrent" provides 'Waiter', a lightweight synchronization
-- primitive built on 'Control.Concurrent.MVar.MVar' that the miso runtime
-- uses to coordinate its event loop with subscription threads.
--
-- Two constructors are available with different wakeup semantics:
--
-- * 'waiter' — __many-to-one__. Multiple threads may call 'notify'
--   concurrently; only one pending notification is retained at a time
--   ('Control.Concurrent.MVar.tryPutMVar' is used, so rapid notifications
--   coalesce). The single consumer calls 'wait' to block until at least one
--   notification arrives.
--
-- * 'oneshot' — __one-to-many__. One thread calls 'notify' to permanently
--   unblock /all/ threads currently (or subsequently) calling 'wait'.
--   Implemented with 'Control.Concurrent.MVar.readMVar', so the stored value
--   is never consumed.
--
-- = See also
--
-- * "Miso.Effect" — 'Miso.Effect.Sub' subscriptions that use 'notify' to
--   wake the event loop
-- * "Miso.Runtime" — the event loop that calls 'wait'
-----------------------------------------------------------------------------
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
