-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Util
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.Util" provides 'createSub', the building block used
-- by every subscription in "Miso.Subscription". It handles the
-- acquire-use-release lifecycle of an external resource (an event
-- listener, an animation-frame callback, etc.) using
-- 'Control.Exception.bracket' so the resource is always cleaned up when
-- the 'Miso.Types.Component' unmounts, even if an exception is thrown.
--
-- = Quick start
--
-- Use 'createSub' to build a custom subscription from any pair of
-- acquire\/release actions. The subscription sleeps between polls and
-- relies entirely on the acquire step to register whatever callbacks
-- deliver events to the sink:
--
-- @
-- import "Miso.Subscription.Util" ('createSub')
-- import "Miso.Effect" ('Sub')
-- import qualified "Miso.FFI.Internal" as FFI
--
-- -- Custom subscription: fire an action whenever the window is resized
-- resizeSub :: (Int -> action) -> 'Sub' action
-- resizeSub toAction sink = 'createSub' acquire release sink
--   where
--     acquire =
--       FFI.'Miso.FFI.Internal.windowAddEventListener' \"resize\" $ \\_ -> do
--         w <- FFI.'Miso.FFI.Internal.windowInnerWidth'
--         sink (toAction w)
--     release cb =
--       FFI.'Miso.FFI.Internal.windowRemoveEventListener' \"resize\" cb
-- @
--
-- = How it works
--
-- @'createSub' acquire release sink@ runs:
--
-- @
-- 'Control.Exception.bracket' acquire release (\\_ -> forever (threadDelay 10000_000_000))
-- @
--
-- The @forever@ loop keeps the subscription thread alive by sleeping in
-- very long increments. All actual work is done inside callbacks
-- registered during @acquire@, which call @sink@ directly. @release@ is
-- guaranteed to run (via 'Control.Exception.bracket') when the component
-- teardown kills the thread.
--
-- = See also
--
-- * "Miso.Effect" — 'Miso.Effect.Sub', 'Miso.Effect.Sink'
-- * "Miso.FFI.Internal" — 'Miso.FFI.Internal.windowAddEventListener',
--   'Miso.FFI.Internal.windowRemoveEventListener'
-- * "Miso.Subscription" — all built-in subscriptions
----------------------------------------------------------------------------
module Miso.Subscription.Util
   ( -- ** Utilities
     createSub
   ) where
----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Control.Exception (bracket)
-----------------------------------------------------------------------------
import           Miso.Effect
-----------------------------------------------------------------------------
-- | Utility function to allow resource finalization on 'Sub'.
createSub
  :: IO a
  -- ^ Acquire resource
  -> (a -> IO b)
  -- ^ Release resource
  -> Sub action
createSub acquire release = \_ ->
  bracket acquire release (\_ -> forever (threadDelay (secs 10000)))
    where
      secs :: Int -> Int
      secs = (*1000000)
----------------------------------------------------------------------------
