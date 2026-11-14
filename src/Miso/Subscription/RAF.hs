-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.RAF
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.RAF" provides 'rAFSub', a subscription that hooks
-- into the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame requestAnimationFrame>
-- loop. On each frame the browser calls back with a
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp DOMHighResTimeStamp>
-- (milliseconds since page load, sub-millisecond precision), which is
-- forwarded to the component as an action.
--
-- This is the recommended driver for canvas-based animations and games
-- because the browser throttles the callback to the display refresh rate
-- (typically 60 fps) and pauses it automatically when the tab is hidden.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Subscription.RAF"
-- import "Miso.Canvas"
--
-- data Action = Tick Double   -- DOMHighResTimeStamp in ms
--
-- myComponent = ('Miso.component' model update view)
--   { 'Miso.Types.subs' = [ 'rAFSub' Tick ] }
--
-- update :: Action -> t'Miso.Effect.Effect' p props Model Action
-- update (Tick t) = do
--   'Miso.State.modify' (\\m -> m { time = t })
-- @
--
-- = Lifecycle
--
-- Internally 'rAFSub' uses 'Miso.Subscription.Util.rAFLoop' (built on
-- 'Miso.Subscription.Util.createSub'):
--
-- * __Acquire__ — schedules the first @requestAnimationFrame@ callback,
--   which re-schedules itself on every invocation.
-- * __Release__ — when the 'Sub' is stopped ('Miso.stopSub', or
--   the component unmounting), the callback is told to stop; on its next
--   frame it frees its JS reference instead of re-scheduling. See
--   @Note [rAFLoop teardown]@ in "Miso.Subscription.Util".
--
-- = See also
--
-- * "Miso.Canvas" — canvas drawing API driven by 'rAFSub' ticks
-- * "Miso.Subscription.Util" — 'Miso.Subscription.Util.createSub'
-- * "Miso.Subscription" — re-export hub
----------------------------------------------------------------------------
module Miso.Subscription.RAF
  ( rAFSub
  , rAFSubElapsed
  ) where
----------------------------------------------------------------------------
import           Data.IORef
----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import           Miso.Subscription.Util (rAFLoop)
----------------------------------------------------------------------------
-- | A 'Sub' for 60FPS animations when using 'requestAnimationFrame'.
--
-- The 'Double' returned is a [DOMHighResTimeStamp](https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp) expressed in milliseconds.
--
rAFSub
  :: (Double -> action)
  -- ^ Callback fired each frame with a @DOMHighResTimeStamp@ in milliseconds
  -> Sub model action
rAFSub toAction sink = rAFLoop (pure (sink . toAction)) sink
----------------------------------------------------------------------------
-- | Like 'rAFSub' but fires @action@ at most once per @interval@ milliseconds.
--
-- Elapsed time is accumulated in 'IORef's inside the subscription so the
-- model is not touched between ticks — Miso only re-renders when a tick
-- actually fires, rather than on every animation frame.
--
-- @
-- app = defaultApp model update view
--   { subs = [ rAFSubElapsed 175 Tick ] }
-- @
--
rAFSubElapsed
  :: Double
  -- ^ Minimum interval between ticks in milliseconds (e.g. @175@ for ~6 fps)
  -> action
  -- ^ Action to dispatch each time the interval elapses
  -> Sub model action
rAFSubElapsed interval action sink = flip rAFLoop sink $ do
  lastRef <- newIORef (0 :: Double)
  elapRef <- newIORef (0 :: Double)
  pure $ \t -> do
    lastT <- readIORef lastRef
    elap <- readIORef elapRef
    let dt      = if lastT == 0 then 0 else min interval (t - lastT)
        newElap = elap + dt
    writeIORef lastRef t
    if newElap >= interval
      then writeIORef elapRef (newElap - interval) *> sink action
      else writeIORef elapRef newElap
----------------------------------------------------------------------------
