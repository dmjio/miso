-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
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
--   { 'Miso.Types.subs'   = [ 'rAFSub' Tick ]
--   , 'Miso.Types.events' = 'Miso.Event.Types.defaultEvents'
--   }
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update (Tick t) = do
--   'Miso.State.modify' (\\m -> m { time = t })
-- @
--
-- = Lifecycle
--
-- Internally 'rAFSub' uses 'Miso.Subscription.Util.createSub':
--
-- * __Acquire__ — schedules the first @requestAnimationFrame@ callback,
--   which re-schedules itself on every invocation.
-- * __Release__ — cancels the queued frame and calls
--   'Miso.DSL.freeFunction' to release the JS reference when the 'Sub' is
--   stopped ('Miso.Runtime.stopSub', or the component unmounting). If a
--   frame callback is in flight at that moment (e.g. 'Miso.Runtime.stopSub'
--   called from the action it just dispatched), the callback does the
--   cleanup itself once it finishes, rather than re-scheduling.
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
import           Miso.DSL
import           Miso.Effect (Sub)
import           Miso.Subscription.Util (createSub)
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
-- | Where a 'rAFLoop' is in its life. See Note [rAFLoop teardown].
data FrameState
  = Idle
  -- ^ A frame is queued and no callback is running
  | Running
  -- ^ The frame callback is executing
  | Stopped
  -- ^ The 'Sub' has been released
----------------------------------------------------------------------------
-- | A self re-scheduling @requestAnimationFrame@ loop backed by a single
-- callback. @mkOnFrame@ runs once at acquire to build the per-frame handler.
rAFLoop :: IO (Double -> IO ()) -> Sub model action
rAFLoop mkOnFrame = createSub acquire release
  where
    acquire = do
      onFrame <- mkOnFrame
      stateRef <- newIORef Idle
      idRef <- newIORef (0 :: Int)
      cbRef <- newIORef (error "rAFLoop: uninitialized, impossible")
      callback <-
        syncCallback1 $ \jsval -> do
          enter <- atomicModifyIORef' stateRef $ \case
            Idle -> (Running, True)
            s    -> (s, False)
          if not enter then pure () else do
            onFrame =<< fromJSValUnchecked jsval
            cb <- readIORef cbRef
            writeIORef idRef =<< requestAnimationFrame cb
            stopped <- atomicModifyIORef' stateRef $ \case
              Stopped -> (Stopped, True)
              _       -> (Idle, False)
            if stopped
              then do
                cancelAnimationFrame =<< readIORef idRef
                freeFunction (Function cb)
              else pure ()
      writeIORef cbRef callback
      writeIORef idRef =<< requestAnimationFrame callback
      pure (stateRef, idRef, callback)

    release (stateRef, idRef, callback) = do
      wasIdle <- atomicModifyIORef' stateRef $ \case
        Idle -> (Stopped, True)
        s    -> (Stopped, False)
      if wasIdle
        then do
          cancelAnimationFrame =<< readIORef idRef
          freeFunction (Function callback)
        else pure ()
----------------------------------------------------------------------------
-- Note [rAFLoop teardown]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- 'release' runs on the 'Sub' thread (via 'killThread'), concurrently with
-- the frame callback. The callback dispatches an action, and the runtime may
-- process it -- including a 'Miso.Runtime.stopSub' of this very 'Sub' --
-- before the callback re-schedules itself. Freeing the callback there and
-- letting it re-schedule afterwards queues a frame on a freed function, and
-- the browser invoking it crashes (@getJSVal@ on the WASM backend).
--
-- So ownership of teardown is decided atomically on 'FrameState':
--
-- * 'release' in 'Idle': a frame is queued and nothing is running, so cancel
--   it and free the callback here.
-- * 'release' in 'Running': do nothing but mark 'Stopped'. The callback, on
--   its way out, sees 'Stopped', cancels the frame it just queued and frees
--   itself.
--
-- The callback writes the new frame id before leaving 'Running', so 'release'
-- only reads @idRef@ once it is current. No lock is taken: the JS backend's
-- sync callbacks throw 'WouldBlockException' if they block.
