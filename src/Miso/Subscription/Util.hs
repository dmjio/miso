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
-- the t'Miso.Types.Component' unmounts, even if an exception is thrown.
--
-- It also provides 'rAFLoop', a @requestAnimationFrame@ loop built on
-- 'createSub' whose callback re-schedules itself every frame. It is the
-- basis of 'Miso.Subscription.RAF.rAFSub',
-- 'Miso.Subscription.RAF.rAFSubElapsed' and
-- 'Miso.Subscription.Canvas.canvasSub'. See Note [rAFLoop teardown].
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
-- 'Control.Exception.bracket' acquire release (\\_ -> forever (threadDelay 1000_000_000))
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
   , rAFLoop
   ) where
----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, void)
import           Control.Exception (bracket)
import           Data.IORef
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Effect
-----------------------------------------------------------------------------
-- | Utility function to allow resource finalization on 'Sub'.
createSub
  :: IO a
  -- ^ Acquire resource
  -> (a -> IO b)
  -- ^ Release resource
  -> Sub model action
createSub acquire release = \_ _ ->
  bracket acquire release (\_ -> forever (threadDelay (secs 1000)))
    where
      -- 'Int' is 32 bits on wasm32 (GHC and MicroHs), so the delay must stay
      -- below 2^31 microseconds (about 2147 seconds); MicroHs throws
      -- 'Overflow' on a larger product, which would kill the 'Sub' thread and
      -- release the resource.
      secs :: Int -> Int
      secs = (*1000000)
----------------------------------------------------------------------------
-- | A self re-scheduling @requestAnimationFrame@ loop, as a 'Sub'.
--
-- The 'IO' action runs once, when the 'Sub' starts, and builds the
-- per-frame handler; it is the place to acquire anything the handler
-- needs (a canvas context, 'Data.IORef.IORef's for timing state, ...).
-- The handler then receives the frame's @DOMHighResTimeStamp@ in
-- milliseconds, every frame, until the 'Sub' is stopped.
--
-- A single callback is used for the whole loop; it is freed when the
-- 'Sub' stops. See Note [rAFLoop teardown].
--
-- @since 1.15.0.0
rAFLoop
  :: IO (Double -> IO ())
  -- ^ Builds the per-frame handler; runs once at start
  -> Sub model action
rAFLoop mkOnFrame = createSub acquire release
  where
    acquire = do
      onFrame <- mkOnFrame
      stopRef <- newIORef False
      cbRef <- newIORef (error "rAFLoop: uninitialized, impossible")
      callback <-
        syncCallback1 $ \jsval -> do
          stopped <- readIORef stopRef
          if stopped
            then freeFunction . Function =<< readIORef cbRef
            else do
              onFrame =<< fromJSValUnchecked jsval
              void . requestAnimationFrame =<< readIORef cbRef
      writeIORef cbRef callback
      void (requestAnimationFrame callback)
      pure stopRef
    release stopRef = writeIORef stopRef True
----------------------------------------------------------------------------
-- Note [rAFLoop teardown]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- The frame callback owns its own lifetime: it is the only thing that ever
-- frees itself. 'release' just sets a flag, and the next frame the callback
-- sees it, frees itself and does not re-schedule. So a stopped loop runs
-- one more (silent) frame before the callback is gone.
--
-- Why not cancel the queued frame and free the callback in 'release'? The
-- callback and 'release' run on different Haskell threads, and the wasm RTS
-- has no timer, so it context-switches at every heap-block boundary
-- whenever another thread is runnable (-C0; see Note [No timer on wasm32]
-- in GHC's rts). Sinking an action wakes the scheduler thread, so the
-- callback can be descheduled right after it, and if the action stops this
-- 'Sub' then 'release' frees the callback before the callback resumes and
-- calls @requestAnimationFrame@ on its own, now freed, handle: the JSFFI
-- glue throws (@getJSVal@) and the runtime is dead. Any scheme where
-- 'release' frees needs the two threads to agree on who frees, i.e. a lock.
-- With one owner there is nothing to agree on: the flag is checked at the
-- start of every invocation, and a rAF callback never runs concurrently
-- with itself, so no interleaving reaches a freed handle.
--
-- Freeing the callback from inside itself is fine: the trampoline has
-- already dereferenced the Haskell closure by the time the body runs, and
-- the body does nothing with the handle afterwards.
----------------------------------------------------------------------------
