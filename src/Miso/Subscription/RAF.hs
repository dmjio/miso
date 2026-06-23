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
-- A `Sub` for `requestAnimationFrame`. Meant to be used in Canvas based
-- animations / games to achieve 60fps.
--
-- @
-- main :: IO ()
-- main = startApp defaultEvents comp { subs = [ rAFSub Tick ] }
--
-- data Action = Tick Double
-- @
--
----------------------------------------------------------------------------
module Miso.Subscription.RAF
  ( rAFSub
  , rAFSubElapsed
  ) where
----------------------------------------------------------------------------
import           Control.Monad (void)
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
rAFSub :: (Double -> action) -> Sub action
rAFSub toAction sink = createSub acquire release sink
  where
    acquire = do
      ref <- newIORef (error "rAFSub: uninitialized, impossible")
      callback <-
        syncCallback1 $ \jsval -> do
          sink . toAction =<< fromJSValUnchecked jsval
          void (requestAnimationFrame =<< readIORef ref)

      writeIORef ref callback
      void (requestAnimationFrame callback)
      pure callback

    release callback = freeFunction (Function callback)
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
rAFSubElapsed :: Double -> action -> Sub action
rAFSubElapsed interval action sink = createSub acquire release sink
  where
    acquire = do
      cbRef   <- newIORef (error "rAFSubElapsed: uninitialized, impossible")
      lastRef <- newIORef (0.0 :: Double)
      elapRef <- newIORef (0.0 :: Double)
      callback <- syncCallback1 $ \jsval -> do
        t    <- fromJSValUnchecked jsval
        prev <- readIORef lastRef
        writeIORef lastRef t
        let dt = if prev == 0 then 0 else min interval (t - prev)
        elap <- readIORef elapRef
        let newElap = elap + dt
        if newElap >= interval
          then do
            writeIORef elapRef (newElap - interval)
            sink action
          else writeIORef elapRef newElap
        void (requestAnimationFrame =<< readIORef cbRef)
      writeIORef cbRef callback
      void (requestAnimationFrame callback)
      pure callback

    release callback = freeFunction (Function callback)
----------------------------------------------------------------------------
