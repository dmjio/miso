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
module Miso.Subscription.RAF where
----------------------------------------------------------------------------
import           Control.Monad (void)
import           Data.IORef
----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Effect (Sub)
import           Miso.Subscription.Util (createSub)
----------------------------------------------------------------------------
-- | A 'Sub' for 60FPS animations when using 'requestForAnimationFrame'.
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
          sink =<< toAction <$> fromJSValUnchecked jsval
          void (requestAnimationFrame =<< readIORef ref)

      writeIORef ref callback
      void (requestAnimationFrame callback)
      pure callback

    release callback = freeFunction (Function callback)
----------------------------------------------------------------------------
