{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Subscription.Mouse (mouseSub) where

import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

import Miso.Types
import Miso.FFI

mouseSub :: (Coords -> Action m) -> Sub m
mouseSub f = \sink -> do
  windowAddEventListener "mousemove" =<< do
    asyncCallback1 $ \mouseEvent -> do
      Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
      Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
      sink $ f (x,y)

