{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Subscription.Window where

import GHCJS.Foreign.Callback
import GHCJS.Marshal

import JavaScript.Object
import JavaScript.Object.Internal
import Miso.FFI
import Miso.Html.Internal ( Sub )

windowSub :: ((Int, Int) -> action) -> Sub action
windowSub f = \sink ->
  windowAddEventListener "resize" =<< do
    asyncCallback1 $ \windowEvent -> do
      target <- getProp "target" (Object windowEvent)
      Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
      Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
      sink $ f (w,h)

