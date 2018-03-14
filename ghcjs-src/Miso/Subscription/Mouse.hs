{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Mouse
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Mouse (mouseSub) where

import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

import Miso.FFI
import Miso.Html.Internal ( Sub )

-- | Captures mouse coordinates as they occur and writes them to
-- an event sink
mouseSub :: ((Int,Int) -> action) -> Sub action
mouseSub f = \sink -> do
  windowAddEventListener "mousemove" =<< do
    asyncCallback1 $ \mouseEvent -> do
      Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
      Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
      sink $ f (x,y)
