{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Mouse
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Mouse (mouseSub) where

import Control.Monad.IO.Class

import Miso.Types (Sub)
import Miso.FFI
import Miso.String

default (MisoString)

-- | Captures mouse coordinates as they occur and writes them to
-- an event sink
mouseSub :: ((Int,Int) -> action) -> Sub action
mouseSub f = \sink -> do
  windowAddEventListener "mousemove" =<< do
    asyncCallback1 $ \mouseEvent -> do
      Just x <- fromJSVal =<< getProp "clientX" (JSObject mouseEvent)
      Just y <- fromJSVal =<< getProp "clientY" (JSObject mouseEvent)
      liftIO (sink $ f (x,y))
