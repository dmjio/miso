{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Window where

import GHCJS.Foreign.Callback
import GHCJS.Marshal

import JavaScript.Object
import JavaScript.Object.Internal
import Miso.FFI
import Miso.Signal
import Miso.Types

data Window = Window {
    window :: Signal (Int,Int)
  , stopWindow :: IO ()
  , startWindow :: IO ()
  }

windowSignal :: IO Window
windowSignal = do
  (window, sink) <- signal
  cb <- asyncCallback1 $ \windowEvent -> do
    target <- getProp "target" (Object windowEvent)
    Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
    Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
    sink (w,h)
  let startWindow = windowAddEventListener "resize" cb
      stopWindow = windowRemoveEventListener "resize" cb
  startWindow
  pure Window {..}

