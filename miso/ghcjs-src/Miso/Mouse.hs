{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Mouse where

import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal
import Miso.FFI
import Miso.Signal
import Miso.Types

data Mouse = Mouse {
    mouse        :: Signal (Int,Int)
  , stopMouse    :: IO ()
  , startMouse   :: IO ()
  }

mouseSignal :: IO Mouse
mouseSignal = do
  (mouse, sink) <- signal
  cb <- asyncCallback1 $ \mouseEvent -> do
    Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
    Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
    sink (x,y)
  let startMouse = windowAddEventListener "mousemove" cb
      stopMouse = windowRemoveEventListener "mousemove" cb
  startMouse
  pure Mouse {..}

