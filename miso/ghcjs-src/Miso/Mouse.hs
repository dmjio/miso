{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Mouse where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.JSString
import FRP.Elerea.Simple
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.Object
import JavaScript.Object.Internal
import Miso.FFI
import Miso.Signal

data Mouse = Mouse {
    mouse :: SignalGen (Signal [(Int,Int)])
  , mouseRunning :: IO Bool
  , stopMouse :: IO ()
  , startMouse :: IO ()
  }

mouseSignal :: IO Mouse
mouseSignal = do
  ref <- newIORef True
  let stopMouse = writeIORef ref False
      startMouse = writeIORef ref True
      mouseRunning = readIORef ref
  (mouse, sink) <- signal
  cb <- asyncCallback1 $ \mouseEvent -> do
    Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
    Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
    result <- readIORef ref
    when result $ sink (x,y)
  addEventListener "mousemove" cb
  pure Mouse {..}

