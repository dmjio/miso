{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Window where

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

data Window = Window {
    window :: SignalGen (Signal [(Int,Int)])
  , windowRunning :: IO Bool
  , stopWindow :: IO ()
  , startWindow :: IO ()
  }

windowSignal :: IO Window
windowSignal = do
  ref <- newIORef True
  let stopWindow = writeIORef ref False
      startWindow = writeIORef ref True
      windowRunning = readIORef ref
  (window, sink) <- signal
  cb <- asyncCallback1 $ \windowEvent -> do
    target <- getProp "target" (Object windowEvent)
    Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
    Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
    result <- readIORef ref
    when result $ sink (w,h)
  addEventListener "resize" cb
  pure Window {..}

