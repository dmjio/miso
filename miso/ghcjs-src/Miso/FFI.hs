module Miso.FFI where

import Data.JSString
import GHCJS.Foreign.Callback
import GHCJS.Types

foreign import javascript unsafe "window.addEventListener($1, $2);"
  windowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "window.removeEventListener($1, $2);"
  windowRemoveEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

foreign import javascript unsafe "console.log($1);"
  consoleLog :: JSVal -> IO ()
