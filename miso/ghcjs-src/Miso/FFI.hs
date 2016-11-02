module Miso.FFI where

import Data.JSString
import GHCJS.Foreign.Callback
import GHCJS.Types

foreign import javascript unsafe "window.addEventListener($1, $2);"
  addEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()
{-# INLINE addEventListener #-}
