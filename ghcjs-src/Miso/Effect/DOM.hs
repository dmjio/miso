module Miso.Effect.DOM
  ( focus
  , blur
  ) where

import Miso.String

foreign import javascript unsafe "document.getElementById($1).focus();"
  focus :: MisoString -> IO ()

foreign import javascript unsafe "document.getElementById($1).blur();"
  blur :: MisoString -> IO ()
