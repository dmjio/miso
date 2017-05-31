module Miso.Effect.DOM
  ( focus
  , blur
  ) where

import Miso.String

-- | Fails silently if no element found
-- Analgous to `document.getElementById(id).focus()`
foreign import javascript unsafe "callFocus($1);"
  focus :: MisoString -> IO ()

-- | Fails silently if no element found
-- Analgous to `document.getElementById(id).blur()`
foreign import javascript unsafe "callBlur($1);"
  blur :: MisoString -> IO ()
