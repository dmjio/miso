-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect.DOM
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
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
