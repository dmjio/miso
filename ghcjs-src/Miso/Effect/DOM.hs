{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect.DOM
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect.DOM
  ( focus
  , blur
  , alert
  ) where

import Miso.String

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
foreign import javascript unsafe "callFocus($1);"
  focus :: MisoString -> IO ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
foreign import javascript unsafe "callBlur($1);"
  blur :: MisoString -> IO ()

-- | Calls the @alert()@ function.
foreign import javascript unsafe "alert($1);"
  alert :: MisoString -> IO ()
