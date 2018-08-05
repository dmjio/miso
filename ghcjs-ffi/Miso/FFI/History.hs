-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.History
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.History
  ( getWindowLocationHref
  , go
  , back
  , forward
  , pushState
  , replaceState
  ) where

import GHCJS.Types

foreign import javascript safe "$r = window.location.href || '';"
  getWindowLocationHref :: IO JSString

foreign import javascript unsafe "window.history.go($1);"
  go :: Int -> IO ()

foreign import javascript unsafe "window.history.back();"
  back :: IO ()

foreign import javascript unsafe "window.history.forward();"
  forward :: IO ()

foreign import javascript unsafe "window.history.pushState(null, null, $1);"
  pushState :: JSString -> IO ()

foreign import javascript unsafe "window.history.replaceState(null, null, $1);"
  replaceState :: JSString -> IO ()
