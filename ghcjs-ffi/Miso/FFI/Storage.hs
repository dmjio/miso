-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.Storage
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.Storage
  ( Storage
  , localStorage
  , sessionStorage
  , getItem
  , removeItem
  , setItem
  , length
  , clear
  ) where

import GHCJS.Types
import Prelude hiding (length)

newtype Storage = Storage JSVal

foreign import javascript unsafe "$r = window.localStorage"
  localStorage :: IO Storage

foreign import javascript unsafe "$r = window.sessionStorage"
  sessionStorage :: IO Storage

foreign import javascript unsafe "$r = $1.getItem($2);"
  getItem :: Storage -> JSString -> IO JSVal

foreign import javascript unsafe "$1.removeItem($2);"
  removeItem :: Storage -> JSString -> IO ()

foreign import javascript unsafe "$1.setItem($2, $3);"
  setItem :: Storage -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$r = $1.length;"
  length :: Storage -> IO Int

foreign import javascript unsafe "$1.clear();"
  clear :: Storage -> IO ()
