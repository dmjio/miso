{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.SSE
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.SSE
  ( EventSource(..)
  , data'
  , new
  , addEventListener
  ) where

import           GHCJS.Types

import qualified Miso.FFI as FFI

newtype EventSource = EventSource JSVal

foreign import javascript unsafe "$r = $1.data;"
  data' :: JSVal -> IO JSVal

foreign import javascript unsafe "$r = new EventSource($1);"
  new :: JSString -> IO EventSource

addEventListener :: EventSource -> JSString -> (JSVal -> IO ()) -> IO ()
addEventListener (EventSource s) = FFI.addEventListener s
