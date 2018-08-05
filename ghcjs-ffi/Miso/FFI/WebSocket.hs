{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.WebSocket
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.WebSocket
  ( Socket(..)
  , create
  , socketState
  , send
  , addEventListener
  , data'
  , wasClean
  , code
  , reason
  ) where

import           GHCJS.Types

import qualified Miso.FFI as FFI
import           Miso.WebSocket

newtype Socket = Socket JSVal

foreign import javascript unsafe "$r = new WebSocket($1, $2);"
  create :: JSString -> JSVal -> IO Socket

foreign import javascript unsafe "$r = $1.readyState;"
  socketState :: Socket -> IO Int

foreign import javascript unsafe "$1.send($2);"
  send :: Socket -> JSString -> IO ()

addEventListener :: Socket -> JSString -> (JSVal -> IO ()) -> IO ()
addEventListener (Socket s) = FFI.addEventListener s

foreign import javascript unsafe "$r = $1.data"
  data' :: JSVal -> IO JSVal

foreign import javascript unsafe "$r = $1.wasClean"
  wasClean :: JSVal -> IO WasClean

foreign import javascript unsafe "$r = $1.code"
  code :: JSVal -> IO Int

foreign import javascript unsafe "$r = $1.reason"
  reason :: JSVal -> IO Reason
