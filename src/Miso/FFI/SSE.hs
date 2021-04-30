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

import           Miso.FFI (JSM)
import qualified Miso.FFI as FFI
import           Miso.String

import qualified Language.Javascript.JSaddle as JSaddle
import           Language.Javascript.JSaddle hiding (new)

newtype EventSource = EventSource JSVal

data' :: JSVal -> JSM JSVal
data' v = v ! ("data" :: JSString)

new :: MisoString -> JSM EventSource
new url = EventSource <$> JSaddle.new (jsg ("EventSource" :: JSString)) [url]

addEventListener :: EventSource -> MisoString -> (JSVal -> JSM ()) -> JSM ()
addEventListener (EventSource s) = FFI.addEventListener s
