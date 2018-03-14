{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.SSE
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.SSE
 ( -- * Subscription
   sseSub
   -- * Types
 , SSE (..)
 ) where

import Data.Aeson
import GHCJS.Foreign.Callback
import GHCJS.Types
import Miso.FFI
import Miso.Html.Internal     ( Sub )
import Miso.String

-- | Server-sent events Subscription
sseSub :: FromJSON msg => MisoString -> (SSE msg -> action) -> Sub action
sseSub url f = \sink -> do
  es <- newEventSource url
  onMessage es =<< do
    asyncCallback1 $ \val -> do
      getData val >>= parse >>= \x -> do
        sink $ f (SSEMessage x)
  onError es =<< do
    asyncCallback $
      sink (f SSEError)
  onClose es =<< do
    asyncCallback $
      sink (f SSEClose)

-- | Server-sent events data
data SSE message
  = SSEMessage message
  | SSEClose
  | SSEError
  deriving (Show, Eq)

foreign import javascript unsafe "$r = $1.data;"
  getData :: JSVal -> IO JSVal

newtype EventSource = EventSource JSVal

foreign import javascript unsafe "$r = new EventSource($1);"
  newEventSource :: JSString -> IO EventSource

foreign import javascript unsafe "$1.onmessage = $2;"
  onMessage :: EventSource -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2;"
  onError :: EventSource -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2;"
  onClose :: EventSource -> Callback (IO ()) -> IO ()

-- | Test URL
-- http://sapid.sourceforge.net/ssetest/webkit.events.php
-- var source = new EventSource("demo_sse.php");
