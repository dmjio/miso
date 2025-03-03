{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.SSE
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.SSE
 ( -- * Subscription
   sseSub
   -- * Types
 , SSE (..)
 ) where

import Control.Monad.IO.Class
import Data.Aeson

import Miso.FFI
import Miso.String
import Miso.Types (Sub)

-- | Server-sent events Subscription
sseSub :: FromJSON msg => MisoString -> (SSE msg -> action) -> Sub action
sseSub url f = \sink -> do
  EventSource es <- newSSE url
  addEventListener es "message" =<< do
    asyncCallback1 $ \val -> do
      dat <- parse =<< sseData val
      sink (f (SSEMessage dat))
  addEventListener es "error" =<< do
    asyncCallback $ sink (f SSEError)
  addEventListener es "close" =<< do
    asyncCallback $ sink (f SSEClose)

-- | Server-sent events data
data SSE message
  = SSEMessage message
  | SSEClose
  | SSEError
  deriving (Show, Eq)

-- | Test URL
-- http://sapid.sourceforge.net/ssetest/webkit.events.php
-- var source = new EventSource("demo_sse.php");
