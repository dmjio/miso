-----------------------------------------------------------------------------
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
 ( -- *** Subscription
   sseSub
   -- *** Types
 , SSE (..)
 ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import           Miso.Effect (Sub)
import           Miso.FFI
import           Miso.String
-----------------------------------------------------------------------------
import qualified Miso.FFI.SSE as SSE
-----------------------------------------------------------------------------
-- | Server-sent events Subscription
sseSub :: FromJSON msg => MisoString -> (SSE msg -> action) -> Sub action
sseSub url f = \sink -> do
  es <- SSE.new url
  SSE.addEventListener es "message" $ \val -> do
    dat <- jsonParse =<< SSE.data' val
    sink (f (SSEMessage dat))
  SSE.addEventListener es "error" $ \_ ->
    sink (f SSEError)
  SSE.addEventListener es "close" $ \_ ->
    sink (f SSEClose)
-----------------------------------------------------------------------------
-- | Server-sent events data
data SSE message
  = SSEMessage message
  | SSEClose
  | SSEError
  deriving (Show, Eq)
-----------------------------------------------------------------------------
