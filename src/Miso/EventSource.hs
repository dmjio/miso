-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.EventSource
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Functions and types for working with [Server Sent Events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events)
--
----------------------------------------------------------------------------
module Miso.EventSource
  ( -- *** EventSource
    connectText
  , connectJSON
  , close
  , socketState
  -- *** Defaults
  , emptyEventSource
  -- *** Types
  , EventSource (..)
  , URL
  -- *** Re-exports
  , Payload (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
-----------------------------------------------------------------------------
import           Miso.Effect
import           Miso.Runtime
import           Miso.String
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
connectText
  :: URL
  -> (EventSource -> action)
  -- ^ onOpen
  -> (MisoString -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect parent model action
connectText = eventSourceConnectText
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
connectJSON
  :: FromJSON value
  => URL
  -> (EventSource -> action)
  -- ^ onOpen
  -> (value -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect parent model action
connectJSON = eventSourceConnectJSON
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close>
close
  :: EventSource
  -> Effect parent model action
close = eventSourceClose
-----------------------------------------------------------------------------
