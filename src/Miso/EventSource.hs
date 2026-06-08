-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.EventSource
-- Copyright   :  (C) 2016-2026 David M. Johnson
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
import           Miso.JSON
-----------------------------------------------------------------------------
import           Miso.Effect
import           Miso.Runtime
import           Miso.String
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
--
-- Opens a Server-Sent Events connection that delivers plain-text messages.
--
-- @
-- update = \case
--   Connect -> connectText "/events" SSEOpened SSEMessage SSEError
-- @
--
connectText
  :: URL
  -- ^ URL of the SSE endpoint
  -> (EventSource -> action)
  -- ^ @onOpen@ callback, receives the t'EventSource' handle for later use
  -> (MisoString -> action)
  -- ^ @onMessage@ callback, receives the raw text payload
  -> (MisoString -> action)
  -- ^ @onError@ callback, receives an error description
  -> Effect parent props model action
connectText = eventSourceConnectText
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
--
-- Opens a Server-Sent Events connection that decodes each message as JSON.
--
-- @
-- update = \case
--   Connect -> connectJSON "/events" SSEOpened SSEReceived SSEError
-- @
--
connectJSON
  :: FromJSON value
  => URL
  -- ^ URL of the SSE endpoint
  -> (EventSource -> action)
  -- ^ @onOpen@ callback, receives the t'EventSource' handle for later use
  -> (value -> action)
  -- ^ @onMessage@ callback, receives the JSON-decoded message payload
  -> (MisoString -> action)
  -- ^ @onError@ callback, receives an error description
  -> Effect parent props model action
connectJSON = eventSourceConnectJSON
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close>
--
-- Closes an open t'EventSource' connection.
-- It is important to close the connection when it is no longer needed to avoid resource leaks.
close
  :: EventSource
  -- ^ The t'EventSource' handle returned by 'connectText' or 'connectJSON'
  -> Effect parent props model action
close = eventSourceClose
-----------------------------------------------------------------------------
