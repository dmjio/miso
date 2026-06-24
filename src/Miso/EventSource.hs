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
-- Interface to the browser's
-- [Server-Sent Events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events)
-- API, which provides a persistent, unidirectional stream of text or JSON
-- messages pushed from a server to the browser client.
--
-- Typical usage inside an @update@ function:
--
-- @
-- update FetchStream =
--   connectText "\/events" StreamOpened GotMessage StreamError
-- update (GotMessage msg) =
--   modify (\\m -> m { messages = msg : messages m })
-- @
--
-- 'close' shuts down an open 'EventSource' connection. 'socketState' reads
-- the current ready-state, and 'emptyEventSource' provides a zero-value for
-- use in the model before a connection is established.
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
-- | Open a Server-Sent Events connection that delivers raw 'MisoString' messages.
--
-- The three callbacks map browser events to @action@ values dispatched into
-- the MVU loop:
--
-- * @onOpen@    — receives the live 'EventSource' handle (store it in the model
--   to 'close' it later)
-- * @onMessage@ — called with each message payload as a plain string
-- * @onError@   — called with a description of the connection error
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
connectText
  :: URL
  -- ^ URL endpoint for the Server-Sent Events stream
  -> (EventSource -> action)
  -- ^ @onOpen@ callback; receives the live 'EventSource' handle
  -> (MisoString -> action)
  -- ^ @onMessage@ callback; receives each raw text message
  -> (MisoString -> action)
  -- ^ @onError@ callback; receives an error description
  -> Effect parent props model action
connectText = eventSourceConnectText
-----------------------------------------------------------------------------
-- | Open a Server-Sent Events connection that decodes each message as JSON.
--
-- Identical to 'connectText' but the @onMessage@ callback receives a parsed
-- Haskell value of type @value@ (via 'FromJSON') rather than a raw string.
-- JSON decode failures are silently dropped; use 'connectText' and decode
-- manually if you need error recovery.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
connectJSON
  :: FromJSON value
  => URL
  -- ^ URL endpoint for the Server-Sent Events stream
  -> (EventSource -> action)
  -- ^ @onOpen@ callback; receives the live 'EventSource' handle
  -> (value -> action)
  -- ^ @onMessage@ callback; receives each JSON-decoded message
  -> (MisoString -> action)
  -- ^ @onError@ callback; receives an error description
  -> Effect parent props model action
connectJSON = eventSourceConnectJSON
-----------------------------------------------------------------------------
-- | Close an open 'EventSource' connection.
--
-- After calling 'close', no further @onMessage@ or @onError@ callbacks will
-- fire. Corresponds to
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close EventSource.close()>.
close
  :: EventSource
  -- ^ The 'EventSource' handle to close
  -> Effect parent props model action
close = eventSourceClose
-----------------------------------------------------------------------------
