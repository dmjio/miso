-----------------------------------------------------------------------------
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.WebSocket
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.WebSocket
  ( -- *** t'WebSocket'
    connect
  , connectJSON
  , connectText
  , connectBLOB
  , connectArrayBuffer
  , sendText
  , sendJSON
  , sendBLOB
  , sendArrayBuffer
  , close
  , socketState
  -- *** Defaults
  , emptyWebSocket
  -- *** Types
  , WebSocket   (..)
  , URL
  , SocketState (..)
  , CloseCode   (..)
  , Closed      (..)
  , Payload     (..)
  , Blob        (..)
  , ArrayBuffer (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
-----------------------------------------------------------------------------
import           Miso.Effect
import           Miso.Runtime
import           Miso.String (MisoString)
import           Miso.FFI (Blob(..), ArrayBuffer(..))
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
--
-- Establishes a t'WebSocket' server that receives potentially multiple different t'Payload'.
--
-- It's more common to use 'connectJSON' or 'connectText'. But 'connect' can be used to received multiple
-- different kinds of data from a t'WebSocket' server.
--
connect
  :: FromJSON json
  => URL
  -- ^ URL endpoint for a t'WebSocket' connection
  -> (WebSocket -> action)
  -- ^ @onOpen@ callback w/ t'WebSocket' object for successful connection. t'WebSocket' is used here to send messages.
  -> (Closed -> action)
  -- ^ @onClosed@ method that is called when a t'WebSocket' connection has closed.
  -> (Payload json -> action)
  -- ^ @onMessage@ is a callback invoked when a message has been received from the t'WebSocket' server.
  -> (MisoString -> action)
  -- ^ Error message callback
  -> Effect parent model action
connect = websocketConnect
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
--
-- Establishes a t'WebSocket' server that assumes a JSON-encoded protocol.
--
connectJSON
  :: FromJSON json
  => URL
  -- ^ URL endpoint for a t'WebSocket' connection
  -> (WebSocket -> action)
  -- ^ @onOpen@ callback w/ t'WebSocket' object for successful connection. t'WebSocket' is used here to send messages.
  -> (Closed -> action)
  -- ^ @onClosed@ method that is called when a t'WebSocket' connection has closed.
  -> (json -> action)
  -- ^ @onMessage@ is a callback invoked when a JSON-encoded message has been received from the t'WebSocket' server.
  -> (MisoString -> action)
  -- ^ Error message callback
  -> Effect parent model action
connectJSON = websocketConnectJSON
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
--
-- Establishes a t'WebSocket' server that assumes a text-encoded protocol.
--
connectText
  :: URL
  -- ^ URL endpoint for a t'WebSocket' connection
  -> (WebSocket -> action)
  -- ^ @onOpen@ callback w/ t'WebSocket' object for successful connection. t'WebSocket' is used here to send messages.
  -> (Closed -> action)
  -- ^ @onClosed@ method that is called when a t'WebSocket' connection has closed.
  -> (MisoString -> action)
  -- ^ @onMessage@ is a callback invoked when a text-encoded message has been received from the t'WebSocket' server.
  -> (MisoString -> action)
  -- ^ Error message callback
  -> Effect parent model action
connectText = websocketConnectText
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
--
-- Establishes a t'WebSocket' server that assumes a binary-encoded protocol.
--
connectBLOB
  :: URL
  -- ^ URL endpoint for a t'WebSocket' connection
  -> (WebSocket -> action)
  -- ^ @onOpen@ callback w/ t'WebSocket' object for successful connection. t'WebSocket' is used here to send messages.
  -> (Closed -> action)
  -- ^ @onClosed@ method that is called when a t'WebSocket' connection has closed.
  -> (Blob -> action)
  -- ^ @onMessage@ is a callback invoked when a binary-encoded message has been received from the t'WebSocket' server.
  -> (MisoString -> action)
  -- ^ @onError@ callback
  -> Effect parent model action
connectBLOB = websocketConnectBLOB
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket>
--
-- Establishes a t'WebSocket' server that assumes an t'ArrayBuffer' protocol.
--
connectArrayBuffer
  :: URL
  -- ^ URL endpoint for a t'WebSocket' connection
  -> (WebSocket -> action)
  -- ^ @onOpen@ callback w/ t'WebSocket' object for successful connection. t'WebSocket' is used here to send messages.
  -> (Closed -> action)
  -- ^ @onClosed@ method that is called when a t'WebSocket' connection has closed.
  -> (ArrayBuffer -> action)
  -- ^ @onMessage@ is a callback invoked when an t'ArrayBuffer' message has been received from the t'WebSocket' server.
  -> (MisoString -> action)
  -- ^ @onError@ callback
  -> Effect parent model action
connectArrayBuffer = websocketConnectArrayBuffer
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
--
-- @
--
-- data Person = Person { name :: MisoString, age :: Int }
--   deriving (Show, Eq)
--
-- instance ToJSON Person where
--   toJSON (Person name age) = object [ "name" .= name, "age" .= age ]
--
-- test :: WebSocket -> Effect parent model action
-- test connection = do
--   sendJSON (connection :: WebSocket) (Person "alice" 42)
--   sendJSON (connection :: WebSocket) (Person "bob" 42)
--
-- @
--
sendJSON
  :: ToJSON json
  => WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> json
  -- ^ A JSON-encoded message
  -> Effect parent model action
sendJSON socket x = websocketSend socket (JSON x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendBLOB
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> Blob
  -- ^ An t'Blob' payload to send to the t'WebSocket' server
  -> Effect parent model action
sendBLOB socket x = websocketSend @() socket (blob x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendArrayBuffer
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> ArrayBuffer
  -- ^ An t'ArrayBuffer' payload to send to the t'WebSocket' server
  -> Effect parent model action
sendArrayBuffer socket x = websocketSend @() socket (arrayBuffer x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendText
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> MisoString
  -- ^ A text payload to send to t'WebSocket' server
  -> Effect parent model action
sendText socket x = websocketSend @() socket (TEXT x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close>
--
-- It is very important to close the t'WebSocket', otherwise leaks can occur.
--
-- 'close' is a no-op if invoked multiple times.
--
close
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to close the socket on the server.
  -> Effect parent model action
close = websocketClose
-----------------------------------------------------------------------------
