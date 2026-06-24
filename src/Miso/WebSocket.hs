-----------------------------------------------------------------------------
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.WebSocket
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.WebSocket" provides a full-duplex
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket WebSocket>
-- client that integrates directly into the MVU loop. Every operation
-- — connecting, sending, and closing — returns an 'Miso.Effect.Effect',
-- making WebSocket communication a first-class citizen of the @update@
-- function.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.WebSocket"
--
-- data Action
--   = Connect
--   | Connected  'WebSocket'
--   | Received   'Miso.String.MisoString'
--   | Disconnected 'Closed'
--   | WsError    'Miso.String.MisoString'
--   | Send       'Miso.String.MisoString'
--   | Disconnect
--
-- -- Hold the socket handle in the model so we can send later
-- data Model = Model { ws :: 'WebSocket' }
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update Connect =
--   'connectText' \"wss:\/\/echo.websocket.org\"
--     Connected Disconnected Received WsError
-- update (Connected sock) =
--   'Miso.State.modify' (\\m -> m { ws = sock })
-- update (Received msg) =
--   'Miso.Effect.io_' (consoleLog msg)
-- update (Send txt) = do
--   sock <- 'Miso.State.gets' ws
--   'sendText' sock txt
-- update Disconnect = do
--   sock <- 'Miso.State.gets' ws
--   'close' sock
-- update _ = pure ()
-- @
--
-- = Connection variants
--
-- Five @connect@ functions cover every wire format. They all share the
-- same callback signature — @onOpen@, @onClosed@, @onMessage@, @onError@
-- — but differ in how the message payload is decoded:
--
-- ['connectText'] 'Miso.String.MisoString' — plain UTF-8 text
-- ['connectJSON'] @json@ ('Miso.JSON.FromJSON' json) — auto-decoded from JSON
-- ['connectBLOB'] 'Blob' — raw binary Blob
-- ['connectArrayBuffer'] 'ArrayBuffer' — raw binary buffer
-- ['connect'] @'Payload' json@ — mixed; caller pattern-matches the payload ADT
--
-- = Sending messages
--
-- The 'WebSocket' handle delivered to @onOpen@ must be stored in the
-- model and passed to each send call:
--
-- @
-- 'sendText'        :: 'WebSocket' -> 'Miso.String.MisoString' -> 'Miso.Effect.Effect' p props model action
-- 'sendJSON'        :: 'Miso.JSON.ToJSON' json => 'WebSocket' -> json -> 'Miso.Effect.Effect' p props model action
-- 'sendBLOB'        :: 'WebSocket' -> 'Blob' -> 'Miso.Effect.Effect' p props model action
-- 'sendArrayBuffer' :: 'WebSocket' -> 'ArrayBuffer' -> 'Miso.Effect.Effect' p props model action
-- @
--
-- = Lifecycle
--
-- * Always call 'close' when the connection is no longer needed to avoid
--   resource leaks. It is safe to call 'close' multiple times — subsequent
--   calls are no-ops.
-- * 'socketState' lets you query the current 'SocketState'
--   (@CONNECTING@, @OPEN@, @CLOSING@, @CLOSED@) asynchronously.
-- * 'emptyWebSocket' (@= -1@) is a null sentinel you can store in the
--   model before a connection has been established.
--
-- = Types
--
-- * 'WebSocket' — an opaque integer file descriptor returned by @onOpen@.
-- * 'URL' — alias for 'Miso.String.MisoString'.
-- * 'SocketState' — four-state enum mirroring the JS @readyState@ property.
-- * 'Closed' — close event data: 'closedCode', 'wasClean', 'reason'.
-- * 'CloseCode' — typed close codes from
--   <https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/code RFC 6455>.
-- * 'Payload' — @JSON value | BLOB Blob | TEXT MisoString | BUFFER ArrayBuffer@.
-- * 'Blob' / 'ArrayBuffer' — re-exported from "Miso.FFI.Internal".
--
-- = See also
--
-- * "Miso.EventSource" — Server-Sent Events (SSE), a unidirectional alternative
-- * "Miso.Fetch" — one-shot HTTP requests
-- * "Miso.Effect" — 'Miso.Effect.Effect', 'Miso.Effect.io_'
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
import           Miso.Effect
import           Miso.JSON
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
  -> Effect parent props model action
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
  -> Effect parent props model action
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
  -> Effect parent props model action
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
  -> Effect parent props model action
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
  -> Effect parent props model action
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
-- test :: WebSocket -> Effect parent props model action
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
  -> Effect parent props model action
sendJSON socket x = websocketSend socket (JSON x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendBLOB
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> Blob
  -- ^ An t'Blob' payload to send to the t'WebSocket' server
  -> Effect parent props model action
sendBLOB socket x = websocketSend @() socket (blob x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendArrayBuffer
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> ArrayBuffer
  -- ^ An t'ArrayBuffer' payload to send to the t'WebSocket' server
  -> Effect parent props model action
sendArrayBuffer socket x = websocketSend @() socket (arrayBuffer x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendText
  :: WebSocket
  -- ^ t'WebSocket' descriptor required to send a message to the server.
  -> MisoString
  -- ^ A text payload to send to t'WebSocket' server
  -> Effect parent props model action
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
  -> Effect parent props model action
close = websocketClose
-----------------------------------------------------------------------------
