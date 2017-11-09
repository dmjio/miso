{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.WebSocket
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.WebSocket
  ( -- * Types
    WebSocket
  , WebSocketEvent       (..)
  , WebSocketClosedEvent (..)
  , WebSocketConfig      (..)
  , PingConfig           (..)
  , SocketState          (..)
  , CloseCode            (..)

    -- * Subscription
  , websocketSub
  , websocketSubWithConfig

    -- * Creating a websocket
  , newWebSocket
  , newWebSocketWithProtocols

    -- * Interacting with the websocket
  , sendJsonToWebSocket
  , sendTextToWebSocket
  , closeWebSocket
  , closeWebSocketWithCode

    -- * Querying the socket
  , getSocketState
  , wsURL

    -- * Configuration
  , defaultWSConfig
  , defaultPingConfig
  ) where

import Control.Concurrent      (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newMVar,
                                tryTakeMVar, readMVar)
import Control.Monad
import Data.Aeson
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types

import Miso.FFI                (stringify, parseJsonString)
import Miso.Html.Internal      (Sub)
import Miso.String             hiding (concat)


-- | WebSocket connection messages. The `message` type should be a
-- | `FromJSON` instance.
data WebSocketEvent message
  = WebSocketMessage message
  | WebSocketConnectFailed {failedUrl :: MisoString,
                            failedProtocols :: [MisoString],
                            failedMessage :: MisoString}
  | WebSocketUnparseableMessage {unparsableMsgData::MisoString,
                                 unparseableErrorMsg::MisoString}
  | WebSocketClosed WebSocketClosedEvent
  | WebSocketOpen
  | WebSocketError MisoString
  deriving (Show, Eq, Generic)

-- | High-level WebSocket datatype.
-- The internal websocket object is wrapped in an MVar because it
-- could change (if the connection is restarted)
data WebSocket = WebSocket {
  wsURL       :: !MisoString,
  wsProtocols :: ![MisoString],
  wsSocketRef :: !(MVar JSWebSocket)
  } deriving (Eq)


instance Show WebSocket where
  show ws = "WebSocket(" <> show (wsURL ws) <> ")"

-- | Ping configuration, if you want to keep your websocket active.
data PingConfig = PingConfig {
  -- | How many seconds to wait between pings.
  pingSeconds :: Int,
  -- | Message to send in the ping.
  pingSend :: MisoString,
  -- | Expect this message in response.
  pingReceive :: MisoString
  }

-- | Configuration for websocket connection.
data WebSocketConfig = WebSocketConfig {
  -- | If @Just (n, msg)@, will send a ping message @msg@ every @n@ seconds.
  websocketPing :: Maybe PingConfig,
  -- | Decide whether to reconnect when the websocket closes.
  -- | If this returns true, the websocket will be attempted to be
  -- | reconnected. If false, the action handler for WebSocketClosed will be
  -- | triggered with the given event.
  websocketShouldReconnectOnClose :: WebSocketClosedEvent -> IO Bool
  }

-- | Information received when a WebSocket closes.
data WebSocketClosedEvent = WebSocketClosedEvent {
  closedCode :: CloseCode,
  closedCleanly :: Bool,
  closedReason :: MisoString
  }
  deriving (Show, Eq, Generic)

-- | `SocketState` corresponding to current WebSocket connection
data SocketState
  = CONNECTING -- ^ 0
  | OPEN       -- ^ 1
  | CLOSING    -- ^ 2
  | CLOSED     -- ^ 3
  deriving (Show, Eq, Ord, Enum)

-- | Low-level websocket handle, wraps the raw JSVal.
newtype JSWebSocket = JSWebSocket JSVal

-- | Create a new low-level websocket.
newJSWebSocket
  :: MisoString -> [MisoString] -> IO (Either MisoString JSWebSocket)
newJSWebSocket url protocols = do
  result <- newWebSocketOrError' url =<< toJSVal protocols
  isWebSocket' result >>= \case
    True -> pure (Right (JSWebSocket result))
    False -> Left <$> getExceptionMessage' result

-- | Create a new high-level websocket.
newWebSocketWithProtocols
  :: MisoString -> [MisoString] -> IO (Either MisoString WebSocket)
newWebSocketWithProtocols url protocols = newJSWebSocket url protocols >>= \case
  Left err -> pure (Left err)
  Right socket_ -> Right . WebSocket url protocols <$> newMVar socket_

-- | Create a new high-level websocket with an empty protocol list
newWebSocket :: MisoString -> IO (Either MisoString WebSocket)
{-# INLINE newWebSocket #-}
newWebSocket = flip newWebSocketWithProtocols []

-- | Close a high-level websocket.
closeWebSocket :: WebSocket -> IO ()
closeWebSocket ws@(WebSocket _ _ ref) = tryTakeMVar ref >>= \case
  Nothing -> putStrLn $ concat ["WebSocket ", show ws, " already closed"]
  Just ws_ -> closeJSWebSocket ws_

-- | Close a websocket with the default code (1000)
closeJSWebSocket :: JSWebSocket -> IO ()
closeJSWebSocket = closeWebSocketWithCode 1000

-- | Default ping config. Sends "ping" every 30 seconds and expects "pong".
defaultPingConfig :: PingConfig
defaultPingConfig = PingConfig 30 "ping" "pong"

-- | Default websocket config.
-- Doesn't set up a ping, and attempts to reconnect if close was abnormal.
defaultWSConfig :: WebSocketConfig
defaultWSConfig = WebSocketConfig {
  websocketPing = Nothing,
  websocketShouldReconnectOnClose = \event -> pure (not $ closedCleanly event)
  }

--------------------------------------------------------------------------------
-- * Creating Subscriptions


-- | WebSocket subscription with default configuration.
websocketSub
  :: FromJSON message
  => WebSocket                          -- ^ Handle to the socket.
  -> (WebSocketEvent message -> action) -- ^ Message handler.
  -> Sub action model                   -- ^ A subscription.
websocketSub = websocketSubWithConfig defaultWSConfig


-- | WebSocket subscription, given a particular configuration.
--
-- If the socket disconnects, the config will be used to determine
-- whether to reconnect; if so it happens seamlessly. If not, the
-- WebSocketClosed event will be fired.
websocketSubWithConfig
  :: FromJSON message
  => WebSocketConfig                    -- ^ Configuration
  -> WebSocket                          -- ^ Handle to the socket.
  -> (WebSocketEvent message -> action) -- ^ Event handler.
  -> Sub action model                   -- ^ A subscription
websocketSubWithConfig config socket handler getModel sink = do
  let (url, protocols) = (wsURL socket, wsProtocols socket)

  -- Spin off ping thread if configured
  pingThreadId <- flip mapM (websocketPing config) $ \(PingConfig{..}) -> do
    forkIO $ forever $ do
      threadDelay (pingSeconds * 1000000)
      sendTextToWebSocket socket pingSend

  -- Fire the WebSocketOpen event when the socket opens.
  socket_ <- getSocket_ socket
  onOpen socket_ =<< do
    asyncCallback (sink (handler WebSocketOpen))

  -- When a message is received, parse it as JSON and fire an event.
  onMessage socket_ =<< do
    let parseData raw = parseJsonString raw >>= \case
          Error err -> sink $ handler (WebSocketUnparseableMessage raw (ms err))
          Success message -> sink $ handler (WebSocketMessage message)
    asyncCallback1 $ case websocketPing config of
      -- Iff a ping is configured, filter out ping responses before parsing.
      Just (PingConfig {..}) -> getData >=> \case
        msg | msg == pingReceive -> pure ()
        rawData -> parseData rawData
      Nothing -> getData >=> parseData

  -- When the socket closed, determine whether to reconnect. If so,
  -- update the MVar in the WebSocket. If reconnection fails, the
  -- WebSocketConnectFailed event will be fired.
  onClose socket_ =<< do
    asyncCallback1 $ \e -> do
      -- Stop the ping thread, if it's running.
      mapM_ killThread pingThreadId
      -- Take this MVar to prevent further actions from hitting it.
      void $ takeMVar (wsSocketRef socket)
      closedCode <- codeToCloseCode <$> getCode e
      closedReason <- getReason e
      closedCleanly <- wasClean e
      let closedEvent = WebSocketClosedEvent {..}
      websocketShouldReconnectOnClose config closedEvent >>= \case
        True -> newJSWebSocket url protocols >>= \case
          Right ws -> do
            setSocket_ socket ws
            websocketSubWithConfig config socket handler getModel sink
          Left err -> do
            sink (handler $ WebSocketConnectFailed url protocols err)
        False -> sink $ handler (WebSocketClosed closedEvent)

  -- On an error, fire the WebSocketError event.
  onError socket_ =<< do
    asyncCallback1 $ \v -> do
      sink =<< handler . WebSocketError <$> getData v

--------------------------------------------------------------------------------

-- | Read the IORef in a WebSocket to get its JSWebSocket
getSocket_ :: WebSocket -> IO JSWebSocket
getSocket_ = readMVar . wsSocketRef

-- | Set the IORef on a WebSocket.
setSocket_ :: WebSocket -> JSWebSocket -> IO ()
setSocket_ ws ws_ = putMVar (wsSocketRef ws) ws_

-- | Retrieves current status of `socket`
getSocketState :: WebSocket -> IO SocketState
getSocketState socket = toEnum <$> (getSocketState' =<< getSocket_ socket)

-- | Send a JSON-able message to a websocket.
sendJsonToWebSocket :: ToJSON json => WebSocket -> json -> IO ()
sendJsonToWebSocket socket m = sendTextToWebSocket socket =<< stringify m

-- | Send arbitrary text to a websocket.
sendTextToWebSocket :: WebSocket -> MisoString -> IO ()
sendTextToWebSocket socket m = do
  socket_ <- getSocket_ socket
  send' socket_ m


--------------------------------------------------------------------------------
-- * FFI
--------------------------------------------------------------------------------

foreign import javascript unsafe "$1.send($2);"
  send' :: JSWebSocket -> JSString -> IO ()

foreign import javascript unsafe
  "try { $r = new WebSocket($1, $2); } catch (e) { $r = e; }"
  newWebSocketOrError' :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe "$2.close($1);"
  closeWebSocketWithCode :: Int -> JSWebSocket -> IO ()

foreign import javascript unsafe "$r = $1.readyState;"
  getSocketState' :: JSWebSocket -> IO Int

-- | Use this to figure out if websocket creation was successful.
foreign import javascript safe "$r = $1.constructor === WebSocket;"
  isWebSocket' :: JSVal -> IO Bool

-- | Get the message off of the exception so that we can return it.
foreign import javascript safe "$r = $1.message;"
  getExceptionMessage' :: JSVal -> IO MisoString

foreign import javascript unsafe "$1.onopen = $2"
  onOpen :: JSWebSocket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2"
  onClose :: JSWebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onmessage = $2"
  onMessage :: JSWebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2"
  onError :: JSWebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.data || ''"
  getData :: JSVal -> IO JSString

foreign import javascript unsafe "$r = $1.wasClean"
  wasClean :: JSVal -> IO Bool

foreign import javascript unsafe "$r = $1.code"
  getCode :: JSVal -> IO Int

foreign import javascript unsafe "$r = $1.reason"
  getReason :: JSVal -> IO MisoString


-- | Code corresponding to a closed connection
-- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
data CloseCode
  = CLOSE_NORMAL
   -- ^ 1000, Normal closure; the connection successfully completed whatever purpose for which it was created.
  | CLOSE_GOING_AWAY
   -- ^ 1001, The endpoint is going away, either because of a server failure or because the browser is navigating away from the page that opened the connection.
  | CLOSE_PROTOCOL_ERROR
   -- ^ 1002, The endpoint is terminating the connection due to a protocol error.
  | CLOSE_UNSUPPORTED
   -- ^ 1003, The connection is being terminated because the endpoint received data of a type it cannot accept (for example, a textonly endpoint received binary data).
  | CLOSE_NO_STATUS
   -- ^ 1005, Reserved.  Indicates that no status code was provided even though one was expected.
  | CLOSE_ABNORMAL
   -- ^ 1006, Reserved. Used to indicate that a connection was closed abnormally (that is, with no close frame being sent) when a status code is expected.
  | Unsupported_Data
   -- ^ 1007, The endpoint is terminating the connection because a message was received that contained inconsistent data (e.g., nonUTF8 data within a text message).
  | Policy_Violation
   -- ^ 1008, The endpoint is terminating the connection because it received a message that violates its policy. This is a generic status code, used when codes 1003 and 1009 are not suitable.
  | CLOSE_TOO_LARGE
   -- ^ 1009, The endpoint is terminating the connection because a data frame was received that is too large.
  | Missing_Extension
   -- ^ 1010, The client is terminating the connection because it expected the server to negotiate one or more extension, but the server didn't.
  | Internal_Error
   -- ^ 1011, The server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.
  | Service_Restart
   -- ^ 1012, The server is terminating the connection because it is restarting.
  | Try_Again_Later
   -- ^ 1013, The server is terminating the connection due to a temporary condition, e.g. it is overloaded and is casting off some of its clients.
  | TLS_Handshake
   -- ^ 1015, Reserved. Indicates that the connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).
  | OtherCode Int
   -- ^ OtherCode that is reserved and not in the range 0999
  deriving (Show, Eq, Generic)

instance ToJSVal CloseCode
instance FromJSVal CloseCode

codeToCloseCode :: Int -> CloseCode
codeToCloseCode = go
  where
    go 1000 = CLOSE_NORMAL
    go 1001 = CLOSE_GOING_AWAY
    go 1002 = CLOSE_PROTOCOL_ERROR
    go 1003 = CLOSE_UNSUPPORTED
    go 1005 = CLOSE_NO_STATUS
    go 1006 = CLOSE_ABNORMAL
    go 1007 = Unsupported_Data
    go 1008 = Policy_Violation
    go 1009 = CLOSE_TOO_LARGE
    go 1010 = Missing_Extension
    go 1011 = Internal_Error
    go 1012 = Service_Restart
    go 1013 = Try_Again_Later
    go 1015 = TLS_Handshake
    go n    = OtherCode n
