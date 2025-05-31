{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.WebSocket
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.WebSocket
  ( -- *** Subscription
    websocketSub
  , send
  , close
  , connect
  , getSocketState
  -- *** Types
  , WebSocket   (..)
  , URL         (..)
  , Protocols   (..)
  , SocketState (..)
  , CloseCode   (..)
  , WasClean    (..)
  , Reason      (..)
  ) where
-----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (when, void, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicWriteIORef)
import           GHC.Generics (Generic)
import           Language.Javascript.JSaddle hiding (create)
import           System.IO.Unsafe (unsafePerformIO)
-----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import qualified Miso.FFI.Internal as FFI
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
websocket :: IORef (Maybe Socket)
{-# NOINLINE websocket #-}
websocket = unsafePerformIO (newIORef Nothing)
-----------------------------------------------------------------------------
closedCode :: IORef (Maybe CloseCode)
{-# NOINLINE closedCode #-}
closedCode = unsafePerformIO (newIORef Nothing)
-----------------------------------------------------------------------------
secs :: Int -> Int
secs = (*1000000)
-----------------------------------------------------------------------------
-- | WebSocket subscription
websocketSub
  :: FromJSON m
  => URL
  -> Protocols
  -> (WebSocket m -> action)
  -> Sub action
websocketSub (URL u) (Protocols ps) f sink = do
  socket <- createWebSocket u ps
  liftIO (writeIORef websocket (Just socket))
  void . FFI.forkJSM $ handleReconnect
  addEventListener socket "open" $ \_ -> do
    liftIO (writeIORef closedCode Nothing)
    sink (f WebSocketOpen)
  addEventListener socket "message" $ \v -> do
    d <- FFI.jsonParse =<< v ! ("data" :: MisoString)
    sink $ f (WebSocketMessage d)
  addEventListener socket "close" $ \e -> do
    code <- codeToCloseCode <$> getCode e
    liftIO (writeIORef closedCode (Just code))
    reason <- getReason e
    clean <- wasClean e
    sink $ f (WebSocketClose code clean reason)
  addEventListener socket "error" $ \v -> do
    liftIO (writeIORef closedCode Nothing)
    d' <- v ! ("data" :: MisoString)
#ifndef ghcjs_HOST_OS
    undef <- FFI.ghcjsPure (isUndefined d')
#else
    let undef = isUndefined d'
#endif
    if undef
      then do
         sink $ f (WebSocketError mempty)
      else do
         Just d <- fromJSVal d'
         sink $ f (WebSocketError d)
  where
    handleReconnect = do
      liftIO (threadDelay (secs 3))
      Just s <- liftIO (readIORef websocket)
      status <- socketState s
      code <- liftIO (readIORef closedCode)
      if status == 3
        then do
          unless (code == Just CLOSE_NORMAL) $
            websocketSub (URL u) (Protocols ps) f sink
        else handleReconnect
-----------------------------------------------------------------------------
-- | Sends message to a websocket server
send :: ToJSON a => a -> JSM ()
{-# INLINE send #-}
send x = do
  Just socket <- liftIO (readIORef websocket)
  sendJson' socket x
-----------------------------------------------------------------------------
-- | Sends message to a websocket server
close :: JSM ()
{-# INLINE close #-}
close = mapM_ closeSocket =<< liftIO (readIORef websocket)
-----------------------------------------------------------------------------
-- | Connects to a websocket server
connect :: URL -> Protocols -> JSM ()
{-# INLINE connect #-}
connect (URL url') (Protocols ps) = do
  Just ws <- liftIO (readIORef websocket)
  s <- socketState ws
  when (s == 3) $ do
    socket <- createWebSocket url' ps
    liftIO (atomicWriteIORef websocket (Just socket))
-----------------------------------------------------------------------------
-- | Retrieves current status of `WebSocket`
getSocketState :: JSM SocketState
getSocketState = do
  Just ws <- liftIO (readIORef websocket)
  toEnum <$> socketState ws
-----------------------------------------------------------------------------
sendJson' :: ToJSON json => Socket -> json -> JSM ()
sendJson' socket m = sendSocket socket =<< FFI.jsonStringify m
-----------------------------------------------------------------------------
createWebSocket :: MisoString -> [MisoString] -> JSM Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols = createSocket url' =<< toJSVal protocols
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
-- | WebSocket connection messages
data WebSocket action
  = WebSocketMessage action
  | WebSocketClose CloseCode WasClean Reason
  | WebSocketOpen
  | WebSocketError MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | URL of Websocket server
newtype URL = URL MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Protocols for Websocket connection
newtype Protocols = Protocols [MisoString]
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Wether or not the connection closed was done so cleanly
newtype WasClean = WasClean Bool deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Reason for closed connection
newtype Reason = Reason MisoString deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | `SocketState` corresponding to current WebSocket connection
data SocketState
  = CONNECTING -- ^ 0
  | OPEN       -- ^ 1
  | CLOSING    -- ^ 2
  | CLOSED     -- ^ 3
  deriving (Show, Eq, Ord, Enum)
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
instance ToJSVal CloseCode
-----------------------------------------------------------------------------
instance FromJSVal CloseCode
-----------------------------------------------------------------------------
newtype Socket = Socket JSVal
-----------------------------------------------------------------------------
createSocket :: MisoString -> JSVal -> JSM Socket
createSocket url protocols = Socket <$> new (jsg ("WebSocket" :: JSString)) (url, protocols)
-----------------------------------------------------------------------------
socketState :: Socket -> JSM Int
socketState (Socket s) = fromJSValUnchecked =<< s ! ("readyState" :: JSString)
-----------------------------------------------------------------------------
addEventListener :: Socket -> MisoString -> (JSVal -> JSM ()) -> JSM ()
addEventListener (Socket s) name cb = do
  FFI.addEventListener s name cb
-----------------------------------------------------------------------------
wasClean :: JSVal -> JSM WasClean
wasClean v = WasClean <$> (fromJSValUnchecked =<< v ! ("wasClean" :: JSString))
-----------------------------------------------------------------------------
getCode :: JSVal -> JSM Int
getCode v = fromJSValUnchecked =<< v ! ("code" :: JSString)
-----------------------------------------------------------------------------
getReason :: JSVal -> JSM Reason
getReason v = Reason <$> (fromJSValUnchecked =<< v ! ("reason" :: JSString))
-----------------------------------------------------------------------------
closeSocket :: Socket -> JSM ()
closeSocket (Socket s) = do
  _ <- s # ("close" :: JSString) $ ([] :: [JSString])
  pure ()
-----------------------------------------------------------------------------
sendSocket :: Socket -> MisoString -> JSM ()
sendSocket (Socket s) msg = do
  _ <- s # ("send" :: JSString) $ [msg]
  pure ()
-----------------------------------------------------------------------------
