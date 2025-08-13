-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
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
  ( -- *** Functions
    connect
  , socketState
  , send
  , close
  -- *** Defaults
  , emptyWebSocket
  -- *** Types
  , WebSocket   (..)
  , URL
  , SocketState (..)
  , CloseCode   (..)
  , Closed      (..)
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad
import           GHC.Generics (Generic)
import           Language.Javascript.JSaddle hiding (create)
-----------------------------------------------------------------------------
import           Miso.Effect (Effect, withSink)
import qualified Miso.FFI.Internal as FFI
import           Miso.String (MisoString)
import qualified Data.IntMap as IM
import           System.IO.Unsafe
import           Data.IORef
-----------------------------------------------------------------------------
type Socket = JSVal
-----------------------------------------------------------------------------
connections :: IORef (IM.IntMap Socket)
{-# NOINLINE connections #-}
connections = unsafePerformIO (newIORef IM.empty)
-----------------------------------------------------------------------------
connectionIds :: IORef Int
{-# NOINLINE connectionIds #-}
connectionIds = unsafePerformIO (newIORef (0 :: Int))
-----------------------------------------------------------------------------
-- | WebSocket connection
connect
  :: URL
  -- ^ WebSocket URL
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (JSVal -> action)
  -- ^ onMessage
  -> (JSVal -> action)
  -- ^ onError
  -> Effect parent model action
connect url onOpen onClosed onError onMessage =
  withSink $ \sink -> do
    next <- liftIO (atomicModifyIORef' connectionIds $ \x -> (x + 1, x))
    socket <- FFI.websocketConnect url
      (sink $ onOpen (WebSocket next))
      (sink . onClosed <=< fromJSValUnchecked)
      (sink . onMessage)
      (sink . onError)
    liftIO $
      atomicModifyIORef' connections $ \imap ->
        (IM.insert next socket imap, ())
-----------------------------------------------------------------------------
send :: WebSocket -> JSVal -> Effect parent model action
send (WebSocket socketId) msg = withSink $ \_ -> do
  maybeSocket <- IM.lookup socketId <$> liftIO (readIORef connections)
  forM_ maybeSocket $ \socket ->
    -- dmj: warn here on debug
    FFI.websocketSend socket msg
-----------------------------------------------------------------------------
close :: WebSocket -> Effect parent model action
close (WebSocket socketId) = withSink $ \_ -> do
  result <-
    liftIO $ atomicModifyIORef' connections $ \imap ->
      (IM.delete socketId imap, IM.lookup socketId imap)
  forM_ result FFI.websocketClose
-----------------------------------------------------------------------------
-- | Retrieves current status of `WebSocket`
socketState :: WebSocket -> (SocketState -> action) -> Effect parent model action 
socketState (WebSocket socketId) callback = withSink $ \sink -> do
  IM.lookup socketId <$> liftIO (readIORef connections) >>= \case
    Just socket -> do
      ss <- fromJSValUnchecked =<< socket ! "socketState"
      sink $ callback (toEnum ss)
    Nothing -> pure ()
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
data Closed
  = Closed
  { closedCode :: CloseCode
  , wasClean :: Bool
  , reason :: MisoString
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
instance FromJSVal Closed where
  fromJSVal o = do
    closed_ <- fmap codeToCloseCode <$> do (fromJSVal =<< o ! "code")
    wasClean_ <- fromJSVal =<< o ! "wasClean"
    reason_ <- fromJSVal =<< o ! "reason"
    pure (Closed <$> closed_ <*> wasClean_ <*> reason_)
-----------------------------------------------------------------------------
-- | URL that the @Websocket@ will @connect@ to
type URL = MisoString
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
newtype WebSocket = WebSocket Int
  deriving (ToJSVal, Eq, Num)
-----------------------------------------------------------------------------
emptyWebSocket :: WebSocket
emptyWebSocket = (-1)
-----------------------------------------------------------------------------
