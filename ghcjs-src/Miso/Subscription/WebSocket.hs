{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( WebSocket (..)
  , URL (..)
  , Protocols (..)
  , websocketSub
  , send
  , close
  , connect
  , getSocketState
  ) where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.Maybe
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.TypedArray.ArrayBuffer
import JavaScript.Web.Blob.Internal      hiding (close)
import Miso.FFI
import Miso.Html.Internal                ( Sub )
import Miso.String
import Prelude                           hiding (map)
import System.IO.Unsafe

-- | WebSocket connection messages
data WebSocket action
  = WebSocketMessage action
  | WebSocketClose
  | WebSocketOpen
  | WebSocketError MisoString

websocket :: IORef (Maybe Socket)
{-# NOINLINE websocket #-}
websocket = unsafePerformIO (newIORef Nothing)

secs :: Int -> Int
secs = (*1000000)

websocketSub
  :: FromJSON m
  => URL
  -> Protocols
  -> (WebSocket m -> action)
  -> Sub action model
websocketSub (URL u) (Protocols ps) f _ sink = do
  socket <- createWebSocket u ps
  writeIORef websocket (Just socket)
  --  Handle Reconnects
  void . forkIO . forever $ do
    threadDelay (secs 3)
    Just s <- readIORef websocket
    status <- getSocketState' s
    when (status == 3) $ do
      atomicWriteIORef websocket
        =<< Just <$> createWebSocket u ps
  onOpen socket =<< do
    asyncCallback $ do
      putStrLn "opened"
      sink (f WebSocketOpen)
  onMessage socket =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      sink $ f (WebSocketMessage d)
  onClose socket =<< do
    asyncCallback $ do
      putStrLn "closed"
      sink (f WebSocketClose)
  onError socket =<< do
    asyncCallback1 $ \v -> do
      putStrLn "error"
      d <- parse =<< getData v
      sink $ f (WebSocketError d)

send :: ToJSON a => a -> IO ()
{-# INLINE send #-}
send x = do
  putStrLn "reading socket"
  Just socket <- readIORef websocket
  putStrLn "got socket"
  sendJson' socket x

close :: Maybe Int -> Maybe JSString -> IO ()
{-# INLINE close #-}
close value reason = do
  Just ws <- readIORef websocket
  closeSocket' ws
    (fromMaybe 1000 value)
    (fromMaybe mempty reason)

connect :: URL -> Protocols -> IO ()
{-# INLINE connect #-}
connect (URL url') (Protocols ps) = do
  Just ws <- readIORef websocket
  s <- getSocketState' ws
  when (s == 3) $ do
    socket <- createWebSocket url' ps
    atomicWriteIORef websocket (Just socket)

newtype URL = URL MisoString
  deriving (Show, Eq)

newtype Protocols = Protocols [MisoString]
  deriving (Show, Eq)

foreign import javascript unsafe "$1.close($2,$3)"
  closeSocket' :: Socket -> Int -> JSString -> IO ()

closeSocket :: Socket -> Maybe Int -> Maybe JSString -> IO ()
{-# INLINE closeSocket #-}
closeSocket ws value reason =
  closeSocket' ws
    (fromMaybe 1000 value)
    (fromMaybe mempty reason)

foreign import javascript unsafe "$1.binaryType === 'blob' ? 0 : 1"
  binaryType' :: Socket -> IO Int

foreign import javascript unsafe "$1.bufferedAmount"
  bufferedAmount :: Socket -> IO Int

foreign import javascript unsafe "$1.extensions"
  extensions :: Socket -> IO JSString

foreign import javascript unsafe "$1.url"
  url :: Socket -> IO JSString

foreign import javascript unsafe "$r = new WebSocket($1, $2);"
  createWebSocket' :: JSString -> JSVal -> IO Socket

foreign import javascript unsafe "$r = $1.readyState;"
  getSocketState' :: Socket -> IO Int

getSocketState = do
  Just ws <- readIORef websocket
  getSocketState' ws

foreign import javascript unsafe "$1.send($2);"
  send' :: Socket -> JSString -> IO ()

sendJson' :: ToJSON json => Socket -> json -> IO ()
sendJson' socket m = send' socket =<< stringify m

foreign import javascript unsafe "$1.send($2);"
  sendArrayBuffer' :: Socket -> ArrayBuffer -> IO ()

foreign import javascript unsafe "$1.send($2);"
  sendBlob' :: Socket -> Blob -> IO ()

foreign import javascript unsafe "$r = $1.protocol;"
  protocol :: Socket -> IO JSString

-- getSocketState :: Socket -> IO State
-- {-# INLINE getSocketState #-}
-- getSocketState s = toEnum <$> getSocketState' s

createWebSocket :: JSString -> [JSString] -> IO Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  createWebSocket' url' =<< toJSVal protocols

foreign import javascript unsafe "$1.onopen = $2"
  onOpen :: Socket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2"
  onClose :: Socket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onmessage = $2"
  onMessage :: Socket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2"
  onError :: Socket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.type"
  getType :: JSVal -> IO JSString

foreign import javascript unsafe "$r = $1.data"
  getData :: JSVal -> IO JSVal

foreign import javascript unsafe "$r = typeof $1;"
  typeOf :: JSVal -> IO JSString

foreign import javascript unsafe "$r = $1 instanceof Blob"
  isBlob :: JSVal -> IO Bool

foreign import javascript unsafe "$r = $1 instanceof ArrayBuffer"
  isArrayBuffer :: JSVal -> IO Bool

-- | Note, you can only 'send' if an open event has been received
-- createCallback
--   :: FromJSON m
--   => (WebSocket m -> IO ())
--   -> IO (Callback (JSVal -> IO ()))
-- createCallback sink =
--   asyncCallback1 $ \o -> getType o >>= flip go o
--    where
--      go "open" _ = sink OpenEvent
--      go "message" o = do
--        dat <- getData o
--        typ <- typeOf dat
--        case typ of
--          "object" -> do
--             blob <- isBlob o
--             when blob $ sink $ Message $ BlobData (SomeBlob dat)
--             arrayBuffer <- isArrayBuffer o
--             when arrayBuffer $
--               sink $ MessageEvent $ ArrayBufferData (unsafeCoerce o)
--          "string" -> do
--             result <- parse dat
--             sink $ MessageEvent (JSON result)
--          _ -> pure ()
--      go "error" _ = sink ErrorEvent
--      go "close" o = do
--        code <- getCode o
--        sink =<< CloseEvent
--             <$> pure code
--             <*> pure (codeToCloseCode code)
--             <*> getReason o
--             <*> wasClean o
--      go _ _ = pure ()

-- foreign import javascript unsafe "$r = $1.wasClean"
--   wasClean :: JSVal -> IO WasClean

-- foreign import javascript unsafe "$r = $1.code"
--   getCode :: JSVal -> IO Code

-- foreign import javascript unsafe "$r = $1.reason"
--   getReason :: JSVal -> IO Reason

newtype Socket = Socket JSVal

--- | https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
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


-- codeToCloseCode :: Code -> CloseCode
-- codeToCloseCode (Code x) = go x
--   where
--     go 1000 = CLOSE_NORMAL
--     go 1001 = CLOSE_GOING_AWAY
--     go 1002 = CLOSE_PROTOCOL_ERROR
--     go 1003 = CLOSE_UNSUPPORTED
--     go 1005 = CLOSE_NO_STATUS
--     go 1006 = CLOSE_ABNORMAL
--     go 1007 = Unsupported_Data
--     go 1008 = Policy_Violation
--     go 1009 = CLOSE_TOO_LARGE
--     go 1010 = Missing_Extension
--     go 1011 = Internal_Error
--     go 1012 = Service_Restart
--     go 1013 = Try_Again_Later
--     go 1015 = TLS_Handshake
--     go n    = OtherCode n
