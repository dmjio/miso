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
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.WebSocket
  ( -- * Types
    WebSocket   (..)
  , URL         (..)
  , Protocols   (..)
  , SocketState (..)
  , CloseCode   (..)
  , WasClean    (..)
  , Reason      (..)
    -- * Subscription
  , websocketSub
  , send
  , connect
  , getSocketState
  ) where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.Maybe
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import Prelude                hiding (map)
import System.IO.Unsafe

import Miso.FFI
import Miso.Html.Internal     ( Sub )
import Miso.WebSocket

websocket :: IORef (Maybe Socket)
{-# NOINLINE websocket #-}
websocket = unsafePerformIO (newIORef Nothing)

closedCode :: IORef (Maybe CloseCode)
{-# NOINLINE closedCode #-}
closedCode = unsafePerformIO (newIORef Nothing)

secs :: Int -> Int
secs = (*1000000)

-- | WebSocket subscription
websocketSub
  :: FromJSON m
  => URL
  -> Protocols
  -> (WebSocket m -> action)
  -> Sub action
websocketSub (URL u) (Protocols ps) f sink = do
  socket <- createWebSocket u ps
  writeIORef websocket (Just socket)
  void . forkIO $ handleReconnect
  onOpen socket =<< do
    writeIORef closedCode Nothing
    asyncCallback $ sink (f WebSocketOpen)
  onMessage socket =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      sink $ f (WebSocketMessage d)
  onClose socket =<< do
    asyncCallback1 $ \e -> do
      code <- codeToCloseCode <$> getCode e
      writeIORef closedCode (Just code)
      reason <- getReason e
      clean <- wasClean e
      sink $ f (WebSocketClose code clean reason)
  onError socket =<< do
    asyncCallback1 $ \v -> do
      writeIORef closedCode Nothing
      d <- parse =<< getData v
      sink $ f (WebSocketError d)
  where
    handleReconnect = do
      threadDelay (secs 3)
      Just s <- readIORef websocket
      status <- getSocketState' s
      code <- readIORef closedCode
      if status == 3
        then do
          unless (code == Just CLOSE_NORMAL) $
            websocketSub (URL u) (Protocols ps) f sink
        else handleReconnect

-- | Sends message to a websocket server
send :: ToJSON a => a -> IO ()
{-# INLINE send #-}
send x = do
  Just socket <- readIORef websocket
  sendJson' socket x

-- | Connects to a websocket server
connect :: URL -> Protocols -> IO ()
{-# INLINE connect #-}
connect (URL url') (Protocols ps) = do
  Just ws <- readIORef websocket
  s <- getSocketState' ws
  when (s == 3) $ do
    socket <- createWebSocket url' ps
    atomicWriteIORef websocket (Just socket)

foreign import javascript unsafe "$r = new WebSocket($1, $2);"
  createWebSocket' :: JSString -> JSVal -> IO Socket

foreign import javascript unsafe "$r = $1.readyState;"
  getSocketState' :: Socket -> IO Int

-- | Retrieves current status of `WebSocket`
getSocketState :: IO SocketState
getSocketState = do
  Just ws <- readIORef websocket
  toEnum <$> getSocketState' ws

foreign import javascript unsafe "$1.send($2);"
  send' :: Socket -> JSString -> IO ()

sendJson' :: ToJSON json => Socket -> json -> IO ()
sendJson' socket m = send' socket =<< stringify m

createWebSocket :: JSString -> [JSString] -> IO Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  createWebSocket' url' =<< toJSVal protocols

foreign import javascript unsafe "$1.onopen = $2"
  onOpen :: Socket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2"
  onClose :: Socket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onmessage = $2"
  onMessage :: Socket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2"
  onError :: Socket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.data"
  getData :: JSVal -> IO JSVal

foreign import javascript unsafe "$r = $1.wasClean"
  wasClean :: JSVal -> IO WasClean

foreign import javascript unsafe "$r = $1.code"
  getCode :: JSVal -> IO Int

foreign import javascript unsafe "$r = $1.reason"
  getReason :: JSVal -> IO Reason

newtype Socket = Socket JSVal

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
