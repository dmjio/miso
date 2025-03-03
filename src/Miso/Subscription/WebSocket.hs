{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , close
  , connect
  , getSocketState
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.IORef
import           Data.Maybe
import           Prelude hiding (map)
import           System.IO.Unsafe

import           Miso.Types (Sub)
import           Miso.FFI hiding (send, close)
import qualified Miso.FFI as FFI
import           Miso.String
import           Miso.WebSocket

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
  Socket socket <- createWebSocket u ps
  liftIO (writeIORef websocket (Just (Socket socket)))
  void . forkIO $ handleReconnect
  addEventListener socket "open" =<< do
    asyncCallback $ do
      writeIORef closedCode Nothing
      sink (f WebSocketOpen)
  addEventListener socket "message" =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< FFI.websocketData v 
      liftIO . sink $ f (WebSocketMessage d)
  addEventListener socket "close" =<< do
    asyncCallback1 $ \e -> do
      code <- codeToCloseCode <$> code e
      liftIO (writeIORef closedCode (Just code))
      reason <- reason e
      clean <- wasClean e
      liftIO . sink $ f (WebSocketClose code (WasClean clean) (Reason reason))
  addEventListener socket "error" =<< do
    asyncCallback1 $ \v -> do
      writeIORef closedCode Nothing
      dater <- websocketData v
      undef <- isNullOrUndefined dater
      if undef
        then do
           liftIO . sink $ f (WebSocketError mempty)
        else do
           Just d <- fromJSVal dater
           liftIO . sink $ f (WebSocketError d)
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

-- | Sends message to a websocket server
send :: ToJSON a => a -> IO ()
{-# INLINE send #-}
send x = do
  Just socket <- liftIO (readIORef websocket)
  sendJson' socket x

-- | Sends message to a websocket server
close :: IO ()
{-# INLINE close #-}
close = mapM_ FFI.close =<< readIORef websocket

-- | Connects to a websocket server
connect :: URL -> Protocols -> IO ()
{-# INLINE connect #-}
connect (URL url') (Protocols ps) = do
  Just ws <- readIORef websocket
  s <- socketState ws
  when (s == 3) $ do
    socket <- createWebSocket url' ps
    liftIO (atomicWriteIORef websocket (Just socket))

-- | Retrieves current status of `WebSocket`
getSocketState :: IO SocketState
getSocketState = do
  Just ws <- liftIO (readIORef websocket)
  toEnum <$> socketState ws

sendJson' :: ToJSON json => Socket -> json -> IO ()
sendJson' socket m = FFI.send socket =<< stringify m

createWebSocket :: MisoString -> [MisoString] -> IO Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  create url' =<< toJSVal protocols

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
