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
  ( -- *** Types
    WebSocket   (..)
  , URL         (..)
  , Protocols   (..)
  , SocketState (..)
  , CloseCode   (..)
  , WasClean    (..)
  , Reason      (..)
    -- *** Subscription
  , websocketSub
  , send
  , close
  , connect
  , getSocketState
  ) where
-----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (when, void, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicWriteIORef)
import           Language.Javascript.JSaddle
import           System.IO.Unsafe (unsafePerformIO)
-----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import           Miso.FFI (forkJSM, jsonParse, jsonStringify)
import           Miso.FFI.WebSocket (Socket)
import qualified Miso.FFI.WebSocket as WS
import           Miso.String (MisoString)
import           Miso.WebSocket
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
  void . forkJSM $ handleReconnect
  WS.addEventListener socket "open" $ \_ -> do
    liftIO (writeIORef closedCode Nothing)
    sink (f WebSocketOpen)
  WS.addEventListener socket "message" $ \v -> do
    d <- jsonParse =<< WS.data' v
    sink $ f (WebSocketMessage d)
  WS.addEventListener socket "close" $ \e -> do
    code <- codeToCloseCode <$> WS.code e
    liftIO (writeIORef closedCode (Just code))
    reason <- WS.reason e
    clean <- WS.wasClean e
    sink $ f (WebSocketClose code clean reason)
  WS.addEventListener socket "error" $ \v -> do
    liftIO (writeIORef closedCode Nothing)
    d' <- WS.data' v
#ifndef ghcjs_HOST_OS
    undef <- ghcjsPure (isUndefined d')
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
      status <- WS.socketState s
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
close =
  mapM_ WS.close =<<
    liftIO (readIORef websocket)
-----------------------------------------------------------------------------
-- | Connects to a websocket server
connect :: URL -> Protocols -> JSM ()
{-# INLINE connect #-}
connect (URL url') (Protocols ps) = do
  Just ws <- liftIO (readIORef websocket)
  s <- WS.socketState ws
  when (s == 3) $ do
    socket <- createWebSocket url' ps
    liftIO (atomicWriteIORef websocket (Just socket))
-----------------------------------------------------------------------------
-- | Retrieves current status of `WebSocket`
getSocketState :: JSM SocketState
getSocketState = do
  Just ws <- liftIO (readIORef websocket)
  toEnum <$> WS.socketState ws
-----------------------------------------------------------------------------
sendJson' :: ToJSON json => Socket -> json -> JSM ()
sendJson' socket m = WS.send socket =<< jsonStringify m
-----------------------------------------------------------------------------
createWebSocket :: MisoString -> [MisoString] -> JSM Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  WS.create url' =<< toJSVal protocols
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
