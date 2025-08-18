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
  ( -- *** WebSocket
    connect
  , send
  , close
  , socketState
  -- *** Defaults
  , emptyWebSocket
  -- *** Types
  , WebSocket
  , URL
  , SocketState (..)
  , CloseCode   (..)
  , Closed      (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.Effect
import           Miso.Runtime
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/connect>
connect
  :: URL
  -> (WebSocket -> action)
  -> (Closed -> action)
  -> (JSVal -> action)
  -> (JSVal -> action)
  -> Effect parent model action
connect = websocketConnect
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
send
  :: WebSocket
  -> JSVal
  -> Effect parent model action
send = websocketSend
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close>
close
  :: WebSocket
  -> Effect parent model action
close = websocketClose
-----------------------------------------------------------------------------
