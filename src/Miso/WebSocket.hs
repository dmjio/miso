-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/connect>
connect
  :: FromJSON json
  => URL
  -> (WebSocket -> action)
  -> (Closed -> action)
  -> (Payload json -> action)
  -> (MisoString -> action)
  -> Effect parent model action
connect = websocketConnect
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendJSON
  :: ToJSON json
  => WebSocket
  -> json
  -> Effect parent model action
sendJSON socket x = websocketSend socket (JSON x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendBLOB
  :: WebSocket
  -> Blob
  -> Effect parent model action
sendBLOB socket x = websocketSend @() socket (blob x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendArrayBuffer
  :: WebSocket
  -> ArrayBuffer
  -> Effect parent model action
sendArrayBuffer socket x = websocketSend @() socket (arrayBuffer x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
sendText
  :: WebSocket
  -> MisoString
  -> Effect parent model action
sendText socket x = websocketSend @() socket (TEXT x)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close>
close
  :: WebSocket
  -> Effect parent model action
close = websocketClose
-----------------------------------------------------------------------------
