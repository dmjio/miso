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
  , WebSocket   (..)
  , URL
  , SocketState (..)
  , CloseCode   (..)
  , Closed      (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.Runtime
-----------------------------------------------------------------------------
