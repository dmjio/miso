-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.EventSource
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.EventSource
  ( -- *** EventSource
    connect
  , close
  , socketState
  -- *** Defaults
  , emptyEventSource
  -- *** Types
  , EventSource (..)
  , URL
  -- *** Re-exports
  , Payload (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
-----------------------------------------------------------------------------
import           Miso.Effect
import           Miso.Runtime
import           Miso.String
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
connect
  :: FromJSON value
  => URL
  -> (EventSource -> action)
  -- ^ onOpen
  -> (Payload value -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect parent model action
connect = eventSourceConnect
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close>
close
  :: EventSource
  -> Effect parent model action
close = eventSourceClose
-----------------------------------------------------------------------------
