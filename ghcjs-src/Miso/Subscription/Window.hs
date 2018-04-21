{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Window
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Window where

import Control.Monad
import Data.Monoid

import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

import Miso.Event
import Miso.FFI
import Miso.Html.Internal         ( Sub )
import Miso.String

import Data.Aeson.Types (parseEither)

-- | Captures window coordinates changes as they occur and writes them to
-- an event sink
windowCoordsSub :: ((Int, Int) -> action) -> Sub action
windowCoordsSub f = \sink -> do
  sink . f =<< (,) <$> windowInnerHeight <*> windowInnerWidth
  windowAddEventListener "resize" =<< do
    asyncCallback1 $ \windowEvent -> do
      target <- getProp "target" (Object windowEvent)
      Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
      Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
      sink $ f (h, w)

-- | @windowOn eventName decoder toAction@ is a subscription which parallels the
-- attribute handler `on`, providing a subscription to listen to window level events.
windowSub :: MisoString -> Decoder r -> (r -> action) -> Sub action
windowSub  = windowSubWithOptions defaultOptions

windowSubWithOptions :: Options -> MisoString -> Decoder r -> (r -> action) -> Sub action
windowSubWithOptions Options{..} eventName Decoder{..} toAction = \sink -> do
  windowAddEventListener eventName =<< do
    decodeAtVal <- toJSVal decodeAt
    asyncCallback1 $ \e -> do
      Just v <- jsvalToValue =<< objectToJSON decodeAtVal e
      case parseEither decoder v of
        Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
        Right r -> do
          when stopPropagation $ eventStopPropagation e
          when preventDefault $ eventPreventDefault e
          sink (toAction r)
