{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Geolocation
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Geolocation
  ( geoSub
  , defGeoOptions
  , GeoOptions (..)
  , Geo (..)
  ) where

import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.Object
import JavaScript.Object.Internal

import Miso.Html.Internal         ( Sub )
import Miso.String

data GeoOptions = GeoOptions
  { enableHighAccuracy :: Bool
  , maximumAge :: Int
  , timeout :: Int
  } deriving (Show, Eq)

instance ToJSVal GeoOptions where
  toJSVal GeoOptions {..} = do
    o@(Object j) <- create
    toJSVal enableHighAccuracy >>= \e ->
      setProp "enableHighAccuracy" e o
    toJSVal maximumAge >>= \m ->
      setProp "maximumAge" m o
    toJSVal timeout >>= \t ->
      setProp "timeout" t o
    pure j

defGeoOptions :: GeoOptions
defGeoOptions = GeoOptions True 30000 27000

-- | GeoLocation API
-- https://developer.mozilla.org/en-US/docs/Web/API/Geolocation
data Geo
  = GeoError Code Message
  | GeoCoords Latitude Longitude
  deriving (Show, Eq)

type Code = Int
type Message = MisoString
type Latitude = Double
type Longitude = Double

geoSub :: GeoOptions -> (Geo -> action) -> Sub action model
geoSub options f _ = \sink -> do
  successCb <- asyncCallback1 $ \geoObj -> do
    lat <- getLat (Object geoObj)
    long <- getLong (Object geoObj)
    sink . f $ GeoCoords lat long
  failCb <- asyncCallback1 $ \geoObj -> do
    Just x <- fromJSVal =<< getProp "code" (Object geoObj)
    Just y <- fromJSVal =<< getProp "message" (Object geoObj)
    sink . f $ GeoError x y
  toJSVal options >>= watchPosition successCb failCb . Object

foreign import javascript unsafe "$r = $1.coords.latitude"
  getLat :: Object -> IO Double

foreign import javascript unsafe "$r = $1.coords.longitude"
  getLong :: Object -> IO Double

foreign import javascript unsafe "navigator.geolocation.watchPosition($1, $2, $3);"
  watchPosition
    :: Callback (JSVal -> IO ())
    -> Callback (JSVal -> IO ())
    -> Object
    -> IO ()
