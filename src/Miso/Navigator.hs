-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE CPP                 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Navigator
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Navigator" wraps the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator Navigator>
-- API as 'Miso.Effect.Effect' combinators that integrate directly into the
-- Model-View-Update loop. Each function returns an 'Miso.Effect.Effect' and
-- feeds its result back as an action via 'Miso.Effect.withSink' or
-- 'Miso.Effect.io'.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Navigator"
--
-- data Action
--   = RequestLocation
--   | GotLocation 'Geolocation'
--   | LocationError 'GeolocationError'
--   | RequestCamera
--   | GotStream 'Stream'
--   | MediaError 'Miso.DSL.JSVal'
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update RequestLocation =
--   'geolocation' GotLocation LocationError
-- update RequestCamera =
--   'getUserMedia' ('userMedia' { audio = False }) GotStream MediaError
-- update _ = pure ()
-- @
--
-- = API groups
--
-- * __Camera \/ microphone__ ('Navigator.mediaDevices.getUserMedia'):
--   'getUserMedia', 'userMedia', 'UserMedia', 'Stream'
-- * __Clipboard__ ('Navigator.clipboard.writeText'):
--   'copyClipboard'
-- * __Online status__ ('Navigator.onLine'):
--   'isOnLine'
-- * __Geolocation__ ('Navigator.geolocation.getCurrentPosition'):
--   'geolocation', 'Geolocation', 'GeolocationError', 'GeolocationErrorCode'
--
-- = Error handling
--
-- Geolocation errors are decoded from the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/GeolocationPositionError GeolocationPositionError>
-- object into 'GeolocationError', which carries a 'GeolocationErrorCode'
-- (@'PERMISSION_DENIED'@, @'POSITION_UNAVAILABLE'@, @'TIMEOUT'@) and a
-- human-readable message string.
--
-- = See also
--
-- * "Miso.FFI.Internal" — 'Miso.FFI.Internal.getUserMedia', 'Miso.FFI.Internal.copyClipboard',
--   'Miso.FFI.Internal.geolocation', 'Miso.FFI.Internal.isOnLine' — the raw IO primitives
-- * "Miso.Subscription.OnLine" — subscription-based online\/offline monitoring
-- * "Miso.Effect" — 'Miso.Effect.withSink', 'Miso.Effect.io'
-----------------------------------------------------------------------------
module Miso.Navigator
  ( -- ** User media
    getUserMedia
  , userMedia
  , UserMedia (..)
  , Stream
  -- ** Clipboard
  , copyClipboard
  -- ** OnLine
  , isOnLine
  -- ** Geolocation
  , geolocation
  , Geolocation (..)
  , GeolocationError (..)
  , GeolocationErrorCode (..)
  ) where
-----------------------------------------------------------------------------
import           Control.Monad ((<=<))
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.String
import           Miso.Effect
import qualified Miso.FFI.Internal as FFI
----------------------------------------------------------------------------
-- | A media stream
type Stream = JSVal
----------------------------------------------------------------------------
-- | Get access to user's media devices.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia>
--
getUserMedia
  :: UserMedia
  -- ^ Options
  -> (Stream -> action)
  -- ^ Successful callback
  -> (JSVal -> action)
  -- ^ Errorful callback
  -> Effect context props model action
getUserMedia UserMedia {..} successful errorful =
  withSink $ \sink ->
    FFI.getUserMedia audio video
      (sink . successful)
      (sink . errorful)
-----------------------------------------------------------------------------
-- | Get access to the user's clipboard.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/clipboard>
--
copyClipboard
  :: MisoString
  -- ^ Options
  -> action
  -- ^ Successful callback
  -> (JSVal -> action)
  -- ^ Errorful callback
  -> Effect context props model action
copyClipboard txt successful errorful =
  withSink $ \sink ->
    FFI.copyClipboard txt
      (sink successful)
      (sink . errorful)
-----------------------------------------------------------------------------
-- | Get user's online status
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/onLine>
--
isOnLine
  :: (Bool -> action)
  -- ^ Successful callback
  -> Effect context props model action
isOnLine action = io (action <$> FFI.isOnLine)
-----------------------------------------------------------------------------
-- | Type for dealing with 'navigator.mediaDevices.getUserMedia'
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/mediaDevices>
--
data UserMedia
  = UserMedia
  { audio :: Bool
  -- ^ Request access to the user's microphone
  , video :: Bool
  -- ^ Request access to the user's camera
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Default t'UserMedia'
userMedia :: UserMedia
userMedia = UserMedia True True
-----------------------------------------------------------------------------
-- | Geolocation fetching
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/geolocation>
--
geolocation
  :: (Geolocation -> action)
  -- ^ Success callback; receives the device's current position
  -> (GeolocationError -> action)
  -- ^ Error callback; receives a 'GeolocationError' with code and message
  -> Effect context props model action
geolocation successful errorful = do
  withSink $ \sink ->
    FFI.geolocation
      (sink . successful <=< fromJSValUnchecked)
      (sink . errorful <=< fromJSValUnchecked)
-----------------------------------------------------------------------------
-- | Geolocation errors
data GeolocationError = GeolocationError GeolocationErrorCode MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal GeolocationError where
  fromJSVal v = do
    code <- fromJSVal =<< (v ! "code")
    msg <- fromJSVal =<< (v ! "message")
    pure (GeolocationError <$> code <*> msg)
-----------------------------------------------------------------------------
-- | Geolocation error code
data GeolocationErrorCode
  = PERMISSION_DENIED
  | POSITION_UNAVAILABLE
  | TIMEOUT
  deriving (Enum, Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal GeolocationErrorCode where
  fromJSVal code =
    fromJSValUnchecked code >>= \case
      (1 :: Int) -> pure (Just PERMISSION_DENIED)
      2 -> pure (Just POSITION_UNAVAILABLE)
      3 -> pure (Just TIMEOUT)
      _ -> pure Nothing
-----------------------------------------------------------------------------
-- | Geolocation holds latitude, longitude and accuracy, among others.
data Geolocation
  = Geolocation
  { latitude :: Double
  -- ^ Latitude in decimal degrees
  , longitude :: Double
  -- ^ Longitude in decimal degrees
  , accuracy :: Double
  -- ^ Accuracy of the position in metres (95% confidence radius)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal Geolocation where
  fromJSVal geo = do
    lat <- fromJSVal =<< geo ! "coords" ! "latitude"
    lon <- fromJSVal =<< geo ! "coords" ! "longitude"
    acc <- fromJSVal =<< geo ! "coords" ! "accuracy"
    pure (Geolocation <$> lat <*> lon <*> acc)
-----------------------------------------------------------------------------
