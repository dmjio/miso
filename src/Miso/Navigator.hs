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
----------------------------------------------------------------------------
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
  -> Effect parent props model action
getUserMedia UserMedia {..} successful errorful =
  withSink $ \sink ->
    FFI.getUserMedia audio video
      (sink . successful)
      (sink . errorful)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText>
--
-- Copies text to the system clipboard.
copyClipboard
  :: MisoString
  -- ^ Text to write to the clipboard
  -> action
  -- ^ Action dispatched on success
  -> (JSVal -> action)
  -- ^ Action dispatched with the error object on failure
  -> Effect parent props model action
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
  -> Effect parent props model action
isOnLine action = io (action <$> FFI.isOnLine)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia>
--
-- Constraints controlling which media devices are requested.
data UserMedia
  = UserMedia
  { audio :: Bool
  -- ^ @True@ to request microphone access
  , video :: Bool
  -- ^ @True@ to request camera access
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Default t'UserMedia'
userMedia :: UserMedia
userMedia = UserMedia True True
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Geolocation/getCurrentPosition>
--
-- Requests the device's current geographic position.
geolocation
  :: (Geolocation -> action)
  -- ^ Success callback receiving the t'Geolocation' result
  -> (GeolocationError -> action)
  -- ^ Error callback receiving a t'GeolocationError'
  -> Effect parent props model action
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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/GeolocationPositionError/code>
--
-- Numeric error code returned by the Geolocation API.
data GeolocationErrorCode
  = PERMISSION_DENIED
  -- ^ The user denied permission to use geolocation
  | POSITION_UNAVAILABLE
  -- ^ The position could not be determined
  | TIMEOUT
  -- ^ The request timed out before a position was obtained
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
-- | Result from the Geolocation API.
data Geolocation
  = Geolocation
  { latitude :: Double
  -- ^ Latitude in decimal degrees
  , longitude :: Double
  -- ^ Longitude in decimal degrees
  , accuracy :: Double
  -- ^ Accuracy of the position in metres
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal Geolocation where
  fromJSVal geo = do
    lat <- fromJSVal =<< geo ! "coords" ! "latitude"
    lon <- fromJSVal =<< geo ! "coords" ! "longitude"
    acc <- fromJSVal =<< geo ! "coords" ! "accuracy"
    pure (Geolocation <$> lat <*> lon <*> acc)
-----------------------------------------------------------------------------
