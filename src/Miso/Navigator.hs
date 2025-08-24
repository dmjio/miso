-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Navigator
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
import           Language.Javascript.JSaddle
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.String
import           Miso.Effect
import qualified Miso.FFI.Internal as FFI
----------------------------------------------------------------------------
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
  -> Effect parent model action
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
  -> Effect parent model action
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
  -> Effect parent model action
isOnLine action = io (action <$> FFI.isOnLine)
-----------------------------------------------------------------------------
-- | Type for dealing with 'navigator.mediaDevices.getUserMedia'
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/mediaDevices>
--
data UserMedia
  = UserMedia
  { audio, video :: Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Default 'UserMedia'
userMedia :: UserMedia
userMedia = UserMedia True True
-----------------------------------------------------------------------------
-- | Geolocation fetching
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/geolocation>
--
geolocation
  :: (Geolocation -> action) 
  -> (GeolocationError -> action) 
  -> Effect parent model action
geolocation successful errorful = do
  withSink $ \sink ->
    FFI.geolocation
      (sink . successful <=< fromJSValUnchecked)
      (sink . errorful <=< fromJSValUnchecked)
-----------------------------------------------------------------------------
data GeolocationError = GeolocationError GeolocationErrorCode MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal GeolocationError where
  fromJSVal v = do
    code <- fromJSVal =<< (v ! "code")
    msg <- fromJSVal =<< (v ! "message")
    pure (GeolocationError <$> code <*> msg)
-----------------------------------------------------------------------------
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
data Geolocation
  = Geolocation
  { latitude, longitude, accuracy :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal Geolocation where
  fromJSVal geo = do
    lat <- fromJSVal =<< geo ! "coords" ! "latitude"
    lon <- fromJSVal =<< geo ! "coords" ! "longitude"
    acc <- fromJSVal =<< geo ! "coords" ! "accuracy"
    pure (Geolocation <$> lat <*> lon <*> acc)
-----------------------------------------------------------------------------
