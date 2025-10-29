-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Media
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Media
  ( -- *** Types
    Media        (..)
  , NetworkState (..)
  , ReadyState   (..)
  , Stream
  -- *** Constructors
  , newAudio
  -- *** Methods
  , canPlayType
  , load
  , play
  , pause
  , srcObject
  -- *** Properties
  , autoplay
  , controls
  , currentSrc
  , currentTime
  , defaultMuted
  , defaultPlaybackRate
  , duration
  , ended
  , loop
  , mediaGroup
  , muted
  , networkState
  , paused
  , playbackRate
  , poster
  , preload
  , readyState
  , seeking
  , videoHeight
  , videoWidth
  , volume
  -- *** Event Map
  , mediaEvents
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
import           Language.Javascript.JSaddle hiding (new)
import qualified Language.Javascript.JSaddle as JS
-----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Event
import           Miso.String
-----------------------------------------------------------------------------
-- | Type that abstracts over [Audio](https://www.w3schools.com/jsref/dom_obj_audio.asp) or [Video](https://www.w3schools.com/jsref/dom_obj_video.asp) media objects
newtype Media = Media JSVal
  deriving (ToJSVal)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_networkstate.asp
data NetworkState
  = NETWORK_EMPTY
  | NETWORK_IDLE
  | NETWORK_LOADING
  | NETWORK_NO_SOURCE
  deriving (Show, Eq, Enum)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_readystate.asp
data ReadyState
  = HAVE_NOTHING
  | HAVE_METADATA
  | HAVE_CURRENT_DATA
  | HAVE_FUTURE_DATA
  | HAVE_ENOUGH_DATA
  deriving (Show, Eq, Enum)
-----------------------------------------------------------------------------
-- | Smart constructor for an audio @Media@ with 'Miso.Html.Property.src_' element
newAudio :: MisoString -> JSM Media
newAudio url = do
  a <- JS.new (jsg ("Audio" :: MisoString)) ([] :: [MisoString])
  o <- makeObject a
  FFI.set ("src" :: MisoString) url o
  pure (Media a)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_met_load.asp
load :: Media -> JSM ()
load (Media m) = void $ m # ("load" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_met_play.asp
play :: Media -> JSM ()
play (Media m) = void $ m # ("play" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_met_pause.asp
pause :: Media -> JSM ()
pause (Media a) = void $ a # ("pause" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_met_canplaytype.asp
canPlayType :: Media -> JSM MisoString
canPlayType (Media m) = do
  fromJSValUnchecked =<< do
    m # ("canPlayType" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_autoplay.asp
autoplay :: Media -> JSM Bool
autoplay (Media m) = fromJSValUnchecked =<< m ! ("autoplay" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_controls.asp
controls :: Media -> JSM Bool
controls (Media m) = fromJSValUnchecked =<< m ! ("controls" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_currentsrc.asp
currentSrc :: Media -> JSM MisoString
currentSrc (Media m) = fromJSValUnchecked =<< m ! ("currentSrc" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_currenttime.asp
currentTime :: Media -> JSM Double
currentTime (Media m) = fromJSValUnchecked =<< m ! ("currentTime" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_defaultmuted.asp
defaultMuted :: Media -> JSM Bool
defaultMuted (Media m) = fromJSValUnchecked =<< m ! ("defaultMuted" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_defaultplaybackrate.asp
defaultPlaybackRate :: Media -> JSM Double
defaultPlaybackRate (Media m) = fromJSValUnchecked =<< m ! ("defaultPlaybackRate" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_duration.asp
duration :: Media -> JSM Double
duration (Media m) = fromJSValUnchecked =<< m ! ("duration" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_ended.asp
ended :: Media -> JSM Bool
ended (Media m) = fromJSValUnchecked =<< m ! ("ended" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_loop.asp
loop :: Media -> JSM Bool
loop (Media m) = fromJSValUnchecked =<< m ! ("loop" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_mediagroup.asp
mediaGroup :: Media -> JSM MisoString
mediaGroup (Media m) = fromJSValUnchecked =<< m ! ("mediaGroup" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_muted.asp
muted :: Media -> JSM Bool
muted (Media m) = fromJSValUnchecked =<< m ! ("muted" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_networkstate.asp
networkState :: Media -> JSM NetworkState
networkState (Media m) = do
  number <- fromJSValUnchecked =<< m ! ("networkState" :: MisoString)
  pure (toEnum number)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_paused.asp
paused :: Media -> JSM Bool
paused (Media a) = fromJSValUnchecked =<< a ! ("paused" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_playbackRate.asp
playbackRate :: Media -> JSM Double
playbackRate (Media a) = fromJSValUnchecked =<< a ! ("playbackRate" :: MisoString)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/poster
--
-- Specific to videos.
poster :: Media -> JSM MisoString
poster (Media a) = fromJSValUnchecked =<< a ! ("poster" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_preload.asp
preload :: Media -> JSM MisoString
preload (Media a) = fromJSValUnchecked =<< a ! ("preload" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_readyState.asp
readyState :: Media -> JSM ReadyState
readyState (Media a) = do
  number <- fromJSValUnchecked =<< a ! ("readyState" :: MisoString)
  pure (toEnum number)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_seeking.asp
seeking :: Media -> JSM Bool
seeking (Media a) = fromJSValUnchecked =<< a ! ("seeking" :: MisoString)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/videoHeight
--
-- Specific to videos.
videoHeight :: Media -> JSM Int
videoHeight (Media m) = fromJSValUnchecked =<< m ! ("videoHeight" :: MisoString)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/videoWidth
--
-- Specific to videos.
videoWidth :: Media -> JSM Int
videoWidth (Media m) = fromJSValUnchecked =<< m ! ("videoWidth" :: MisoString)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_prop_volume.asp
volume :: Media -> JSM Double
volume (Media m) = fromJSValUnchecked =<< m ! ("volume" :: MisoString)
-----------------------------------------------------------------------------
-- | A media Stream
type Stream = JSVal
-----------------------------------------------------------------------------
-- | Sets the `srcObject` on audio or video elements.
srcObject :: Stream -> Media -> JSM ()
srcObject stream (Media media) = media <# ("srcObject" :: MisoString) $ stream
-----------------------------------------------------------------------------
