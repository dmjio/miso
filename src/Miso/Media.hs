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
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Event
import           Miso.String
-----------------------------------------------------------------------------
-- | Type that abstracts over [Audio](https://www.w3schools.com/jsref/dom_obj_audio.asp)
-- or [Video](https://www.w3schools.com/jsref/dom_obj_video.asp) media objects.
--
-- You can create them in the View using 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_'.
-- To get the corresponding t'Media' object from the DOM, you can use
--
-- @
-- media <- t'Media' \<$\> 'Miso.FFI.getElementById' "myVideo"
-- @
newtype Media = Media JSVal
  deriving (ToJSVal)
-----------------------------------------------------------------------------
-- | Possible values of [networkState](https://www.w3schools.com/tags/av_prop_networkstate.asp) property.
data NetworkState
  = NETWORK_EMPTY
  | NETWORK_IDLE
  | NETWORK_LOADING
  | NETWORK_NO_SOURCE
  deriving (Show, Eq, Enum)
-----------------------------------------------------------------------------
-- | Possible values of [readyState](https://www.w3schools.com/tags/av_prop_readystate.asp) property.
data ReadyState
  = HAVE_NOTHING
  | HAVE_METADATA
  | HAVE_CURRENT_DATA
  | HAVE_FUTURE_DATA
  | HAVE_ENOUGH_DATA
  deriving (Show, Eq, Enum)
-----------------------------------------------------------------------------
-- | The [load](https://www.w3schools.com/tags/av_met_load.asp) method
-- re-loads the audio/video element.
load :: Media -> IO ()
load (Media m) = void $ m # ("load" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | The [play](https://www.w3schools.com/tags/av_met_play.asp) method starts
-- playing the current audio or video.
play :: Media -> IO ()
play (Media m) = void $ m # ("play" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | The [pause](https://www.w3schools.com/tags/av_met_pause.asp) method pauses
-- the currently playing audio or video.
pause :: Media -> IO ()
pause (Media a) = void $ a # ("pause" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | The [canPlayType](https://www.w3schools.com/tags/av_met_canplaytype.asp)
-- method checks if the browser can play the specified audio/video type.
canPlayType :: Media -> IO MisoString
canPlayType (Media m) = do
  fromJSValUnchecked =<< do
    m # ("canPlayType" :: MisoString) $ ()
-----------------------------------------------------------------------------
-- | The [autoplay](https://www.w3schools.com/tags/av_prop_autoplay.asp) property
-- returns whether the audio/video should start playing as soon as it is loaded.
--
-- To set the property, use 'Miso.Html.Property.autoplay_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
autoplay :: Media -> IO Bool
autoplay (Media m) = fromJSValUnchecked =<< m ! ("autoplay" :: MisoString)
-----------------------------------------------------------------------------
-- | The [controls](https://www.w3schools.com/tags/av_prop_controls.asp)
-- property returns whether the browser should display standard audio/video controls.
--
-- To set the property, use 'Miso.Html.Property.controls_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
controls :: Media -> IO Bool
controls (Media m) = fromJSValUnchecked =<< m ! ("controls" :: MisoString)
-----------------------------------------------------------------------------
-- | The [currentSrc](https://www.w3schools.com/tags/av_prop_currentsrc.asp)
-- property returns the URL of the current audio/video.
currentSrc :: Media -> IO MisoString
currentSrc (Media m) = fromJSValUnchecked =<< m ! ("currentSrc" :: MisoString)
-----------------------------------------------------------------------------
-- | The [currentTime](https://www.w3schools.com/tags/av_prop_currenttime.asp)
-- property returns the current position (in seconds) of the audio/video playback.
--
-- To set the current time, use 'Miso.Html.Property.currentTime_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
currentTime :: Media -> IO Double
currentTime (Media m) = fromJSValUnchecked =<< m ! ("currentTime" :: MisoString)
-----------------------------------------------------------------------------
-- | The [defaultMuted](https://www.w3schools.com/tags/av_prop_defaultmuted.asp)
-- property returns whether the audio/video should be muted by default.
--
-- To set the property, use 'Miso.Html.Property.defaultMuted_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
defaultMuted :: Media -> IO Bool
defaultMuted (Media m) = fromJSValUnchecked =<< m ! ("defaultMuted" :: MisoString)
-----------------------------------------------------------------------------
-- | The [defaultPlaybackRate](https://www.w3schools.com/tags/av_prop_defaultplaybackrate.asp)
-- property returns the default playback speed of the audio/video.
--
-- To set the property, use 'Miso.Html.Property.defaultPlaybackRate_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
defaultPlaybackRate :: Media -> IO Double
defaultPlaybackRate (Media m) = fromJSValUnchecked =<< m ! ("defaultPlaybackRate" :: MisoString)
-----------------------------------------------------------------------------
-- | The [duration](https://www.w3schools.com/tags/av_prop_duration.asp) property
-- returns the length of the current audio/video, in seconds.
duration :: Media -> IO Double
duration (Media m) = fromJSValUnchecked =<< m ! ("duration" :: MisoString)
-----------------------------------------------------------------------------
-- | The [ended](https://www.w3schools.com/tags/av_prop_ended.asp) property
-- returns whether the playback of the audio/video has ended.
ended :: Media -> IO Bool
ended (Media m) = fromJSValUnchecked =<< m ! ("ended" :: MisoString)
-----------------------------------------------------------------------------
-- | The [loop](https://www.w3schools.com/tags/av_prop_loop.asp) property
-- returns whether the audio/video should start playing over again when it is finished.
--
-- To set the property, use 'Miso.Html.Property.loop_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
loop :: Media -> IO Bool
loop (Media m) = fromJSValUnchecked =<< m ! ("loop" :: MisoString)
-----------------------------------------------------------------------------
-- | The [mediaGroup](https://www.w3schools.com/tags/av_prop_mediagroup.asp) property
-- returns the name of the media group the audio/video is a part of.
--
-- To set the property, use 'Miso.Html.Property.mediaGroup_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
mediaGroup :: Media -> IO MisoString
mediaGroup (Media m) = fromJSValUnchecked =<< m ! ("mediaGroup" :: MisoString)
-----------------------------------------------------------------------------
-- | The [muted](https://www.w3schools.com/tags/av_prop_muted.asp) property
-- returns whether the audio/video should be muted (sound turned off).
--
-- To set the property, use 'Miso.Html.Property.muted_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
muted :: Media -> IO Bool
muted (Media m) = fromJSValUnchecked =<< m ! ("muted" :: MisoString)
-----------------------------------------------------------------------------
-- | The [networkState](https://www.w3schools.com/tags/av_prop_networkstate.asp)
-- property returns the current network state (activity) of the audio/video.
networkState :: Media -> IO NetworkState
networkState (Media m) = do
  number <- fromJSValUnchecked =<< m ! ("networkState" :: MisoString)
  pure (toEnum number)
-----------------------------------------------------------------------------
-- | The [paused](https://www.w3schools.com/tags/av_prop_paused.asp) property
-- returns whether the audio/video is paused.
paused :: Media -> IO Bool
paused (Media a) = fromJSValUnchecked =<< a ! ("paused" :: MisoString)
-----------------------------------------------------------------------------
-- | The [playbackRate](https://www.w3schools.com/tags/av_prop_playbackRate.asp)
-- property returns the current playback speed of the audio/video.
--
-- To set the playback rate, use 'Miso.Html.Property.playbackRate_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
playbackRate :: Media -> IO Double
playbackRate (Media a) = fromJSValUnchecked =<< a ! ("playbackRate" :: MisoString)
-----------------------------------------------------------------------------
-- | The [poster](https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/poster) property
-- of the HTMLVideoElement interface is a string that reflects the URL for an image
-- to be shown while no video data is available.
--
-- Specific to videos.
poster :: Media -> IO MisoString
poster (Media a) = fromJSValUnchecked =<< a ! ("poster" :: MisoString)
-----------------------------------------------------------------------------
-- | The [preload](https://www.w3schools.com/tags/av_prop_preload.asp) property
-- returns whether the audio/video should start loading as soon as the page loads.
--
-- To set the preload property, use 'Miso.Html.Property.preload_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
preload :: Media -> IO MisoString
preload (Media a) = fromJSValUnchecked =<< a ! ("preload" :: MisoString)
-----------------------------------------------------------------------------
-- | The [readyState](https://www.w3schools.com/tags/av_prop_readyState.asp) property
-- returns the current ready state of the audio/video.
readyState :: Media -> IO ReadyState
readyState (Media a) = do
  number <- fromJSValUnchecked =<< a ! ("readyState" :: MisoString)
  pure (toEnum number)
-----------------------------------------------------------------------------
-- | The [seeking](https://www.w3schools.com/tags/av_prop_seeking.asp) property
-- returns whether the user is currently seeking in the audio/video.
seeking :: Media -> IO Bool
seeking (Media a) = fromJSValUnchecked =<< a ! ("seeking" :: MisoString)
-----------------------------------------------------------------------------
-- | The HTMLVideoElement interface's read-only
-- [videoHeight](https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/videoHeight)
-- property indicates the intrinsic height of the video, expressed in CSS pixels.
videoHeight :: Media -> IO Int
videoHeight (Media m) = fromJSValUnchecked =<< m ! ("videoHeight" :: MisoString)
-----------------------------------------------------------------------------
-- | The HTMLVideoElement interface's read-only
-- [videoWidth](https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/videoWidth)
-- property indicates the intrinsic width of the video, expressed in CSS pixels
videoWidth :: Media -> IO Int
videoWidth (Media m) = fromJSValUnchecked =<< m ! ("videoWidth" :: MisoString)
-----------------------------------------------------------------------------
-- | The [volume](https://www.w3schools.com/tags/av_prop_volume.asp) property
-- returns the current volume of the audio/video.
--
-- To set the volume, use 'Miso.Html.Property.volume_'
-- on the 'Miso.Html.Element.audio_' or 'Miso.Html.Element.video_' element.
volume :: Media -> IO Double
volume (Media m) = fromJSValUnchecked =<< m ! ("volume" :: MisoString)
-----------------------------------------------------------------------------
-- | A media Stream
type Stream = JSVal
-----------------------------------------------------------------------------
-- | Sets the `srcObject` on audio or video elements.
srcObject :: Stream -> Media -> IO ()
srcObject stream (Media media) = setField media "srcObject" stream
-----------------------------------------------------------------------------
