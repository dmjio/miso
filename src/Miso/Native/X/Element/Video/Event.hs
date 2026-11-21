-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Video.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Video.Event
  ( -- *** Events
    onFirstFrame
  , onFirstFrameWith
  , onFirstFrameMain
  , onFirstFrameMainWith
  , onPlaying
  , onPlayingWith
  , onPlayingMain
  , onPlayingMainWith
  , onPaused
  , onPausedWith
  , onPausedMain
  , onPausedMainWith
  , onStopped
  , onStoppedWith
  , onStoppedMain
  , onStoppedMainWith
  , onTimeUpdate
  , onTimeUpdateWith
  , onTimeUpdateMain
  , onTimeUpdateMainWith
  , onEnded
  , onEndedWith
  , onEndedMain
  , onEndedMainWith
  , onLooped
  , onLoopedWith
  , onLoopedMain
  , onLoopedMainWith
  , onError
  , onErrorWith
  , onErrorMain
  , onErrorMainWith
  , onBuffering
  , onBufferingWith
  , onBufferingMain
  , onBufferingMainWith
    -- *** Types
  , VideoFirstFrameEvent (..)
  , VideoTimeUpdateEvent (..)
  , VideoErrorEvent (..)
  , VideoBufferingEvent (..)
    -- *** Decoders
  , firstFrameDecoder
  , timeUpdateDecoder
  , videoErrorDecoder
  , bufferingDecoder
    -- *** Event Map
  , videoEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
-- | The 'Events' map for the Lynx @<video>@ element.
--
-- Combine with other element maps using @<>@ and pass the result to
-- 'Miso.Native.native', so the delegator listens for these events.
--
-- @since 1.13.0.0
videoEvents :: Events
videoEvents
  = M.fromList
  [ ("firstframe", BUBBLE)
  , ("playing", BUBBLE)
  , ("paused", BUBBLE)
  , ("stopped", BUBBLE)
  , ("timeupdate", BUBBLE)
  , ("ended", BUBBLE)
  , ("looped", BUBBLE)
  , ("error", BUBBLE)
  , ("buffering", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @bindfirstframe@ event.
newtype VideoFirstFrameEvent
  = VideoFirstFrameEvent
  { firstFrameDuration :: Double
    -- ^ Total video duration, in seconds.
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Payload of the @bindtimeupdate@ event.
data VideoTimeUpdateEvent
  = VideoTimeUpdateEvent
  { current :: Double
    -- ^ Current playback position, in seconds.
  , duration :: Double
    -- ^ Total video duration, in seconds.
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Payload of the @binderror@ event.
data VideoErrorEvent
  = VideoErrorEvent
  { errorCode :: Int
    -- ^ Platform playback error code.
  , errorMsg :: MisoString
    -- ^ Platform playback error message.
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Payload of the @bindbuffering@ event.
newtype VideoBufferingEvent
  = VideoBufferingEvent
  { buffering :: Double
    -- ^ Buffered end position on the timeline, in seconds (i.e. the maximum
    -- playable position at the moment).
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'VideoFirstFrameEvent' from the raw Lynx event payload.
--
-- @since 1.13.0.0
firstFrameDecoder :: Decoder VideoFirstFrameEvent
firstFrameDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      VideoFirstFrameEvent <$> o .: "duration"
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'VideoTimeUpdateEvent' from the raw Lynx event payload.
--
-- @since 1.13.0.0
timeUpdateDecoder :: Decoder VideoTimeUpdateEvent
timeUpdateDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      VideoTimeUpdateEvent
        <$> o .: "current"
        <*> o .: "duration"
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'VideoErrorEvent' from the raw Lynx event payload.
--
-- @since 1.13.0.0
videoErrorDecoder :: Decoder VideoErrorEvent
videoErrorDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      VideoErrorEvent
        <$> o .: "errorCode"
        <*> o .: "errorMsg"
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'VideoBufferingEvent' from the raw Lynx event payload.
--
-- @since 1.13.0.0
bufferingDecoder :: Decoder VideoBufferingEvent
bufferingDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      VideoBufferingEvent <$> o .: "buffering"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindfirstframe
--
-- Fired when the first video frame has loaded. If a seek operation occurs
-- before the first frame, this event fires when the first frame after the
-- seek is available.
--
onFirstFrame :: (VideoFirstFrameEvent -> action) -> Attribute model action
onFirstFrame action = on "firstframe" firstFrameDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onFirstFrame', but the handler also receives the target element's 'DOMRef'.
onFirstFrameWith :: (VideoFirstFrameEvent -> DOMRef -> action) -> Attribute model action
onFirstFrameWith action = on "firstframe" firstFrameDecoder $ \e _ domRef -> action e domRef
-----------------------------------------------------------------------------
-- | Like 'onFirstFrame', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = FirstFrame VideoFirstFrameEvent
--
-- view_ [ event (static (onFirstFrameMain FirstFrame)) ] [ "some view" ]
-- @
--
onFirstFrameMain :: (VideoFirstFrameEvent -> action) -> EventHandler model action
onFirstFrameMain action = onMain "firstframe" firstFrameDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onFirstFrameMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = FirstFrame VideoFirstFrameEvent Model DOMRef
--
-- view_ [ event (static (onFirstFrameMainWith FirstFrame)) ] [ "some view" ]
-- @
--
onFirstFrameMainWith :: (VideoFirstFrameEvent -> model -> DOMRef -> action) -> EventHandler model action
onFirstFrameMainWith action = onMain "firstframe" firstFrameDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindplaying
--
-- Fired when video playback starts. This fires for the first playback and
-- when playback resumes after a pause. It does not fire again when looping
-- automatically restarts playback from the beginning.
--
onPlaying :: action -> Attribute model action
onPlaying action = on "playing" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onPlaying', but the handler also receives the target element's 'DOMRef'.
onPlayingWith :: (DOMRef -> action) -> Attribute model action
onPlayingWith action = on "playing" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onPlaying', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Playing
--
-- view_ [ event (static (onPlayingMain Playing)) ] [ "some view" ]
-- @
--
onPlayingMain :: action -> EventHandler model action
onPlayingMain action = onMain "playing" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onPlayingMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Playing Model DOMRef
--
-- view_ [ event (static (onPlayingMainWith Playing)) ] [ "some view" ]
-- @
--
onPlayingMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onPlayingMainWith action = onMain "playing" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindpaused
--
-- Fired when video playback pauses.
--
onPaused :: action -> Attribute model action
onPaused action = on "paused" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onPaused', but the handler also receives the target element's 'DOMRef'.
onPausedWith :: (DOMRef -> action) -> Attribute model action
onPausedWith action = on "paused" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onPaused', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Paused
--
-- view_ [ event (static (onPausedMain Paused)) ] [ "some view" ]
-- @
--
onPausedMain :: action -> EventHandler model action
onPausedMain action = onMain "paused" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onPausedMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Paused Model DOMRef
--
-- view_ [ event (static (onPausedMainWith Paused)) ] [ "some view" ]
-- @
--
onPausedMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onPausedMainWith action = onMain "paused" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindstopped
--
-- Fired only when video playback is stopped by the @stop@ method.
--
onStopped :: action -> Attribute model action
onStopped action = on "stopped" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onStopped', but the handler also receives the target element's 'DOMRef'.
onStoppedWith :: (DOMRef -> action) -> Attribute model action
onStoppedWith action = on "stopped" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onStopped', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Stopped
--
-- view_ [ event (static (onStoppedMain Stopped)) ] [ "some view" ]
-- @
--
onStoppedMain :: action -> EventHandler model action
onStoppedMain action = onMain "stopped" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onStoppedMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Stopped Model DOMRef
--
-- view_ [ event (static (onStoppedMainWith Stopped)) ] [ "some view" ]
-- @
--
onStoppedMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onStoppedMainWith action = onMain "stopped" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindtimeupdate
--
-- Fired when the playback position updates, aligned with Web @\<video\>@
-- @timeupdate@ semantics. Throttled by the @timeupdateInterval_@ property.
--
onTimeUpdate :: (VideoTimeUpdateEvent -> action) -> Attribute model action
onTimeUpdate action = on "timeupdate" timeUpdateDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onTimeUpdate', but the handler also receives the target element's 'DOMRef'.
onTimeUpdateWith :: (VideoTimeUpdateEvent -> DOMRef -> action) -> Attribute model action
onTimeUpdateWith action = on "timeupdate" timeUpdateDecoder $ \e _ domRef -> action e domRef
-----------------------------------------------------------------------------
-- | Like 'onTimeUpdate', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = TimeUpdate VideoTimeUpdateEvent
--
-- view_ [ event (static (onTimeUpdateMain TimeUpdate)) ] [ "some view" ]
-- @
--
onTimeUpdateMain :: (VideoTimeUpdateEvent -> action) -> EventHandler model action
onTimeUpdateMain action = onMain "timeupdate" timeUpdateDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onTimeUpdateMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = TimeUpdate VideoTimeUpdateEvent Model DOMRef
--
-- view_ [ event (static (onTimeUpdateMainWith TimeUpdate)) ] [ "some view" ]
-- @
--
onTimeUpdateMainWith :: (VideoTimeUpdateEvent -> model -> DOMRef -> action) -> EventHandler model action
onTimeUpdateMainWith action = onMain "timeupdate" timeUpdateDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindended
--
-- Fired when video playback fully ends. This does not fire when @loop_ True@.
--
onEnded :: action -> Attribute model action
onEnded action = on "ended" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onEnded', but the handler also receives the target element's 'DOMRef'.
onEndedWith :: (DOMRef -> action) -> Attribute model action
onEndedWith action = on "ended" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onEnded', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Ended
--
-- view_ [ event (static (onEndedMain Ended)) ] [ "some view" ]
-- @
--
onEndedMain :: action -> EventHandler model action
onEndedMain action = onMain "ended" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onEndedMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Ended Model DOMRef
--
-- view_ [ event (static (onEndedMainWith Ended)) ] [ "some view" ]
-- @
--
onEndedMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onEndedMainWith action = onMain "ended" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindlooped
--
-- Fired at the end of each loop iteration, when @loop_ True@, before
-- playback automatically returns to the beginning.
--
onLooped :: action -> Attribute model action
onLooped action = on "looped" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onLooped', but the handler also receives the target element's 'DOMRef'.
onLoopedWith :: (DOMRef -> action) -> Attribute model action
onLoopedWith action = on "looped" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onLooped', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Looped
--
-- view_ [ event (static (onLoopedMain Looped)) ] [ "some view" ]
-- @
--
onLoopedMain :: action -> EventHandler model action
onLoopedMain action = onMain "looped" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onLoopedMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Looped Model DOMRef
--
-- view_ [ event (static (onLoopedMainWith Looped)) ] [ "some view" ]
-- @
--
onLoopedMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onLoopedMainWith action = onMain "looped" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#binderror
--
-- Fired when a video playback error occurs.
--
onError :: (VideoErrorEvent -> action) -> Attribute model action
onError action = on "error" videoErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onError', but the handler also receives the target element's 'DOMRef'.
onErrorWith :: (VideoErrorEvent -> DOMRef -> action) -> Attribute model action
onErrorWith action = on "error" videoErrorDecoder $ \e _ domRef -> action e domRef
-----------------------------------------------------------------------------
-- | Like 'onError', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Errored VideoErrorEvent
--
-- view_ [ event (static (onErrorMain Errored)) ] [ "some view" ]
-- @
--
onErrorMain :: (VideoErrorEvent -> action) -> EventHandler model action
onErrorMain action = onMain "error" videoErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onErrorMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Errored VideoErrorEvent Model DOMRef
--
-- view_ [ event (static (onErrorMainWith Errored)) ] [ "some view" ]
-- @
--
onErrorMainWith :: (VideoErrorEvent -> model -> DOMRef -> action) -> EventHandler model action
onErrorMainWith action = onMain "error" videoErrorDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#bindbuffering
--
-- Fired while the video is buffering.
--
onBuffering :: (VideoBufferingEvent -> action) -> Attribute model action
onBuffering action = on "buffering" bufferingDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onBuffering', but the handler also receives the target element's 'DOMRef'.
onBufferingWith :: (VideoBufferingEvent -> DOMRef -> action) -> Attribute model action
onBufferingWith action = on "buffering" bufferingDecoder $ \e _ domRef -> action e domRef
-----------------------------------------------------------------------------
-- | Like 'onBuffering', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Buffering VideoBufferingEvent
--
-- view_ [ event (static (onBufferingMain Buffering)) ] [ "some view" ]
-- @
--
onBufferingMain :: (VideoBufferingEvent -> action) -> EventHandler model action
onBufferingMain action = onMain "buffering" bufferingDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onBufferingMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Buffering VideoBufferingEvent Model DOMRef
--
-- view_ [ event (static (onBufferingMainWith Buffering)) ] [ "some view" ]
-- @
--
onBufferingMainWith :: (VideoBufferingEvent -> model -> DOMRef -> action) -> EventHandler model action
onBufferingMainWith action = onMain "buffering" bufferingDecoder action
-----------------------------------------------------------------------------
