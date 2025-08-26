-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Miso.Html.Event
  ( -- *** Mouse
    onClick
  , onClickWith
  , onDoubleClick
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  , onContextMenuWithOptions
  -- *** Keyboard
  , onKeyDown
  , onKeyDownWithInfo
  , onKeyPress
  , onKeyUp
  -- *** Form
  , onInput
  , onChange
  , onChangeWith
  , onChecked
  , onSubmit
  -- *** Focus
  , onBlur
  , onFocus
  -- *** Drag
  , onDrag
  , onDragWithOptions
  , onDragLeave
  , onDragLeaveWithOptions
  , onDragEnter
  , onDragEnterWithOptions
  , onDragEnd
  , onDragEndWithOptions
  , onDragStart
  , onDragStartWithOptions
  , onDragOver
  , onDragOverWithOptions
  -- *** Drop
  , onDrop
  , onDropWithOptions
  -- *** Select
  , onSelect
  -- *** Pointer
  , onPointerDown
  , onPointerUp
  , onPointerEnter
  , onPointerLeave
  , onPointerOver
  , onPointerOut
  , onPointerCancel
  , onPointerMove
  -- *** Media
  , onAbort
  , onAbortWith
  , onCanPlay
  , onCanPlayWith
  , onCanPlayThrough
  , onCanPlayThroughWith
  , onDurationChange
  , onDurationChangeWith
  , onEmptied
  , onEmptiedWith
  , onEnded
  , onEndedWith
  , onError
  , onErrorWith
  , onLoadedData
  , onLoadedDataWith
  , onLoadedMetadata
  , onLoadedMetadataWith
  , onLoadStart
  , onLoadStartWith
  , onPause
  , onPauseWith
  , onPlay
  , onPlayWith
  , onPlaying
  , onPlayingWith
  , onProgress
  , onProgressWith
  , onRateChange
  , onRateChangeWith
  , onSeeked
  , onSeekedWith
  , onSeeking
  , onSeekingWith
  , onStalled
  , onStalledWith
  , onSuspend
  , onSuspendWith
  , onTimeUpdate
  , onTimeUpdateWith
  , onVolumeChange
  , onVolumeChangeWith
  , onWaiting
  , onWaitingWith
  ) where
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.Media (Media(..))
import           Miso.Types (Attribute)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle (JSVal)
-----------------------------------------------------------------------------
-- | blur event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: action -> Attribute action
onBlur action = on "blur" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChecked :: (Checked -> action) -> Attribute action
onChecked f = on "change" checkedDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/contextmenu
--
-- This can be used to disable right-click context menu from appearing
--
-- @
-- div_ [ onContextMenuWithOptions NoOp defaultOptions { preventDefault = False } ] [ ]
-- @
--
-- @since 1.9.0.0
onContextMenuWithOptions :: Options -> action -> Attribute action
onContextMenuWithOptions opts action =
  onWithOptions opts "contextmenu" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: action -> Attribute action
onClick action = on "click" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
-- Like 'onClick', but passes the DOM reference along (akin to 'getElementBydId').
onClickWith :: (JSVal -> action) -> Attribute action
onClickWith action = on "click" emptyDecoder $ \() domRef -> action domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: action -> Attribute action
onFocus action = on "focus" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: action -> Attribute action
onDoubleClick action = on "dblclick" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (MisoString -> action) -> Attribute action
onInput f = on "input" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: (MisoString -> action) -> Attribute action
onChange f = on "change" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChangeWith :: (MisoString -> JSVal -> action) -> Attribute action
onChangeWith = on "change" valueDecoder
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/select
onSelect :: (MisoString -> action) -> Attribute action
onSelect f = on "select" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo f = on "keydown" keyInfoDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (KeyCode -> action) -> Attribute action
onKeyDown f = on "keydown" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (KeyCode -> action) -> Attribute action
onKeyPress f = on "keypress" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (KeyCode -> action) -> Attribute action
onKeyUp f = on "keyup" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: action -> Attribute action
onMouseUp action = on "mouseup" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: action -> Attribute action
onMouseDown action = on "mousedown" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: action -> Attribute action
onMouseEnter action = on "mouseenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: action -> Attribute action
onMouseLeave action = on "mouseleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: action -> Attribute action
onMouseOver action = on "mouseover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: action -> Attribute action
onMouseOut action = on "mouseout" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: action -> Attribute action
onDragStart action = on "dragstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStartWithOptions :: Options -> action -> Attribute action
onDragStartWithOptions options action =
  onWithOptions options "dragstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute action
onDragOver action = on "dragover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOverWithOptions :: Options -> action -> Attribute action
onDragOverWithOptions options action =
  onWithOptions options "dragover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute action
onDragEnd action = on "dragend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEndWithOptions :: Options -> action -> Attribute action
onDragEndWithOptions options action =
  onWithOptions options "dragend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute action
onDragEnter action = on "dragenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnterWithOptions :: Options -> action -> Attribute action
onDragEnterWithOptions options action =
  onWithOptions options "dragenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute action
onDragLeave action = on "dragleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeaveWithOptions :: Options -> action -> Attribute action
onDragLeaveWithOptions options action =
  onWithOptions options "dragleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute action
onDrag action = on "drag" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDragWithOptions :: Options -> action -> Attribute action
onDragWithOptions options action =
  onWithOptions options "drag" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop :: Options -> action -> Attribute action
onDrop options action =
  onWithOptions options "drop" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDropWithOptions :: Options -> action -> Attribute action
onDropWithOptions options action =
  onWithOptions options "drop" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
--
-- Note: This has `preventDefault` enabled by default.
--
onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions preventDefault
    "submit" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerup
onPointerUp :: (PointerEvent -> action) -> Attribute action
onPointerUp f = on "pointerup" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerdown
onPointerDown :: (PointerEvent -> action) -> Attribute action
onPointerDown f = on "pointerdown" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerenter
onPointerEnter :: (PointerEvent -> action) -> Attribute action
onPointerEnter f = on "pointerenter" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerleave
onPointerLeave :: (PointerEvent -> action) -> Attribute action
onPointerLeave f = on "pointerleave" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerover
onPointerOver :: (PointerEvent -> action) -> Attribute action
onPointerOver f = on "pointerover" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerout
onPointerOut :: (PointerEvent -> action) -> Attribute action
onPointerOut f = on "pointerout" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointercancel
onPointerCancel :: (PointerEvent -> action) -> Attribute action
onPointerCancel f = on "pointercancel" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointermove
onPointerMove :: (PointerEvent -> action) -> Attribute action
onPointerMove f = on "pointermove" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_abort.asp
onAbort :: action -> Attribute action
onAbort action = on "abort" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_abort.asp
onAbortWith :: (Media -> action) -> Attribute action
onAbortWith action = on "abort" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplay.asp
onCanPlay :: action -> Attribute action
onCanPlay action = on "canplay" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplay.asp
onCanPlayWith :: (Media -> action) -> Attribute action
onCanPlayWith action = on "canplay" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplaythrough.asp
onCanPlayThrough :: action -> Attribute action
onCanPlayThrough action = on "canplaythrough" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplaythrough.asp
onCanPlayThroughWith :: (Media -> action) -> Attribute action
onCanPlayThroughWith action = on "canplaythrough" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_durationchange.asp
onDurationChange :: action -> Attribute action
onDurationChange action = on "durationchange" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_durationchange.asp
onDurationChangeWith :: (Media -> action) -> Attribute action
onDurationChangeWith action = on "durationchange" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
onEmptied :: action -> Attribute action
onEmptied action = on "emptied" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
onEmptiedWith :: (Media -> action) -> Attribute action
onEmptiedWith action = on "emptied" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ended.asp
onEnded :: action -> Attribute action
onEnded action = on "ended" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ended.asp
onEndedWith :: (Media -> action) -> Attribute action
onEndedWith action = on "ended" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_error.asp
onError :: action -> Attribute action
onError action = on "error" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_error.asp
onErrorWith :: (Media -> action) -> Attribute action
onErrorWith action = on "error" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadeddata.asp
onLoadedData :: action -> Attribute action
onLoadedData action = on "loadeddata" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadeddata.asp
onLoadedDataWith :: (Media -> action) -> Attribute action
onLoadedDataWith action = on "loadeddata" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadedmetadata.asp
onLoadedMetadata :: action -> Attribute action
onLoadedMetadata action = on "loadedmetadata" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadedmetadata.asp
onLoadedMetadataWith :: (Media -> action) -> Attribute action
onLoadedMetadataWith action = on "loadedmetadata" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadstart.asp
onLoadStart :: action -> Attribute action
onLoadStart action = on "loadstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadstart.asp
onLoadStartWith :: (Media -> action) -> Attribute action
onLoadStartWith action = on "loadstart" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_pause.asp
onPause :: action -> Attribute action
onPause action = on "pause" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_pause.asp
onPauseWith :: (Media -> action) -> Attribute action
onPauseWith action = on "pause" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_play.asp
onPlay :: action -> Attribute action
onPlay action = on "play" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_play.asp
onPlayWith :: (Media -> action) -> Attribute action
onPlayWith action = on "play" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_playing.asp
onPlaying :: action -> Attribute action
onPlaying action = on "playing" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_playing.asp
onPlayingWith :: (Media -> action) -> Attribute action
onPlayingWith action = on "playing" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_progress.asp
onProgress :: action -> Attribute action
onProgress action = on "progress" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_progress.asp
onProgressWith :: (Media -> action) -> Attribute action
onProgressWith action = on "progress" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ratechange.asp
onRateChange :: action -> Attribute action
onRateChange action = on "ratechange" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ratechange.asp
onRateChangeWith :: (Media -> action) -> Attribute action
onRateChangeWith action = on "ratechange" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeked.asp
onSeeked :: action -> Attribute action
onSeeked action = on "seeked" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeked.asp
onSeekedWith :: (Media -> action) -> Attribute action
onSeekedWith action = on "seeked" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeking.asp
onSeeking :: action -> Attribute action
onSeeking action = on "seeking" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeking.asp
onSeekingWith :: (Media -> action) -> Attribute action
onSeekingWith action = on "seeking" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_stalled.asp
onStalled :: action -> Attribute action
onStalled action = on "stalled" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_stalled.asp
onStalledWith :: (Media -> action) -> Attribute action
onStalledWith action = on "stalled" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_suspend.asp
onSuspend :: action -> Attribute action
onSuspend action = on "suspend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_suspend.asp
onSuspendWith :: (Media -> action) -> Attribute action
onSuspendWith action = on "suspend" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_timeupdate.asp
onTimeUpdate :: action -> Attribute action
onTimeUpdate action = on "timeupdate" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_timeupdate.asp
onTimeUpdateWith :: (Media -> action) -> Attribute action
onTimeUpdateWith action = on "timeupdate" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_volumechange.asp
onVolumeChange :: action -> Attribute action
onVolumeChange action = on "volumechange" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_volumechange.asp
onVolumeChangeWith :: (Media -> action) -> Attribute action
onVolumeChangeWith action = on "volumechange" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_waiting.asp
onWaiting :: action -> Attribute action
onWaiting action = on "waiting" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_waiting.asp
onWaitingWith :: (Media -> action) -> Attribute action
onWaitingWith action = on "waiting" emptyDecoder $ \() -> action . Media
-----------------------------------------------------------------------------
