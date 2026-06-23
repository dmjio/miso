-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Html.Event" provides pre-wired event-handler 'Miso.Types.Attribute'
-- values for the most common browser events. Each handler is built on the
-- lower-level 'Miso.Event.on' \/ 'Miso.Event.onWithOptions' primitives from
-- "Miso.Event".
--
-- This module is re-exported in its entirety by "Miso.Html" and "Miso".
--
-- = Naming conventions
--
-- Handlers follow a consistent naming pattern:
--
-- [@onXxx action@] fires @action@; no event data extracted
-- [@onXxxWith (a -> action)@] passes extracted event data or 'Miso.Effect.DOMRef'
-- [@onXxxWithOptions opts act@] adds 'Miso.Event.Types.Options' (@preventDefault@ \/ @stopPropagation@) before firing
-- [@onXxxCapture action@] registers in the capture phase instead of bubble
--
-- = Quick start
--
-- @
-- import "Miso"
--
-- view :: Model -> 'Miso.Types.View' Model Action
-- view m =
--   'Miso.Html.Element.div_' []
--     [ 'Miso.Html.Element.button_' [ 'onClick' Increment ]        [ 'Miso.text' \"+\" ]
--     , 'Miso.Html.Element.input_'  [ 'onInput' SetText
--                     , 'Miso.Html.Property.value_' m.text ]      []
--     , 'Miso.Html.Element.form_'   [ 'onSubmit' Submit ]         []  -- preventDefault by default
--     ]
-- @
--
-- = Event groups
--
-- * __Mouse__: 'onClick', 'onClickCapture', 'onClickWith', 'onClickWithOptions',
--   'onClickPrevent', 'onDoubleClick', 'onDoubleClickWith',
--   'onMouseDown', 'onMouseUp', 'onMouseEnter', 'onMouseLeave',
--   'onMouseOver', 'onMouseOut', 'onContextMenuWithOptions'
-- * __Keyboard__: 'onKeyDown', 'onKeyDownWithInfo', 'onKeyPress', 'onKeyUp', 'onEnter'
-- * __Form__: 'onInput', 'onInputWith', 'onChange', 'onChangeWith',
--   'onChecked', 'onSubmit', 'onSelect'
-- * __Focus__: 'onFocus', 'onBlur'
-- * __Drag__: 'onDrag', 'onDragStart', 'onDragEnd', 'onDragEnter',
--   'onDragLeave', 'onDragOver', 'onDrop' (and @WithOptions@ variants)
-- * __Pointer__: 'onPointerDown', 'onPointerUp', 'onPointerEnter',
--   'onPointerLeave', 'onPointerOver', 'onPointerOut',
--   'onPointerCancel', 'onPointerMove'
-- * __Media__: 'onPlay', 'onPause', 'onEnded', 'onTimeUpdate',
--   'onVolumeChange', 'onLoadedData', 'onLoadedMetadata', … (and @With@ variants)
-- * __Touch__: 'onTouchStart', 'onTouchEnd', 'onTouchMove',
--   'onTouchCancel' (and @WithOptions@ variants)
-- * __Lifecycle__: 'onLoad', 'onUnload', 'onError'
--
-- = Notes
--
-- * 'onSubmit' enables @preventDefault@ by default to suppress the native
--   form submission.
-- * 'onEnter' is a convenience wrapper around 'onKeyDown' that fires
--   different actions depending on whether @keyCode == 13@.
-- * The @WithOptions@ variants require 'Miso.Event.Types.defaultEvents' (or a
--   superset) to include the relevant event name in the component's @events@ map.
--
-- = See also
--
-- * "Miso.Event" — 'Miso.Event.on', 'Miso.Event.onCapture', 'Miso.Event.onWithOptions'
-- * "Miso.Event.Decoder" — 'Miso.Event.Decoder.Decoder' for custom event extraction
-- * "Miso.Event.Types" — 'Miso.Event.Types.Options', 'Miso.Event.Types.KeyCode',
--   'Miso.Event.Types.PointerEvent'
-----------------------------------------------------------------------------
module Miso.Html.Event
  ( -- *** Mouse
    onClick
  , onClickPrevent
  , onClickCapture
  , onClickWith
  , onClickWithOptions
  , onDoubleClick
  , onDoubleClickWith
  , onDoubleClickWithOptions
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
  , onEnter
  -- *** Form
  , onInput
  , onInputWith
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
  , onLoad
  , onUnload
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
  -- *** Touch
  , onTouchStart
  , onTouchStartWithOptions
  , onTouchEnd
  , onTouchEndWithOptions
  , onTouchMove
  , onTouchMoveWithOptions
  , onTouchCancel
  , onTouchCancelWithOptions
  ) where
-----------------------------------------------------------------------------
import           Data.Bool (bool)
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.Media (Media(..))
import           Miso.Types (Attribute, DOMRef)
import           Miso.String (MisoString)
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
onContextMenuWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when the context menu event fires
  -> Attribute action
onContextMenuWithOptions opts action =
  onWithOptions BUBBLE opts "contextmenu" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: action -> Attribute action
onClick action = on "click" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickCapture :: action -> Attribute action
onClickCapture action = onCapture "click" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
-- Like 'onClick', but passes the DOM reference along (akin to @getElementById@).
onClickWith :: (DOMRef -> action) -> Attribute action
onClickWith action = on "click" emptyDecoder $ \() domRef -> action domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on click
  -> Attribute action
onClickWithOptions options action = onWithOptions BUBBLE options "click" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickPrevent :: action -> Attribute action
onClickPrevent = onClickWithOptions preventDefault
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: action -> Attribute action
onFocus action = on "focus" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: action -> Attribute action
onDoubleClick action = on "dblclick" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClickWith :: (DOMRef -> action) -> Attribute action
onDoubleClickWith f = on "dblclick" emptyDecoder $ \() domRef -> f domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClickWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on double-click
  -> Attribute action
onDoubleClickWithOptions options action =
  onWithOptions BUBBLE options "dblclick" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput
  :: (MisoString -> action)
  -- ^ Callback receiving @event.target.value@
  -> Attribute action
onInput f = on "input" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInputWith
  :: (MisoString -> DOMRef -> action)
  -- ^ Callback receiving @event.target.value@ and the element's 'DOMRef'
  -> Attribute action
onInputWith = on "input" valueDecoder
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange
  :: (MisoString -> action)
  -- ^ Callback receiving @event.target.value@
  -> Attribute action
onChange f = on "change" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChangeWith
  :: (MisoString -> DOMRef -> action)
  -- ^ Callback receiving @event.target.value@ and the element's 'DOMRef'
  -> Attribute action
onChangeWith = on "change" valueDecoder
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/select
onSelect
  :: (MisoString -> action)
  -- ^ Callback receiving @event.target.value@ of the selected text
  -> Attribute action
onSelect f = on "select" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo
  :: (KeyInfo -> action)
  -- ^ Callback receiving the key code and modifier key state
  -> Attribute action
onKeyDownWithInfo f = on "keydown" keyInfoDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown
  :: (KeyCode -> action)
  -- ^ Callback receiving the numeric key code of the pressed key
  -> Attribute action
onKeyDown f = on "keydown" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | 'onEnter'
--
-- A convenience function for processing the @Enter@ key.
--
-- @
--
-- data Action = NoOp | OnEnter
--
-- type Model = Int
--
-- view :: Model -> View model Action
-- view entryId = input_ [ onEnter NoOp OnEnter ]
-- @
--
-- @since 1.9.0.0
onEnter
  :: action
  -- ^ The action to call when the keydown *is not* 13 (typically @NoOp@ or @Id@)
  -> action
  -- ^ The action to call when keydown *is* 13.
  -> Attribute action
onEnter nothing action = onKeyDown $ bool nothing action . (==13)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress
  :: (KeyCode -> action)
  -- ^ Callback receiving the numeric key code of the pressed key
  -> Attribute action
onKeyPress f = on "keypress" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp
  :: (KeyCode -> action)
  -- ^ Callback receiving the numeric key code of the released key
  -> Attribute action
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
onDragStartWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when the drag starts
  -> Attribute action
onDragStartWithOptions options action =
  onWithOptions BUBBLE options "dragstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute action
onDragOver action = on "dragover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOverWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch while the dragged element is over this target
  -> Attribute action
onDragOverWithOptions options action =
  onWithOptions BUBBLE options "dragover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute action
onDragEnd action = on "dragend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEndWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when the drag operation ends
  -> Attribute action
onDragEndWithOptions options action =
  onWithOptions BUBBLE options "dragend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute action
onDragEnter action = on "dragenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnterWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when a dragged element enters this target
  -> Attribute action
onDragEnterWithOptions options action =
  onWithOptions BUBBLE options "dragenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute action
onDragLeave action = on "dragleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeaveWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when a dragged element leaves this target
  -> Attribute action
onDragLeaveWithOptions options action =
  onWithOptions BUBBLE options "dragleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute action
onDrag action = on "drag" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDragWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch while the element is being dragged
  -> Attribute action
onDragWithOptions options action =
  onWithOptions BUBBLE options "drag" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop
  :: Options
  -- ^ Propagation options — typically include @preventDefault@ to allow the drop
  -> action
  -- ^ Action to dispatch when a dragged element is dropped on this target
  -> Attribute action
onDrop options action =
  onWithOptions BUBBLE options "drop" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDropWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on drop
  -> Attribute action
onDropWithOptions options action =
  onWithOptions BUBBLE options "drop" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
--
-- Note: This has `preventDefault` enabled by default.
--
onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions BUBBLE preventDefault
    "submit" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerup
onPointerUp
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerUp f = on "pointerup" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerdown
onPointerDown
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerDown f = on "pointerdown" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerenter
onPointerEnter
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerEnter f = on "pointerenter" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerleave
onPointerLeave
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerLeave f = on "pointerleave" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerover
onPointerOver
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerOver f = on "pointerover" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerout
onPointerOut
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerOut f = on "pointerout" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointercancel
onPointerCancel
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
onPointerCancel f = on "pointercancel" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointermove
onPointerMove
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full 'PointerEvent'
  -> Attribute action
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
-- | https://www.w3schools.com/jsref/event_onemptied.asp
onEmptied :: action -> Attribute action
onEmptied action = on "emptied" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/jsref/event_onemptied.asp
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
-- | https://www.w3schools.com/jsref/event_onload.asp
onLoad :: action -> Attribute action
onLoad action = on "load" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onUnload event
onUnload :: action -> Attribute action
onUnload action = on "unload" emptyDecoder $ \() _ -> action
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
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchstart
onTouchStart :: action -> Attribute action
onTouchStart action = on "touchstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchstart
onTouchStartWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on touch start
  -> Attribute action
onTouchStartWithOptions options action = onWithOptions BUBBLE options "touchstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchend
onTouchEnd :: action -> Attribute action
onTouchEnd action = on "touchend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchend
onTouchEndWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on touch end
  -> Attribute action
onTouchEndWithOptions options action = onWithOptions BUBBLE options "touchend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchmove
onTouchMove :: action -> Attribute action
onTouchMove action = on "touchmove" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchmove
onTouchMoveWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch while a touch point is moving
  -> Attribute action
onTouchMoveWithOptions options action = onWithOptions BUBBLE options "touchmove" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchcancel
onTouchCancel :: action -> Attribute action
onTouchCancel action = on "touchcancel" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchcancel
onTouchCancelWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when a touch point is cancelled
  -> Attribute action
onTouchCancelWithOptions options action = onWithOptions BUBBLE options "touchcancel" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
