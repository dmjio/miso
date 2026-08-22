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
import           Miso.Types (DOMRef, Attribute)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | blur event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: action -> Attribute model action
onBlur action = on "blur" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChecked :: (Checked -> action) -> Attribute model action
onChecked f = on "change" checkedDecoder (\action _ _ -> f action)
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
  -> Attribute model action
onContextMenuWithOptions opts action =
  onWithOptions BUBBLE opts "contextmenu" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: action -> Attribute model action
onClick action = on "click" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickCapture :: action -> Attribute model action
onClickCapture action = onCapture "click" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
-- Like 'onClick', but passes the DOM reference along (akin to @getElementById@).
onClickWith :: (DOMRef -> action) -> Attribute model action
onClickWith action = on "click" emptyDecoder $ \() _ domRef -> action domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on click
  -> Attribute model action
onClickWithOptions options action = onWithOptions BUBBLE options "click" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickPrevent :: action -> Attribute model action
onClickPrevent = onClickWithOptions preventDefault
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: action -> Attribute model action
onFocus action = on "focus" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: action -> Attribute model action
onDoubleClick action = on "dblclick" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClickWith :: (DOMRef -> action) -> Attribute model action
onDoubleClickWith f = on "dblclick" emptyDecoder $ \() _ domRef -> f domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClickWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on double-click
  -> Attribute model action
onDoubleClickWithOptions options action =
  onWithOptions BUBBLE options "dblclick" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput
  :: (MisoString -> action)
  -- ^ Callback receiving @event.target.value@
  -> Attribute model action
onInput f = on "input" valueDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInputWith
  :: (MisoString -> DOMRef -> action)
  -- ^ Callback receiving @event.target.value@ and the element's 'DOMRef'
  -> Attribute model action
onInputWith f = on "input" valueDecoder $ \val _ domRef -> f val domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange
  :: (MisoString -> action)
  -- ^ Callback receiving @event.target.value@
  -> Attribute model action
onChange f = on "change" valueDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChangeWith
  :: (MisoString -> DOMRef -> action)
  -- ^ Callback receiving @event.target.value@ and the element's 'DOMRef'
  -> Attribute model action
onChangeWith f = on "change" valueDecoder $ \val _ domRef -> f val domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/select
onSelect
  :: (MisoString -> action)
  -- ^ Callback receiving @event.target.value@ of the selected text
  -> Attribute model action
onSelect f = on "select" valueDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo
  :: (KeyInfo -> action)
  -- ^ Callback receiving the key code and modifier key state
  -> Attribute model action
onKeyDownWithInfo f = on "keydown" keyInfoDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown
  :: (KeyCode -> action)
  -- ^ Callback receiving the numeric key code of the pressed key
  -> Attribute model action
onKeyDown f = on "keydown" keycodeDecoder (\action _ _ -> f action)
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
-- view :: Model -> View context Action
-- view entryId = input_ [ onEnter NoOp OnEnter ]
-- @
--
-- @since 1.9.0.0
onEnter
  :: action
  -- ^ The action to call when the keydown *is not* 13 (typically @NoOp@ or @Id@)
  -> action
  -- ^ The action to call when keydown *is* 13.
  -> Attribute model action
onEnter nothing action = onKeyDown $ bool nothing action . (==13)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress
  :: (KeyCode -> action)
  -- ^ Callback receiving the numeric key code of the pressed key
  -> Attribute model action
onKeyPress f = on "keypress" keycodeDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp
  :: (KeyCode -> action)
  -- ^ Callback receiving the numeric key code of the released key
  -> Attribute model action
onKeyUp f = on "keyup" keycodeDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: action -> Attribute model action
onMouseUp action = on "mouseup" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: action -> Attribute model action
onMouseDown action = on "mousedown" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: action -> Attribute model action
onMouseEnter action = on "mouseenter" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: action -> Attribute model action
onMouseLeave action = on "mouseleave" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: action -> Attribute model action
onMouseOver action = on "mouseover" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: action -> Attribute model action
onMouseOut action = on "mouseout" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: action -> Attribute model action
onDragStart action = on "dragstart" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStartWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when the drag starts
  -> Attribute model action
onDragStartWithOptions options action =
  onWithOptions BUBBLE options "dragstart" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute model action
onDragOver action = on "dragover" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOverWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch while the dragged element is over this target
  -> Attribute model action
onDragOverWithOptions options action =
  onWithOptions BUBBLE options "dragover" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute model action
onDragEnd action = on "dragend" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEndWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when the drag operation ends
  -> Attribute model action
onDragEndWithOptions options action =
  onWithOptions BUBBLE options "dragend" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute model action
onDragEnter action = on "dragenter" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnterWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when a dragged element enters this target
  -> Attribute model action
onDragEnterWithOptions options action =
  onWithOptions BUBBLE options "dragenter" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute model action
onDragLeave action = on "dragleave" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeaveWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when a dragged element leaves this target
  -> Attribute model action
onDragLeaveWithOptions options action =
  onWithOptions BUBBLE options "dragleave" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute model action
onDrag action = on "drag" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDragWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch while the element is being dragged
  -> Attribute model action
onDragWithOptions options action =
  onWithOptions BUBBLE options "drag" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop
  :: Options
  -- ^ Propagation options — typically include @preventDefault@ to allow the drop
  -> action
  -- ^ Action to dispatch when a dragged element is dropped on this target
  -> Attribute model action
onDrop options action =
  onWithOptions BUBBLE options "drop" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDropWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on drop
  -> Attribute model action
onDropWithOptions options action =
  onWithOptions BUBBLE options "drop" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
--
-- Note: This has `preventDefault` enabled by default.
--
onSubmit :: action -> Attribute model action
onSubmit action =
  onWithOptions BUBBLE preventDefault
    "submit" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerup
onPointerUp
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerUp f = on "pointerup" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerdown
onPointerDown
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerDown f = on "pointerdown" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerenter
onPointerEnter
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerEnter f = on "pointerenter" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerleave
onPointerLeave
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerLeave f = on "pointerleave" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerover
onPointerOver
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerOver f = on "pointerover" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerout
onPointerOut
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerOut f = on "pointerout" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointercancel
onPointerCancel
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerCancel f = on "pointercancel" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointermove
onPointerMove
  :: (PointerEvent -> action)
  -- ^ Callback receiving the full t'PointerEvent'
  -> Attribute model action
onPointerMove f = on "pointermove" pointerDecoder (\action _ _ -> f action)
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_abort.asp
onAbort :: action -> Attribute model action
onAbort action = on "abort" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_abort.asp
onAbortWith :: (Media -> action) -> Attribute model action
onAbortWith action = on "abort" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplay.asp
onCanPlay :: action -> Attribute model action
onCanPlay action = on "canplay" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplay.asp
onCanPlayWith :: (Media -> action) -> Attribute model action
onCanPlayWith action = on "canplay" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplaythrough.asp
onCanPlayThrough :: action -> Attribute model action
onCanPlayThrough action = on "canplaythrough" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_canplaythrough.asp
onCanPlayThroughWith :: (Media -> action) -> Attribute model action
onCanPlayThroughWith action = on "canplaythrough" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_durationchange.asp
onDurationChange :: action -> Attribute model action
onDurationChange action = on "durationchange" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_durationchange.asp
onDurationChangeWith :: (Media -> action) -> Attribute model action
onDurationChangeWith action = on "durationchange" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/jsref/event_onemptied.asp
onEmptied :: action -> Attribute model action
onEmptied action = on "emptied" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/jsref/event_onemptied.asp
onEmptiedWith :: (Media -> action) -> Attribute model action
onEmptiedWith action = on "emptied" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ended.asp
onEnded :: action -> Attribute model action
onEnded action = on "ended" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ended.asp
onEndedWith :: (Media -> action) -> Attribute model action
onEndedWith action = on "ended" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_error.asp
onError :: action -> Attribute model action
onError action = on "error" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_error.asp
onErrorWith :: (Media -> action) -> Attribute model action
onErrorWith action = on "error" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/jsref/event_onload.asp
onLoad :: action -> Attribute model action
onLoad action = on "load" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onUnload event
onUnload :: action -> Attribute model action
onUnload action = on "unload" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadeddata.asp
onLoadedData :: action -> Attribute model action
onLoadedData action = on "loadeddata" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadeddata.asp
onLoadedDataWith :: (Media -> action) -> Attribute model action
onLoadedDataWith action = on "loadeddata" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadedmetadata.asp
onLoadedMetadata :: action -> Attribute model action
onLoadedMetadata action = on "loadedmetadata" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadedmetadata.asp
onLoadedMetadataWith :: (Media -> action) -> Attribute model action
onLoadedMetadataWith action = on "loadedmetadata" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadstart.asp
onLoadStart :: action -> Attribute model action
onLoadStart action = on "loadstart" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_loadstart.asp
onLoadStartWith :: (Media -> action) -> Attribute model action
onLoadStartWith action = on "loadstart" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_pause.asp
onPause :: action -> Attribute model action
onPause action = on "pause" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_pause.asp
onPauseWith :: (Media -> action) -> Attribute model action
onPauseWith action = on "pause" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_play.asp
onPlay :: action -> Attribute model action
onPlay action = on "play" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_play.asp
onPlayWith :: (Media -> action) -> Attribute model action
onPlayWith action = on "play" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_playing.asp
onPlaying :: action -> Attribute model action
onPlaying action = on "playing" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_playing.asp
onPlayingWith :: (Media -> action) -> Attribute model action
onPlayingWith action = on "playing" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_progress.asp
onProgress :: action -> Attribute model action
onProgress action = on "progress" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_progress.asp
onProgressWith :: (Media -> action) -> Attribute model action
onProgressWith action = on "progress" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ratechange.asp
onRateChange :: action -> Attribute model action
onRateChange action = on "ratechange" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_ratechange.asp
onRateChangeWith :: (Media -> action) -> Attribute model action
onRateChangeWith action = on "ratechange" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeked.asp
onSeeked :: action -> Attribute model action
onSeeked action = on "seeked" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeked.asp
onSeekedWith :: (Media -> action) -> Attribute model action
onSeekedWith action = on "seeked" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeking.asp
onSeeking :: action -> Attribute model action
onSeeking action = on "seeking" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_seeking.asp
onSeekingWith :: (Media -> action) -> Attribute model action
onSeekingWith action = on "seeking" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_stalled.asp
onStalled :: action -> Attribute model action
onStalled action = on "stalled" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_stalled.asp
onStalledWith :: (Media -> action) -> Attribute model action
onStalledWith action = on "stalled" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_suspend.asp
onSuspend :: action -> Attribute model action
onSuspend action = on "suspend" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_suspend.asp
onSuspendWith :: (Media -> action) -> Attribute model action
onSuspendWith action = on "suspend" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_timeupdate.asp
onTimeUpdate :: action -> Attribute model action
onTimeUpdate action = on "timeupdate" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_timeupdate.asp
onTimeUpdateWith :: (Media -> action) -> Attribute model action
onTimeUpdateWith action = on "timeupdate" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_volumechange.asp
onVolumeChange :: action -> Attribute model action
onVolumeChange action = on "volumechange" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_volumechange.asp
onVolumeChangeWith :: (Media -> action) -> Attribute model action
onVolumeChangeWith action = on "volumechange" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_waiting.asp
onWaiting :: action -> Attribute model action
onWaiting action = on "waiting" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://www.w3schools.com/tags/av_event_waiting.asp
onWaitingWith :: (Media -> action) -> Attribute model action
onWaitingWith action = on "waiting" emptyDecoder $ \() _ -> action . Media
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchstart
onTouchStart :: action -> Attribute model action
onTouchStart action = on "touchstart" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchstart
onTouchStartWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on touch start
  -> Attribute model action
onTouchStartWithOptions options action = onWithOptions BUBBLE options "touchstart" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchend
onTouchEnd :: action -> Attribute model action
onTouchEnd action = on "touchend" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchend
onTouchEndWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch on touch end
  -> Attribute model action
onTouchEndWithOptions options action = onWithOptions BUBBLE options "touchend" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchmove
onTouchMove :: action -> Attribute model action
onTouchMove action = on "touchmove" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchmove
onTouchMoveWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch while a touch point is moving
  -> Attribute model action
onTouchMoveWithOptions options action = onWithOptions BUBBLE options "touchmove" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchcancel
onTouchCancel :: action -> Attribute model action
onTouchCancel action = on "touchcancel" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/touchcancel
onTouchCancelWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> action
  -- ^ Action to dispatch when a touch point is cancelled
  -> Attribute model action
onTouchCancelWithOptions options action = onWithOptions BUBBLE options "touchcancel" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
