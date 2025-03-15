{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings         #-}
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
  ( -- *** Smart constructors
    on
  , onWithOptions
  -- *** Lifecycle events
  , onCreated
  , onDestroyed
  , onBeforeDestroyed
   -- *** Mouse
  , onClick
  , onDoubleClick
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  -- *** Keyboard
  , onKeyDown
  , onKeyDownWithInfo
  , onKeyPress
  , onKeyUp
  -- *** Form
  , onInput
  , onChange
  , onChecked
  , onSubmit
  -- *** Focus
  , onBlur
  , onFocus
  -- *** Drag
  , onDrag
  , onDragLeave
  , onDragEnter
  , onDragEnd
  , onDragStart
  , onDragOver
  -- *** Drop
  , onDrop
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
  ) where

import           Data.Aeson.Types (parseEither)
import           Language.Javascript.JSaddle

import           Miso.Event
import           Miso.FFI (syncCallback, set, objectToJSON, asyncCallback1)
import           Miso.Html.Types ( Attribute (E) )
import           Miso.String (MisoString, unpack)

-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on = onWithOptions defaultOptions

-- | @onWithOptions opts eventName decoder toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- decodes its argument using @decoder@, converts it to an action
-- using @toAction@ and then feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onWithOptions defaultOptions "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onWithOptions
  :: Options
  -> MisoString
  -> Decoder r
  -> (r -> action)
  -> Attribute action
onWithOptions options eventName Decoder{..} toAction =
  E $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   decodeAtVal <- toJSVal decodeAt
   cb <- asyncCallback1 $ \e -> do
       Just v <- fromJSVal =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> sink (toAction r)
   set "runEvent" cb eventHandlerObject
   set "options" jsOptions eventHandlerObject
   set eventName eo (Object eventObj)

-- | @onCreated action@ is an event that gets called after the actual DOM
-- element is created.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onCreated :: action -> Attribute action
onCreated action =
  E $ \sink n -> do
    cb <- syncCallback (sink action)
    set "onCreated" cb n

-- | @onDestroyed action@ is an event that gets called after the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onDestroyed :: action -> Attribute action
onDestroyed action =
  E $ \sink n -> do
    cb <- syncCallback (sink action)
    set "onDestroyed" cb n

-- | @onBeforeDestroyed action@ is an event that gets called before the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onBeforeDestroyed :: action -> Attribute action
onBeforeDestroyed action =
  E $ \sink n -> do
    cb <- syncCallback (sink action)
    set "onBeforeDestroyed" cb n

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: action -> Attribute action
onBlur action = on "blur" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChecked :: (Checked -> action) -> Attribute action
onChecked = on "change" checkedDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: action -> Attribute action
onClick action = on "click" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: action -> Attribute action
onFocus action = on "focus" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: action -> Attribute action
onDoubleClick action = on "dblclick" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (MisoString -> action) -> Attribute action
onInput = on "input" valueDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: (MisoString -> action) -> Attribute action
onChange = on "change" valueDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/select
onSelect :: (MisoString -> action) -> Attribute action
onSelect = on "select" valueDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo = on "keydown" keyInfoDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (KeyCode -> action) -> Attribute action
onKeyDown = on "keydown" keycodeDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (KeyCode -> action) -> Attribute action
onKeyPress = on "keypress" keycodeDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (KeyCode -> action) -> Attribute action
onKeyUp = on "keyup" keycodeDecoder

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: action -> Attribute action
onMouseUp action = on "mouseup" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: action -> Attribute action
onMouseDown action = on "mousedown" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: action -> Attribute action
onMouseEnter action = on "mouseenter" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: action -> Attribute action
onMouseLeave action = on "mouseleave" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: action -> Attribute action
onMouseOver action = on "mouseover" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: action -> Attribute action
onMouseOut action = on "mouseout" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: action -> Attribute action
onDragStart action = on "dragstart" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute action
onDragOver action = on "dragover" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute action
onDragEnd action = on "dragend" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute action
onDragEnter action = on "dragenter" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute action
onDragLeave action = on "dragleave" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute action
onDrag action = on "drag" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop :: AllowDrop -> action -> Attribute action
onDrop (AllowDrop allowDrop) action =
  onWithOptions defaultOptions { preventDefault = allowDrop }
    "drop" emptyDecoder (\() -> action)

-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions defaultOptions { preventDefault = True }
    "submit" emptyDecoder $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerup
onPointerUp :: (PointerEvent -> action) -> Attribute action
onPointerUp action = on "pointerup" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerdown
onPointerDown :: (PointerEvent -> action) -> Attribute action
onPointerDown action = on "pointerdown" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerenter
onPointerEnter :: (PointerEvent -> action) -> Attribute action
onPointerEnter action = on "pointerenter" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerleave
onPointerLeave :: (PointerEvent -> action) -> Attribute action
onPointerLeave action = on "pointerleave" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerover
onPointerOver :: (PointerEvent -> action) -> Attribute action
onPointerOver action = on "pointerover" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerout
onPointerOut :: (PointerEvent -> action) -> Attribute action
onPointerOut action = on "pointerout" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointercancel
onPointerCancel :: (PointerEvent -> action) -> Attribute action
onPointerCancel action = on "pointercancel" pointerDecoder action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointermove
onPointerMove :: (PointerEvent -> action) -> Attribute action
onPointerMove action = on "pointermove" pointerDecoder action

