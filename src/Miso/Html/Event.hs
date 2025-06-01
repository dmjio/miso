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
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
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
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute action
onDragOver action = on "dragover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute action
onDragEnd action = on "dragend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute action
onDragEnter action = on "dragenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute action
onDragLeave action = on "dragleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute action
onDrag action = on "drag" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop :: AllowDrop -> action -> Attribute action
onDrop (AllowDrop allowDrop) action =
  onWithOptions defaultOptions { preventDefault = allowDrop }
    "drop" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions defaultOptions { preventDefault = True }
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
