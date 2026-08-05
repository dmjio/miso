-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Svg.Event" provides event-handler 'Miso.Types.Attribute' values
-- for SVG-specific DOM events. All handlers use 'Miso.Event.emptyDecoder'
-- — they fire a fixed action with no payload extracted from the event
-- object. This module is re-exported by "Miso.Svg".
--
-- For pointer and keyboard events on SVG elements, use the handlers from
-- "Miso.Html.Event" directly — they work on any DOM element regardless of
-- namespace.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Svg"
--
-- data Action = AnimDone | Zoomed
--
-- view :: Model -> 'Miso.Types.View' Model Action
-- view _ =
--   'svg_' []
--     [ 'animate_'
--         [ 'onEnd'  AnimDone
--         , 'onZoom' Zoomed
--         ]
--     , 'circle_'
--         [ 'onClick'    Toggle
--         , 'onMouseOver' Highlight
--         ]
--     ]
-- @
--
-- = Event groups
--
-- * __Animation__ (@\<animate\>@, @\<animateTransform\>@, …):
--   'onBegin', 'onEnd', 'onRepeat'
--
-- * __Document__ (fires on @\<svg\>@ root):
--   'onAbort', 'onError', 'onResize', 'onScroll', 'onZoom'
--
-- * __Graphical__ (fires on any visible SVG element):
--   'onActivate', 'onClick', 'onFocusIn', 'onFocusOut',
--   'onMouseDown', 'onMouseMove', 'onMouseOut', 'onMouseOver', 'onMouseUp'
--
-- Note: 'onClick' is re-exported from "Miso.Html.Event" and is identical
-- to the HTML version.
--
-- = See also
--
-- * "Miso.Html.Event" — 'Miso.Html.Event.onPointerDown', 'Miso.Html.Event.onKeyDown', …
--   work on SVG elements too
-- * "Miso.Svg.Element" — SVG element constructors
-- * "Miso.Event" — 'Miso.Event.on', 'Miso.Event.emptyDecoder' primitives
----------------------------------------------------------------------------
module Miso.Svg.Event
  ( -- *** Animation
    onBegin
  , onEnd
  , onRepeat
    -- *** Document
  , onAbort
  , onError
  , onResize
  , onScroll
  , onZoom
    -- *** Graphical
  , onActivate
  , onClick
  , onFocusIn
  , onFocusOut
  , onMouseDown
  , onMouseMove
  , onMouseOut
  , onMouseOver
  , onMouseUp
  ) where
-----------------------------------------------------------------------------
import Miso.Event (on, emptyDecoder)
import Miso.Html.Event (onClick)
import Miso.Types (Attribute)
-----------------------------------------------------------------------------
-- | onBegin event
onBegin :: action -> Attribute model action
onBegin action = on "begin" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onEnd event
onEnd :: action -> Attribute model action
onEnd action = on "end" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onRepeat event
onRepeat :: action -> Attribute model action
onRepeat action = on "repeat" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onAbort event
onAbort :: action -> Attribute model action
onAbort action = on "abort" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onError event
onError :: action -> Attribute model action
onError action = on "error" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onResize event
onResize :: action -> Attribute model action
onResize action = on "resize" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onScroll event
onScroll :: action -> Attribute model action
onScroll action = on "scroll" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onZoom event
onZoom :: action -> Attribute model action
onZoom action = on "zoom" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onActivate event
onActivate :: action -> Attribute model action
onActivate action = on "activate" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onFocusIn event
onFocusIn :: action -> Attribute model action
onFocusIn action = on "focusin" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onFocusOut event
onFocusOut :: action -> Attribute model action
onFocusOut action = on "focusout" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onMouseDown event
onMouseDown :: action -> Attribute model action
onMouseDown action = on "mousedown" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onMouseMove event
onMouseMove :: action -> Attribute model action
onMouseMove action = on "mousemove" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onMouseOut event
onMouseOut :: action -> Attribute model action
onMouseOut action = on "mouseout" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onMouseOver event
onMouseOver :: action -> Attribute model action
onMouseOver action = on "mouseover" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
-- | onMouseUp event
onMouseUp :: action -> Attribute model action
onMouseUp action = on "mouseup" emptyDecoder $ \() _ _ -> action
-----------------------------------------------------------------------------
