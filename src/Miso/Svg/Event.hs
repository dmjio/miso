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
import Miso.Types (EventHandler)
-----------------------------------------------------------------------------
-- | onBegin event
onBegin :: action -> EventHandler action
onBegin action = on "begin" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onEnd event
onEnd :: action -> EventHandler action
onEnd action = on "end" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onRepeat event
onRepeat :: action -> EventHandler action
onRepeat action = on "repeat" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onAbort event
onAbort :: action -> EventHandler action
onAbort action = on "abort" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onError event
onError :: action -> EventHandler action
onError action = on "error" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onResize event
onResize :: action -> EventHandler action
onResize action = on "resize" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onScroll event
onScroll :: action -> EventHandler action
onScroll action = on "scroll" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onZoom event
onZoom :: action -> EventHandler action
onZoom action = on "zoom" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onActivate event
onActivate :: action -> EventHandler action
onActivate action = on "activate" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onFocusIn event
onFocusIn :: action -> EventHandler action
onFocusIn action = on "focusin" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onFocusOut event
onFocusOut :: action -> EventHandler action
onFocusOut action = on "focusout" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseDown event
onMouseDown :: action -> EventHandler action
onMouseDown action = on "mousedown" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseMove event
onMouseMove :: action -> EventHandler action
onMouseMove action = on "mousemove" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseOut event
onMouseOut :: action -> EventHandler action
onMouseOut action = on "mouseout" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseOver event
onMouseOver :: action -> EventHandler action
onMouseOver action = on "mouseover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseUp event
onMouseUp :: action -> EventHandler action
onMouseUp action = on "mouseup" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
