-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Events
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
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
onBegin :: action -> Attribute action
onBegin action = on "begin" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onEnd event
onEnd :: action -> Attribute action
onEnd action = on "end" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onRepeat event
onRepeat :: action -> Attribute action
onRepeat action = on "repeat" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onAbort event
onAbort :: action -> Attribute action
onAbort action = on "abort" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onError event
onError :: action -> Attribute action
onError action = on "error" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onResize event
onResize :: action -> Attribute action
onResize action = on "resize" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onScroll event
onScroll :: action -> Attribute action
onScroll action = on "scroll" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onZoom event
onZoom :: action -> Attribute action
onZoom action = on "zoom" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onActivate event
onActivate :: action -> Attribute action
onActivate action = on "activate" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onFocusIn event
onFocusIn :: action -> Attribute action
onFocusIn action = on "focusin" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onFocusOut event
onFocusOut :: action -> Attribute action
onFocusOut action = on "focusout" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseDown event
onMouseDown :: action -> Attribute action
onMouseDown action = on "mousedown" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseMove event
onMouseMove :: action -> Attribute action
onMouseMove action = on "mousemove" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseOut event
onMouseOut :: action -> Attribute action
onMouseOut action = on "mouseout" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseOver event
onMouseOver :: action -> Attribute action
onMouseOver action = on "mouseover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | onMouseUp event
onMouseUp :: action -> Attribute action
onMouseUp action = on "mouseup" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
