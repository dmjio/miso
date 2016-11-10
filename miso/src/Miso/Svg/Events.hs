{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Events
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Svg.Events
  ( -- * Animation event handlers
    onBegin
  , onEnd
  , onRepeat
    -- * Document event attributes
  , onAbort
  , onError
  , onResize
  , onScroll
  , onLoad
  , onUnload
  , onZoom
    -- * Graphical Event Attributes
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

import Data.Proxy
import Miso.Html.Event    (onClick, on)
import Miso.Html.Types

-- | onBegin event
onBegin :: action -> Attribute action
onBegin action = on (Proxy :: Proxy "begin") $ \() -> action

-- | onEnd event
onEnd :: action -> Attribute action
onEnd action = on (Proxy :: Proxy "end") $ \() -> action

-- | onRepeat event
onRepeat :: action -> Attribute action
onRepeat action = on (Proxy :: Proxy "repeat") $ \() -> action

-- | onAbort event
onAbort :: action -> Attribute action
onAbort action = on (Proxy :: Proxy "abort") $ \() -> action

-- | onError event
onError :: action -> Attribute action
onError action = on (Proxy :: Proxy "error") $ \() -> action

-- | onResize event
onResize :: action -> Attribute action
onResize action = on (Proxy :: Proxy "resize") $ \() -> action

-- | onScroll event
onScroll :: action -> Attribute action
onScroll action = on (Proxy :: Proxy "scroll") $ \() -> action

-- | onLoad event
onLoad :: action -> Attribute action
onLoad action = on (Proxy :: Proxy "load") $ \() -> action

-- | onUnload event
onUnload :: action -> Attribute action
onUnload action = on (Proxy :: Proxy "unload") $ \() -> action

-- | onZoom event
onZoom :: action -> Attribute action
onZoom action = on (Proxy :: Proxy "zoom") $ \() -> action

-- | onActivate event
onActivate :: action -> Attribute action
onActivate action = on (Proxy :: Proxy "activate") $ \() -> action

-- | onFocusIn event
onFocusIn :: action -> Attribute action
onFocusIn action = on (Proxy :: Proxy "focusin") $ \() -> action

-- | onFocusOut event
onFocusOut :: action -> Attribute action
onFocusOut action = on (Proxy :: Proxy "focusout") $ \() -> action

-- | onMouseDown event
onMouseDown :: action -> Attribute action
onMouseDown action = on (Proxy :: Proxy "mousedown") $ \() -> action
-- | onMouseMove event
onMouseMove :: action -> Attribute action
onMouseMove action = on (Proxy :: Proxy "mousemove") $ \() -> action

-- | onMouseOut event
onMouseOut :: action -> Attribute action
onMouseOut action = on (Proxy :: Proxy "mouseout") $ \() -> action

-- | onMouseOver event
onMouseOver :: action -> Attribute action
onMouseOver action = on (Proxy :: Proxy "mouseover") $ \() -> action

-- | onMouseUp event
onMouseUp :: action -> Attribute action
onMouseUp action = on (Proxy :: Proxy "mouseup") $ \() -> action



