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
import Miso.Html.Event    (onClick)
import Miso.Html.Internal
import Miso.Types

-- | onBegin event
onBegin :: Action model -> Attribute model
onBegin action = on (Proxy :: Proxy "begin") $ \() -> action

-- | onEnd event
onEnd :: Action model -> Attribute model
onEnd action = on (Proxy :: Proxy "end") $ \() -> action

-- | onRepeat event
onRepeat :: Action model -> Attribute model
onRepeat action = on (Proxy :: Proxy "repeat") $ \() -> action

-- | onAbort event
onAbort :: Action model -> Attribute model
onAbort action = on (Proxy :: Proxy "abort") $ \() -> action

-- | onError event
onError :: Action model -> Attribute model
onError action = on (Proxy :: Proxy "error") $ \() -> action

-- | onResize event
onResize :: Action model -> Attribute model
onResize action = on (Proxy :: Proxy "resize") $ \() -> action

-- | onScroll event
onScroll :: Action model -> Attribute model
onScroll action = on (Proxy :: Proxy "scroll") $ \() -> action

-- | onLoad event
onLoad :: Action model -> Attribute model
onLoad action = on (Proxy :: Proxy "load") $ \() -> action

-- | onUnload event
onUnload :: Action model -> Attribute model
onUnload action = on (Proxy :: Proxy "unload") $ \() -> action

-- | onZoom event
onZoom :: Action model -> Attribute model
onZoom action = on (Proxy :: Proxy "zoom") $ \() -> action

-- | onActivate event
onActivate :: Action model -> Attribute model
onActivate action = on (Proxy :: Proxy "activate") $ \() -> action

-- | onFocusIn event
onFocusIn :: Action model -> Attribute model
onFocusIn action = on (Proxy :: Proxy "focusin") $ \() -> action

-- | onFocusOut event
onFocusOut :: Action model -> Attribute model
onFocusOut action = on (Proxy :: Proxy "focusout") $ \() -> action

-- | onMouseDown event
onMouseDown :: Action model -> Attribute model
onMouseDown action = on (Proxy :: Proxy "mousedown") $ \() -> action
-- | onMouseMove event
onMouseMove :: Action model -> Attribute model
onMouseMove action = on (Proxy :: Proxy "mousemove") $ \() -> action

-- | onMouseOut event
onMouseOut :: Action model -> Attribute model
onMouseOut action = on (Proxy :: Proxy "mouseout") $ \() -> action

-- | onMouseOver event
onMouseOver :: Action model -> Attribute model
onMouseOver action = on (Proxy :: Proxy "mouseover") $ \() -> action

-- | onMouseUp event
onMouseUp :: Action model -> Attribute model
onMouseUp action = on (Proxy :: Proxy "mouseup") $ \() -> action



