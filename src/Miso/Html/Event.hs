{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Event
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Miso.Html.Event
  ( -- * Custom event handlers
    on
  , onWithOptions
  , Options (..)
  , defaultOptions
   -- * Mouse events
  , onClick
  , onDoubleClick
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  -- * Keyboard events
  , onKeyDown
  , onKeyPress
  , onKeyUp
  -- * Form events
  , onInput
  , onChecked
  , onSubmit
  -- * Focus events
  , onBlur
  , onFocus
  -- * Drag events
  , onDrag
  , onDragLeave
  , onDragEnter
  , onDragEnd
  , onDragStart
  , onDragOver
  -- * Drop events
  , onDrop
  ) where

import           Data.Proxy (Proxy (..))
import           Miso.Html.Internal ( Attribute, on, onWithOptions, MisoString )
import           Miso.Types ( AllowDrop (..)
                            , Action
                            , defaultOptions
                            , Options (..) )

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: Action model -> Attribute model
onBlur action = on (Proxy :: Proxy "blur") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChecked :: (Bool -> Action model) -> Attribute model
onChecked = on (Proxy :: Proxy "change")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: Action model -> Attribute model
onClick action = on (Proxy :: Proxy "click") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: Action model -> Attribute model
onFocus action = on (Proxy :: Proxy "focus") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: Action model -> Attribute model
onDoubleClick action = on (Proxy :: Proxy "dblclick") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (MisoString -> Action model) -> Attribute model
onInput = on (Proxy :: Proxy "input")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (Int -> Action model) -> Attribute model
onKeyDown = on (Proxy :: Proxy "keydown")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (Int -> Action model) -> Attribute model
onKeyPress = on (Proxy ::Proxy "keypress")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (Int -> Action model) -> Attribute model
onKeyUp = on (Proxy :: Proxy "keyup")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: Action model -> Attribute model
onMouseUp action = on (Proxy :: Proxy "mouseup") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: Action model -> Attribute model
onMouseDown action = on (Proxy :: Proxy "mousedown") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: Action model -> Attribute model
onMouseEnter action = on (Proxy :: Proxy "mouseenter") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: Action model -> Attribute model
onMouseLeave action = on (Proxy :: Proxy "mouseleave") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: Action model -> Attribute model
onMouseOver action = on (Proxy :: Proxy "mouseover") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: Action model -> Attribute model
onMouseOut action = on (Proxy :: Proxy "mouseout") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: Action model -> Attribute model
onDragStart action = on (Proxy :: Proxy "dragstart") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: Action model -> Attribute model
onDragOver action = on (Proxy :: Proxy "dragover") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: Action model -> Attribute model
onDragEnd action = on (Proxy :: Proxy "dragend") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: Action model -> Attribute model
onDragEnter action = on (Proxy :: Proxy "dragenter") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: Action model -> Attribute model
onDragLeave action = on (Proxy :: Proxy "dragleave") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: Action model -> Attribute model
onDrag action = on (Proxy :: Proxy "drag") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop :: AllowDrop -> Action model -> Attribute model
onDrop (AllowDrop allowDrop) action =
  onWithOptions defaultOptions { preventDefault = allowDrop }
    (Proxy :: Proxy "drop") (\() -> action)

-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
onSubmit :: Action model -> Attribute model
onSubmit action =
  onWithOptions defaultOptions { preventDefault = True }
    (Proxy :: Proxy "submit") $ \() -> action
