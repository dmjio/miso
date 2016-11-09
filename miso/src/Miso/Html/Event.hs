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
  -- * Class
  , HasEvent (..)
  ) where

#if !(MIN_VERSION_aeson(0,10,0))
import           Data.Aeson
#else
import           Data.Aeson            hiding (defaultOptions)
#endif
import           Data.Maybe
#ifdef __GHCJS__
import           Data.String
#else
#endif
import           Data.Proxy
import qualified Data.Text             as T
import           GHC.TypeLits

import           Miso.Event
import           Miso.Html.Types
import           Miso.Html.Types.Event

-- | Class for constructing pure event handlers
class HasEvent (eventName :: Symbol) returnType where
  parseEvent :: Proxy eventName -> Grammar returnType

-- | For defining delegated events with options
--
-- > let options = Options { stopPropogation = False, preventDefault = False }
-- >     clickHandler = onWithOptions options (Proxy :: Proxy "click") $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
-- | Handler smart constructor
onWithOptions
   :: ( HasEvent eventName returnType
      , KnownSymbol eventName
      )
   => Options
   -> Proxy eventName
   -> (returnType -> action)
   -> Attribute action
#ifdef __GHCJS__
onWithOptions opts p f =
  E $ EventHandler opts name (parseEvent p) f
    where
      name = fromString $ symbolVal p
#else
onWithOptions _ _ _ = E EventHandler
#endif


-- | For defining delegated events
--
-- > data Action = Add | Subtract
-- > button_ [ on (Proxy :: Proxy "click") $ \() -> Add ] [ text_ "+" ]
--
on :: ( HasEvent eventName returnType
      , KnownSymbol eventName
      )
   => Proxy eventName
   -> (returnType -> action)
   -> Attribute action
on = onWithOptions defaultOptions

-- | Event handler defaults
--
-- @
-- defaultOptions :: Options
-- defaultOptions = Options False False
-- @
--
defaultOptions :: Options
defaultOptions = Options False False

-- | Listens on "blur" event, returns `()`
instance HasEvent "blur" () where parseEvent _ = pure ()

-- | Listens on "change" event, returns `Bool` from `event.target.checked`
instance HasEvent "change" Bool   where parseEvent _ = checkedGrammar

-- | Listens on "click" event, returns `()`
instance HasEvent "click" ()      where parseEvent _ = pure ()

-- | Listens on "dblclick" event, returns `()`
instance HasEvent "dblclick" ()   where parseEvent _ = pure ()

-- | Listens on "focus" event, returns `()`
instance HasEvent "focus" ()      where parseEvent _ = pure ()

-- | Listens on "input" event, returns `T.Text` from  `event.target.value`
instance HasEvent "input" T.Text  where parseEvent _ = inputGrammar

-- | Listens on "keydown" event, returns `Int` from `keyCode`, `which` or `charCode`
instance HasEvent "keydown" Int   where parseEvent _ = keyGrammar

-- | Listens on "keypress" event, returns `Int` from `keyCode`, `which` or `charCode`
instance HasEvent "keypress" Int  where parseEvent _ = keyGrammar

-- | Listens on "keyup" event, returns `Int` from `keyCode`, `which` or `charCode`
instance HasEvent "keyup" Int     where parseEvent _ = keyGrammar

-- | Listens on "mouseup" event, returns `()`
instance HasEvent "mouseup" ()    where parseEvent _ = pure ()

-- | Listens on "mousedown" event, returns `()`
instance HasEvent "mousedown" ()  where parseEvent _ = pure ()

-- | Listens on "mouseenter" event, returns `()`
instance HasEvent "mouseenter" () where parseEvent _ = pure ()

-- | Listens on "mouseleave" event, returns `()`
instance HasEvent "mouseleave" () where parseEvent _ = pure ()

-- | Listens on "mouseover" event, returns `()`
instance HasEvent "mouseover" ()  where parseEvent _ = pure ()

-- | Listens on "mouseout" event, returns `()`
instance HasEvent "mouseout" ()   where parseEvent _ = pure ()

-- | Listens on "dragstart" event, returns `()`
instance HasEvent "dragstart" ()  where parseEvent _ = pure ()

-- | Listens on "dragover" event, returns `()`
instance HasEvent "dragover" ()   where parseEvent _ = pure ()

-- | Listens on "dragend" event, returns `()`
instance HasEvent "dragend" ()    where parseEvent _ = pure ()

-- | Listens on "dragenter" event, returns `()`
instance HasEvent "dragenter" ()  where parseEvent _ = pure ()

-- | Listens on "dragleave" event, returns `()`
instance HasEvent "dragleave" ()  where parseEvent _ = pure ()

-- | Listens on "drag" event, returns `()`
instance HasEvent "drag" ()       where parseEvent _ = pure ()

-- | Listens on "drop" event, returns `()`
instance HasEvent "drop" ()       where parseEvent _ = pure ()

-- | Listens on "submit" event, returns `()`
instance HasEvent "submit" ()     where parseEvent _ = pure ()

-- | Listens on "begin" event, returns `()`
instance HasEvent "begin" () where parseEvent _ = pure ()

-- | Listens on "end" event, returns `()`

instance HasEvent "end" () where parseEvent _ = pure ()

-- | Listens on "repeat" repeat, returns `()`
instance HasEvent "repeat" () where parseEvent _ = pure ()

-- | Listens on "abort" abort, returns `()`
instance HasEvent "abort" () where parseEvent _ = pure ()

-- | Listens on "error" error, returns `()`
instance HasEvent "error" () where parseEvent _ = pure ()

-- | Listens on "resize" resize, returns `()`
instance HasEvent "resize" () where parseEvent _ = pure ()

-- | Listens on "mousemove" mousemove, returns `()`
instance HasEvent "mousemove" () where parseEvent _ = pure ()

-- | Listens on "focusout" focusout, returns `()`
instance HasEvent "focusout" () where parseEvent _ = pure ()

-- | Listens on "focusin" focusin, returns `()`
instance HasEvent "focusin" () where parseEvent _ = pure ()

-- | Listens on "activate" activate, returns `()`
instance HasEvent "activate" () where parseEvent _ = pure ()

-- | Listens on "zoom" zoom, returns `()`
instance HasEvent "zoom" () where parseEvent _ = pure ()

-- | Listens on "unload" unload, returns `()`
instance HasEvent "unload" () where parseEvent _ = pure ()

-- | Listens on "load" load, returns `()`
instance HasEvent "load" () where parseEvent _ = pure ()

-- | Listens on "scroll" scroll, returns `()`
instance HasEvent "scroll" () where parseEvent _ = pure ()

-- | `blur` event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: action -> Attribute action
onBlur action = on (Proxy :: Proxy "blur") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChecked :: (Bool -> action) -> Attribute action
onChecked = on (Proxy :: Proxy "change")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: action -> Attribute action
onClick action = on (Proxy :: Proxy "click") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: action -> Attribute action
onFocus action = on (Proxy :: Proxy "focus") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: action -> Attribute action
onDoubleClick action = on (Proxy :: Proxy "dblclick") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (T.Text -> action) -> Attribute action
onInput = on (Proxy :: Proxy "input")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (Int -> action) -> Attribute action
onKeyDown = on (Proxy :: Proxy "keydown")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (Int -> action) -> Attribute action
onKeyPress = on (Proxy ::Proxy "keypress")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (Int -> action) -> Attribute action
onKeyUp = on (Proxy :: Proxy "keyup")

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: action -> Attribute action
onMouseUp action = on (Proxy :: Proxy "mouseup") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: action -> Attribute action
onMouseDown action = on (Proxy :: Proxy "mousedown") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: action -> Attribute action
onMouseEnter action = on (Proxy :: Proxy "mouseenter") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: action -> Attribute action
onMouseLeave action = on (Proxy :: Proxy "mouseleave") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: action -> Attribute action
onMouseOver action = on (Proxy :: Proxy "mouseover") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: action -> Attribute action
onMouseOut action = on (Proxy :: Proxy "mouseout") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: action -> Attribute action
onDragStart action = on (Proxy :: Proxy "dragstart") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute action
onDragOver action = on (Proxy :: Proxy "dragover") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute action
onDragEnd action = on (Proxy :: Proxy "dragend") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute action
onDragEnter action = on (Proxy :: Proxy "dragenter") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute action
onDragLeave action = on (Proxy :: Proxy "dragleave") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute action
onDrag action = on (Proxy :: Proxy "drag") $ \() -> action

-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop :: AllowDrop -> action -> Attribute action
onDrop (AllowDrop allowDrop) action =
  onWithOptions defaultOptions { preventDefault = allowDrop }
    (Proxy :: Proxy "drop") (\() -> action)

-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions defaultOptions { preventDefault = True }
    (Proxy :: Proxy "submit") $ \() -> action

-- | Retrieves "value" field in `Grammar`
inputGrammar :: FromJSON a => Grammar a
inputGrammar = do
  target <- getTarget
  result <- getField "value" target
  case result of
    Nothing -> Prelude.error "Couldn't retrieve target input value"
    Just value -> pure value

-- | Retrieves "checked" field in `Grammar`
checkedGrammar :: FromJSON a => Grammar a
checkedGrammar = do
  target <- getTarget
  result <- getField "checked" target
  case result of
    Nothing -> Prelude.error "Couldn't retrieve target checked value"
    Just value -> pure value

-- | Retrieves either "keyCode", "which" or "charCode" field in `Grammar`
keyGrammar :: Grammar Int
keyGrammar = do
  keyCode <- getEventField "keyCode"
  which <- getEventField "which"
  charCode <- getEventField "charCode"
  pure $ head $ catMaybes [ keyCode, which, charCode ]
