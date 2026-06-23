-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Types
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Event.Types" defines the payload types for browser DOM events and
-- the 'Events' map that controls which events are delegated to the miso
-- runtime and at which 'Phase' of the event lifecycle.
--
-- = Event delegation
--
-- Miso uses
-- <https://developer.mozilla.org/en-US/docs/Learn_web_development/Core/Scripting/Event_bubbling event delegation>:
-- a single listener is attached at the root of the component's DOM subtree
-- and catches all matching events as they bubble or capture past it.
-- The 'Events' map (@'Data.Map.Strict.Map' 'Miso.String.MisoString' 'Phase'@)
-- declares which event names participate and their phase.
--
-- The default set ('defaultEvents') covers the most common interactions.
-- Additional event groups can be merged in when constructing a component:
--
-- @
-- myComponent = ('Miso.component' model update view)
--   { 'Miso.Types.events' = 'defaultEvents'
--       \<\> 'keyboardEvents'
--       \<\> 'pointerEvents'
--   }
-- @
--
-- = Event groups
--
-- ['defaultEvents'] blur, change, click, contextmenu, dblclick, focus, input, select, submit
-- ['keyboardEvents'] keydown, keypress, keyup
-- ['mouseEvents'] mouseup, mousedown, mouseenter, mouseleave, mouseover, mouseout, contextmenu
-- ['dragEvents'] drag, dragstart, dragend, dragenter, dragleave, dragover, drop
-- ['pointerEvents'] pointerup, pointerdown, pointerenter, pointerleave, pointerover, pointerout, pointercancel, contextmenu
-- ['mediaEvents'] play, pause, ended, timeupdate, volumechange, …
-- ['clipboardEvents'] cut, copy, paste
-- ['touchEvents'] touchstart, touchend, touchmove, touchcancel
--
-- = Payload types
--
-- * 'KeyCode' \/ 'KeyInfo' — keyboard event key code and modifier state.
-- * 'Checked' — checkbox @checked@ boolean.
-- * 'PointerEvent' \/ 'PointerType' — pointer position, pressure, tilt, and device type.
-- * 'Options' — per-handler flags: 'preventDefault', 'stopPropagation'.
--
-- = See also
--
-- * "Miso.Event.Decoder" — 'Miso.Event.Decoder.Decoder' and pre-built decoders for these types
-- * "Miso.Html.Event" — @onClick@, @onInput@, @onKeyDown@, … combinators
-----------------------------------------------------------------------------
module Miso.Event.Types
  ( -- ** Types
    Events
  , Phase (..)
    -- *** KeyboardEvent
  , KeyInfo (..)
  , KeyCode (..)
    -- *** CheckedEvent
  , Checked (..)
    -- *** PointerEvent
  , PointerEvent (..)
  , PointerType (..)
    -- *** Options
  , Options (..)
  , defaultOptions
  , preventDefault
  , stopPropagation
    -- *** Events
  , defaultEvents
  , keyboardEvents
  , mouseEvents
  , dragEvents
  , pointerEvents
  , mediaEvents
  , clipboardEvents
  , touchEvents
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (FromJSON(..), withText)
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | Type useful for both KeyCode and additional key press information.
data KeyInfo
  = KeyInfo
  { keyCode :: !KeyCode
  -- ^ Numeric key code of the pressed key (see 'KeyCode')
  , shiftKey :: !Bool
  -- ^ 'True' if the Shift key was held when the event fired
  , metaKey :: !Bool
  -- ^ 'True' if the Meta (Command on macOS, Windows key on PC) key was held
  , ctrlKey :: !Bool
  -- ^ 'True' if the Control key was held
  , altKey :: !Bool
  -- ^ 'True' if the Alt (Option on macOS) key was held
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Type used for Keyboard events.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode#Browser_compatibility>
newtype KeyCode = KeyCode Int
  deriving (Show, Eq, Ord, FromJSON, Num)
-----------------------------------------------------------------------------
-- | Type used for Checkbox events.
newtype Checked = Checked Bool
  deriving (Show, Eq, Ord, FromJSON)
-----------------------------------------------------------------------------
-- | Type used for Pointer events.
-- <https://w3c.github.io/pointerevents>
data PointerEvent
  = PointerEvent
  { pointerType :: PointerType
  -- ^ Device kind: 'MousePointerType', 'PenPointerType', 'TouchPointerType', or 'UnknownPointerType'
  , pointerId :: Int
  -- ^ Unique identifier for this pointer, stable across move\/up\/cancel events
  , isPrimary :: Bool
  -- ^ 'True' for the primary pointer in a multi-touch sequence
  , client :: (Double, Double)
  -- ^ @(clientX, clientY)@ — position relative to the viewport
  , screen :: (Double, Double)
  -- ^ @(screenX, screenY)@ — position relative to the screen
  , offset :: (Double, Double)
  -- ^ @(offsetX, offsetY)@ — position relative to the target element
  , page :: (Double,Double)
  -- ^ @(pageX, pageY)@ — position relative to the full document
  , tilt :: (Double,Double)
  -- ^ @(tiltX, tiltY)@ — pen tilt angle in degrees from the surface plane
  , pressure :: Double
  -- ^ Normalised pressure in @[0, 1]@; @0.5@ for mouse buttons that lack pressure sensitivity
  , button :: Int
  -- ^ Which button changed state; @-1@ during move with no button change.
  -- See <https://w3c.github.io/pointerevents/#the-button-property PointerEvent.button>
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Pointer type
-- <https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent/pointerType>
data PointerType
  = MousePointerType
  | PenPointerType
  | TouchPointerType
  | UnknownPointerType MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON PointerType where
  parseJSON = withText "PointerType" $ \case
    "mouse" -> pure MousePointerType
    "touch" -> pure TouchPointerType
    "pen"   -> pure PenPointerType
    x       -> pure (UnknownPointerType (ms x))
-----------------------------------------------------------------------------
-- | t'Options' for handling event propagation.
data Options
  = Options
  { _preventDefault :: Bool
  -- ^ If 'True', calls @event.preventDefault()@ to suppress the browser's default behaviour
  , _stopPropagation :: Bool
  -- ^ If 'True', calls @event.stopPropagation()@ to halt event bubbling\/capturing
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance Monoid Options where
  mempty = defaultOptions
-----------------------------------------------------------------------------
instance Semigroup Options where
  Options p1 s1 <> Options p2 s2 = Options (p1 || p2) (s1 || s2)
-----------------------------------------------------------------------------
-- | Smart constructor for specifying 'preventDefault'
--
-- @since 1.9.0.0
preventDefault :: Options
preventDefault = defaultOptions { _preventDefault = True }
-----------------------------------------------------------------------------
-- | Smart constructor for specifying 'stopPropagation'
--
-- @since 1.9.0.0
stopPropagation :: Options
stopPropagation = defaultOptions { _stopPropagation = True }
-----------------------------------------------------------------------------
instance ToJSVal Options where
  toJSVal Options {..} = do
    o <- create
    flip (setProp "preventDefault") o =<< toJSVal _preventDefault
    flip (setProp "stopPropagation") o =<< toJSVal _stopPropagation
    toJSVal o
-----------------------------------------------------------------------------
-- | Default value for @Options@.
--
-- > defaultOptions = Options { preventDefault = False, stopPropagation = False }
defaultOptions :: Options
defaultOptions
  = Options
  { _preventDefault = False
  , _stopPropagation = False
  }
-----------------------------------------------------------------------------
-- | Convenience type for Events
type Events = M.Map MisoString Phase
-----------------------------------------------------------------------------
-- | Default delegated events
defaultEvents :: Events
defaultEvents = M.fromList
  [ ("blur", CAPTURE)
  , ("change", BUBBLE)
  , ("click", BUBBLE)
  , ("contextmenu", BUBBLE)
  , ("dblclick", BUBBLE)
  , ("focus", CAPTURE)
  , ("input", BUBBLE)
  , ("select", BUBBLE)
  , ("submit", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Keyboard events
keyboardEvents :: Events
keyboardEvents = M.fromList
  [ ("keydown", BUBBLE)
  , ("keypress", BUBBLE)
  , ("keyup", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Mouse events
mouseEvents :: Events
mouseEvents = M.fromList
  [ ("mouseup", BUBBLE)
  , ("mousedown", BUBBLE)
  , ("mouseenter", CAPTURE)
  , ("mouseleave", BUBBLE)
  , ("mouseover", BUBBLE)
  , ("mouseout", BUBBLE)
  , ("contextmenu", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Drag events
dragEvents :: Events
dragEvents = M.fromList
  [ ("dragstart", BUBBLE)
  , ("dragover", BUBBLE)
  , ("dragend", BUBBLE)
  , ("dragenter", BUBBLE)
  , ("dragleave", BUBBLE)
  , ("drag", BUBBLE)
  , ("drop", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Pointer events
pointerEvents :: Events
pointerEvents = M.fromList
  [ ("pointerup", BUBBLE)
  , ("pointerdown", BUBBLE)
  , ("pointerenter", CAPTURE)
  , ("pointercancel", BUBBLE)
  , ("pointerleave", BUBBLE)
  , ("pointerover", BUBBLE)
  , ("pointerout", BUBBLE)
  , ("contextmenu", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Audio video events
-- For use with the @<audio>@ and @<video>@ tags.
--
-- @
-- myApp :: 'Miso.Types.App' Model Action
-- myApp = ('Miso.Types.component' model update view){ events = 'defaultEvents' <> 'mediaEvents' }
-- @
mediaEvents :: Events
mediaEvents = M.fromList
  [ ("abort", CAPTURE)
  , ("canplay", CAPTURE)
  , ("canplaythrough", CAPTURE)
  , ("durationchange", BUBBLE)
  , ("emptied", CAPTURE)
  , ("ended", CAPTURE)
  , ("error", CAPTURE)
  , ("loadeddata", BUBBLE)
  , ("loadedmetadata", BUBBLE)
  , ("loadstart", BUBBLE)
  , ("pause", CAPTURE)
  , ("play", CAPTURE)
  , ("playing", CAPTURE)
  , ("progress", CAPTURE)
  , ("ratechange", CAPTURE)
  , ("seeked", CAPTURE)
  , ("seeking", CAPTURE)
  , ("stalled", CAPTURE)
  , ("suspend", CAPTURE)
  , ("timeupdate", CAPTURE)
  , ("volumechange", CAPTURE)
  , ("waiting", CAPTURE)
  ]
-----------------------------------------------------------------------------
-- | Clipboard events
clipboardEvents :: Events
clipboardEvents = M.fromList
  [ ("cut", BUBBLE)
  , ("copy", BUBBLE)
  , ("paste", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Touch events
touchEvents :: Events
touchEvents = M.fromList
  [ ("touchstart", BUBBLE)
  , ("touchcancel", BUBBLE)
  , ("touchmove", BUBBLE)
  , ("touchend", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Phase during which event listener is invoked.
--
-- @since 1.9.0.0
data Phase = CAPTURE | BUBBLE deriving (Eq, Show)
-----------------------------------------------------------------------------
instance ToJSVal Phase where
  toJSVal = \case
    CAPTURE -> toJSVal True
    BUBBLE -> toJSVal False
-----------------------------------------------------------------------------
