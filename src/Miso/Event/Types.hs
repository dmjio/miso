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
----------------------------------------------------------------------------
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
-- | Keyboard event payload containing the key code and modifier key state.
data KeyInfo
  = KeyInfo
  { keyCode :: !KeyCode
  -- ^ The numeric key code (@keyCode@, @which@, or @charCode@)
  , shiftKey :: !Bool
  -- ^ @True@ when the Shift key was held during the event
  , metaKey :: !Bool
  -- ^ @True@ when the Meta (Cmd on macOS, Windows key on Windows) was held
  , ctrlKey :: !Bool
  -- ^ @True@ when the Control key was held
  , altKey :: !Bool
  -- ^ @True@ when the Alt (Option on macOS) key was held
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
-- | <https://w3c.github.io/pointerevents>
--
-- Payload for pointer events (@pointerdown@, @pointermove@, @pointerup@, etc.).
data PointerEvent
  = PointerEvent
  { pointerType :: PointerType
  -- ^ Hardware type producing the event (@mouse@, @pen@, @touch@, or unknown)
  , pointerId :: Int
  -- ^ Unique identifier for this pointer (stable across a single press/release sequence)
  , isPrimary :: Bool
  -- ^ @True@ when this is the primary pointer in a multi-touch sequence
  , client :: (Double, Double)
  -- ^ @(clientX, clientY)@ — coordinates relative to the viewport
  , screen :: (Double, Double)
  -- ^ @(screenX, screenY)@ — coordinates relative to the screen
  , offset :: (Double, Double)
  -- ^ @(offsetX, offsetY)@ — coordinates relative to the target element
  , page :: (Double, Double)
  -- ^ @(pageX, pageY)@ — coordinates relative to the document
  , tilt :: (Double, Double)
  -- ^ @(tiltX, tiltY)@ — stylus tilt angles in degrees
  , pressure :: Double
  -- ^ Normalised pressure in the range @[0, 1]@
  , button :: Int
  -- ^ <https://w3c.github.io/pointerevents/#the-button-property> — which button was pressed
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent/pointerType>
--
-- Hardware type responsible for a t'PointerEvent'.
data PointerType
  = MousePointerType
  -- ^ Event produced by a mouse
  | PenPointerType
  -- ^ Event produced by a pen or stylus
  | TouchPointerType
  -- ^ Event produced by a touch contact
  | UnknownPointerType MisoString
  -- ^ Unrecognised pointer type, containing the raw string from the browser
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON PointerType where
  parseJSON = withText "PointerType" $ \case
    "mouse" -> pure MousePointerType
    "touch" -> pure TouchPointerType
    "pen"   -> pure PenPointerType
    x       -> pure (UnknownPointerType (ms x))
-----------------------------------------------------------------------------
-- | Options controlling browser event propagation behaviour.
data Options
  = Options
  { _preventDefault :: Bool
  -- ^ When @True@, calls @event.preventDefault()@ to suppress the browser's default action
  , _stopPropagation :: Bool
  -- ^ When @True@, calls @event.stopPropagation()@ to prevent the event from bubbling further
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
-- | Map from DOM event name to the t'Phase' in which it should be delegated.
-- Passed to the @events@ field of t'Miso.Types.Component'.
type Events = M.Map MisoString Phase
-----------------------------------------------------------------------------
-- | The minimal set of delegated events used by most applications:
-- @blur@, @change@, @click@, @contextmenu@, @dblclick@, @focus@, @input@, @select@, @submit@.
--
-- Combine with 'keyboardEvents', 'mouseEvents', 'pointerEvents', etc. using '(<>)'.
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
-- | The DOM event phase in which the delegated listener fires.
--
-- @since 1.9.0.0
data Phase
  = CAPTURE
  -- ^ Fire during the capture phase (before the event reaches the target element)
  | BUBBLE
  -- ^ Fire during the bubble phase (after the target element handles the event)
  deriving (Eq, Show)
-----------------------------------------------------------------------------
instance ToJSVal Phase where
  toJSVal = \case
    CAPTURE -> toJSVal True
    BUBBLE -> toJSVal False
-----------------------------------------------------------------------------
