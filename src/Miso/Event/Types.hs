-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Types
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Event.Types
  ( -- ** Types
    Events
  , Capture
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
import           Data.Aeson (FromJSON(..), withText)
import qualified Data.Map.Strict as M
import           Language.Javascript.JSaddle (ToJSVal(..), create)
import           Miso.String (MisoString, ms)
import qualified Miso.FFI as FFI
-----------------------------------------------------------------------------
-- | Type useful for both KeyCode and additional key press information.
data KeyInfo
  = KeyInfo
  { keyCode :: !KeyCode
  , shiftKey, metaKey, ctrlKey, altKey :: !Bool
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
  , pointerId :: Int
  , isPrimary :: Bool
  , client :: (Double, Double)
  -- ^ clientX, clientY
  , screen :: (Double, Double)
  -- ^ screenX, screenY
  , offset :: (Double, Double)
  -- ^ offsetX, offsetY
  , page :: (Double,Double)
  -- ^ pageX, pageY
  , tilt :: (Double,Double)
  -- ^ tiltX, tiltY
  , pressure :: Double
  , button :: Int
  -- ^ https://w3c.github.io/pointerevents/#the-button-property
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
  , _stopPropagation :: Bool
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
    FFI.set "preventDefault" _preventDefault o
    FFI.set "stopPropagation" _stopPropagation o
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
type Events = M.Map MisoString Capture
-----------------------------------------------------------------------------
-- | Type synonym to express capture mode for browser / mobile events.
--
-- Used to determine if *capture* should be set when using /addEventListener/
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#capture>
--
type Capture = Bool
-----------------------------------------------------------------------------
-- | Default delegated events
defaultEvents :: Events
defaultEvents = M.fromList
  [ ("blur", True)
  , ("change", False)
  , ("click", False)
  , ("contextmenu", False)
  , ("dblclick", False)
  , ("focus", False)
  , ("input", False)
  , ("select", False)
  , ("submit", False)
  ]
-----------------------------------------------------------------------------
-- | Keyboard events
keyboardEvents :: Events
keyboardEvents = M.fromList
  [ ("keydown", False)
  , ("keypress", False)
  , ("keyup", False)
  ]
-----------------------------------------------------------------------------
-- | Mouse events
mouseEvents :: Events
mouseEvents = M.fromList
  [ ("mouseup", False)
  , ("mousedown", False)
  , ("mouseenter", True)
  , ("mouseleave", False)
  , ("mouseover", False)
  , ("mouseout", False)
  , ("contextmenu", False)
  ]
-----------------------------------------------------------------------------
-- | Drag events
dragEvents :: Events
dragEvents = M.fromList
  [ ("dragstart", False)
  , ("dragover", False)
  , ("dragend", False)
  , ("dragenter", False)
  , ("dragleave", False)
  , ("drag", False)
  , ("drop", False)
  ]
-----------------------------------------------------------------------------
-- | Pointer events
pointerEvents :: Events
pointerEvents = M.fromList
  [ ("pointerup", False)
  , ("pointerdown", False)
  , ("pointerenter", True)
  , ("pointercancel", False)
  , ("pointerleave", False)
  , ("pointerover", False)
  , ("pointerout", False)
  , ("contextmenu", False)
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
  [ ("abort", True)
  , ("canplay", True)
  , ("canplaythrough", True)
  , ("durationchange", False)
  , ("emptied", True)
  , ("ended", True)
  , ("error", True)
  , ("loadeddata", False)
  , ("loadedmetadata", False)
  , ("loadstart", False)
  , ("pause", True)
  , ("play", True)
  , ("playing", True)
  , ("progress", True)
  , ("ratechange", True)
  , ("seeked", True)
  , ("seeking", True)
  , ("stalled", True)
  , ("suspend", True)
  , ("timeupdate", True)
  , ("volumechange", True)
  , ("waiting", True)
  ]
-----------------------------------------------------------------------------
-- | Clipboard events
clipboardEvents :: Events
clipboardEvents = M.fromList
  [ ("cut", False)
  , ("copy", False)
  , ("paste", False)
  ]
-----------------------------------------------------------------------------
-- | Touch events
touchEvents :: Events
touchEvents = M.fromList
  [ ("touchstart", False)
  , ("touchcancel", False)
  , ("touchmove", False)
  , ("touchend", False)
  ]
-----------------------------------------------------------------------------
