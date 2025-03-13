{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Miso.Event.Types where

import qualified Data.Map.Strict as M
import           GHC.Generics
import           GHCJS.Marshal (ToJSVal)
import           Miso.String
import           Data.Aeson (FromJSON)

-- | Type useful for both KeyCode and additional key press information.
data KeyInfo
  = KeyInfo
  { keyCode :: !KeyCode
  , shiftKey, metaKey, ctrlKey, altKey :: !Bool
  } deriving (Show, Eq)

-- | Type used for Keyboard events.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode#Browser_compatibility>
newtype KeyCode = KeyCode Int
  deriving (Show, Eq, Ord, FromJSON)

-- | Type used for Checkbox events.
newtype Checked = Checked Bool
  deriving (Show, Eq, Ord, FromJSON)

-- | Options for handling event propagation.
data Options = Options {
    preventDefault :: Bool
  , stopPropagation :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSVal Options

-- | Default value for 'Options'.
--
-- > defaultOptions = Options { preventDefault = False, stopPropagation = False }
defaultOptions :: Options
defaultOptions
  = Options
  { preventDefault = False
  , stopPropagation = False
  }

-- | Related to using drop-related events
newtype AllowDrop = AllowDrop Bool
  deriving (Show, Eq, FromJSON)

-- | Default delegated events
defaultEvents :: M.Map MisoString Bool
defaultEvents = M.fromList
  [ ("blur", True)
  , ("change", False)
  , ("click", False)
  , ("dblclick", False)
  , ("focus", False)
  , ("input", False)
  , ("select", False)
  , ("submit", False)
  ]

keyboardEvents :: M.Map MisoString Bool
keyboardEvents = M.fromList
  [ ("keydown", False)
  , ("keypress", False)
  , ("keyup", False)
  ]

mouseEvents :: M.Map MisoString Bool
mouseEvents = M.fromList
  [ ("mouseup", False)
  , ("mousedown", False)
  , ("mouseenter", True)
  , ("mouseleave", False)
  , ("mouseover", False)
  , ("mouseout", False)
  ]

dragEvents :: M.Map MisoString Bool
dragEvents = M.fromList
  [ ("dragstart", False)
  , ("dragover", False)
  , ("dragend", False)
  , ("dragenter", False)
  , ("dragleave", False)
  , ("drag", False)
  , ("drop", False)
  ]

