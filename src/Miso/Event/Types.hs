{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Types
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Event.Types where

import qualified Data.Map as M
import           GHC.Generics
import           Miso.String
import           Data.Aeson (FromJSON)

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

-- | Default value for 'Options'.
--
-- > defaultOptions = Options { preventDefault = False, stopPropagation = False }
defaultOptions :: Options
defaultOptions = Options False False

-- | Related to using drop-related events
newtype AllowDrop = AllowDrop Bool
  deriving (Show, Eq, FromJSON)

-- | Default delegated events
defaultEvents :: M.Map MisoString Bool
defaultEvents = M.fromList [
    ("blur", True)
  , ("change", False)
  , ("click", False)
  , ("dblclick", False)
  , ("focus", False)
  , ("input", False)
  , ("keydown", False)
  , ("keypress", False)
  , ("keyup", False)
  , ("mouseup", False)
  , ("mousedown", False)
  , ("mouseenter", False)
  , ("mouseleave", False)
  , ("mouseover", False)
  , ("mouseout", False)
  , ("dragstart", False)
  , ("dragover", False)
  , ("dragend", False)
  , ("dragenter", False)
  , ("dragleave", False)
  , ("drag", False)
  , ("drop", False)
  , ("submit", False)
  ]
