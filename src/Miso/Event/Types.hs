{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Miso.Event.Types where

import qualified Data.Map as M
import           GHC.Generics
import           Miso.String
import           Data.Aeson

-- | Type used for Keyboard events
newtype KeyCode = KeyCode Int
  deriving (Show, Eq, Ord, FromJSON)

-- | Type used for Checkbox events
newtype Checked = Checked Bool
  deriving (Show, Eq, Ord, FromJSON)

-- | Options for handling event propagation
data Options = Options {
    preventDefault :: Bool
  , stopPropagation :: Bool
  } deriving (Show, Eq, Generic)

-- | Default options
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
