{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Miso.Event where

import qualified Data.Map              as M
import           GHCJS.Types
import           Miso.Html.String
import           Miso.Html.Types.Event

defaultEvents :: M.Map JSString Bool
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

-- | `EventHandler` object action
data EventHandler action =
  forall returnType
    . EventHandler {
        eventOptions :: Options
      , eventName :: MisoString
      , eventGrammar :: Grammar returnType
      , eventFunction :: returnType -> action
      }
