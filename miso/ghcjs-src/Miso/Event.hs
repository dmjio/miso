{-# LANGUAGE ExistentialQuantification #-}

module Miso.Event where

import Data.Proxy
import Miso.Types
import Miso.Html.Types.Event
import Miso.Html.String

import GHCJS.Marshal

defaultEvents :: Proxy DefaultEvents
defaultEvents = Proxy

-- | `EventHandler` object action
data EventHandler action =
  forall returnType . (ToJSVal action, FromJSVal action)
    => EventHandler {
        eventOptions :: Options
      , eventName :: MisoString
      , eventGrammar :: Grammar returnType
      , eventFunction :: returnType -> action
      }
