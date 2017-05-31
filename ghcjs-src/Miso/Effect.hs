module Miso.Effect (
  module Miso.Effect.Storage
, module Miso.Effect.XHR
, module Miso.Effect.DOM
, Effect (..)
, noEff
, (<#)
) where

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM

-- | Capturing effects in update actions
data Effect model action
  = NoEffect model
  | Effect model (IO action)

-- | `NoEffect` smart constructor
noEff :: model -> Effect model action
noEff = NoEffect

-- | `Effect` smart constructor
(<#) :: model -> IO action -> Effect model action
(<#) = Effect
