module Miso.Effect where

import Miso.Types

noEffect, noEff :: model -> Effect action model
noEff = noEffect
noEffect = NoEffect

effect :: model -> IO action -> Effect action model
effect = Effect

(<#) :: IO action -> model -> Effect action model
action <# model = Effect model action
infixl 0 <#
