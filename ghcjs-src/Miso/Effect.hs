-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect (
  module Miso.Effect.Storage
, module Miso.Effect.XHR
, module Miso.Effect.DOM
, Effect (..)
, noEff
, (<#)
, (#>)
) where

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and an optional action. The action
-- is always run in a new thread so there is no risk of accidentally
-- blocking the application.
data Effect model action
  = NoEffect model
  | Effect model (IO action)

-- | `NoEffect` smart constructor
noEff :: model -> Effect model action
noEff = NoEffect

-- | `Effect` smart constructor
(<#) :: model -> IO action -> Effect model action
(<#) = Effect

-- | `Effect` smart constructor, flipped
(#>) :: IO action -> model -> Effect model action
(#>) = flip (<#)
