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
-- It consists of the updated model and a list of actions. Each action
-- is run in a new thread so there is no risk of accidentally
-- blocking the application.
data Effect model action
  = Effect model [IO action]

-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect model action
noEff m = Effect m []

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: model -> IO action -> Effect model action
(<#) m a = Effect m [a]

-- | `Effect` smart constructor, flipped
(#>) :: IO action -> model -> Effect model action
(#>) = flip (<#)
