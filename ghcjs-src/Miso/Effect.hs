-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect (
  module Miso.Effect.Storage
, module Miso.Effect.XHR
, module Miso.Effect.DOM
, Effect (..), Sub, Sink
, noEff
, (<#)
, (#>)
, effectSub
) where

import Data.Bifunctor

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each 'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
data Effect action model = Effect model [Sub action]

-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'update' function.
type Sub action = Sink action -> IO ()

-- | Function to asynchronously dispatch actions to the 'update' function.
type Sink action = action -> IO ()

instance Functor (Effect action) where
  fmap f (Effect m acts) = Effect (f m) acts

instance Applicative (Effect action) where
  pure m = Effect m []
  Effect fModel fActs <*> Effect xModel xActs = Effect (fModel xModel) (fActs ++ xActs)

instance Monad (Effect action) where
  return = pure
  Effect m acts >>= f =
    case f m of
      Effect m' acts' -> Effect m' (acts ++ acts')

instance Bifunctor Effect where
  bimap f g (Effect m acts) = Effect (g m) (map (\act -> \sink -> act (sink . f)) acts)

-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect action model
noEff m = Effect m []

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: model -> IO action -> Effect action model
(<#) m a = effectSub m $ \sink -> a >>= sink

-- | `Effect` smart constructor, flipped
(#>) :: IO action -> model -> Effect action model
(#>) = flip (<#)

-- | Like '<#' but schedules a subscription which is an IO computation which has
-- access to a 'Sink' which can be used to asynchronously dispatch actions to
-- the 'update' function.
--
-- A use-case is scheduling an IO computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
effectSub :: model -> Sub action -> Effect action model
effectSub model sub = Effect model [sub]
