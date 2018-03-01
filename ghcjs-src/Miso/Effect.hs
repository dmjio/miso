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
, Effect (..), Sink
, noEff
, (<#)
, (#>)
, effectWithSink
) where

import Data.Bifunctor

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of IO computations. Each IO
-- computation is run in a new thread so there is no risk of accidentally
-- blocking the application. The IO computation is given a 'Sink' callback which
-- can be used to dispatch actions which are fed back to the @update@ function
data Effect action model
  = Effect model [Sink action -> IO ()]

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
(<#) m a = effectWithSink m $ \sink -> a >>= sink

-- | `Effect` smart constructor, flipped
(#>) :: IO action -> model -> Effect action model
(#>) = flip (<#)

-- | Like '<#' but allows the scheduled IO computation to access a
-- 'Sink' which can be used to asynchronously dispatch actions to the
-- 'update' function.
--
-- A use-case is scheduling an IO computation which creates a
-- 3rd-party JS widget which has an associated callback. The callback
-- can then call the sink to turn events into actions. To do this
-- without accessing a sink requires going via a @'Sub'scription@
-- which introduces a leaky-abstraction.
effectWithSink :: model -> (Sink action -> IO ()) -> Effect action model
effectWithSink model sinkAction = Effect model [sinkAction]
