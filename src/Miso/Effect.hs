-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines `Effect`, `Sub` and `Sink` types, which are used to define
-- `Miso.Types.update` function and `Miso.Types.subs` field of the `Miso.Types.App`.
----------------------------------------------------------------------------
module Miso.Effect
  ( -- ** Transition
    -- *** Types
    Transition
    -- *** Combinators
  , effectSub
  , mapAction
  , fromTransition
  , toTransition
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
    -- ** Effect
    -- *** Types
  , Effect (..)
  , Sub
  , Sink
    -- *** Combinators
  , mapSub
  , noEff
  , (<#)
  , (#>)
  , batchEff
  ) where
-----------------------------------------------------------------------------
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(StateT), execStateT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Writer.Strict (Writer, tell, runWriter)
import Control.Monad.Writer (MonadWriter)
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (for_)
-----------------------------------------------------------------------------
import Miso.FFI (JSM)
-----------------------------------------------------------------------------
-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each 'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
data Effect action model = Effect model [Sub action]
-----------------------------------------------------------------------------
-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'Miso.Types.update' function.
type Sub action = Sink action -> JSM ()
-----------------------------------------------------------------------------
-- | Function to asynchronously dispatch actions to the 'Miso.Types.update' function.
type Sink action = action -> JSM ()
-----------------------------------------------------------------------------
-- | Turn a subscription that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (a -> b) -> Sub a -> Sub b
mapSub f sub = \g -> sub (g . f)
-----------------------------------------------------------------------------
instance Functor (Effect action) where
  fmap f (Effect model actions) = Effect (f model) actions
-----------------------------------------------------------------------------
instance Applicative (Effect action) where
  pure m = Effect m []
  Effect f ys <*> Effect x zs = Effect (f x) (ys ++ zs)
-----------------------------------------------------------------------------
instance Monad (Effect action) where
  return = pure
  Effect m xs >>= f | Effect n ys <- f m = Effect n (xs ++ ys)
-----------------------------------------------------------------------------
instance Bifunctor Effect where
  bimap f g (Effect m actions) =
    Effect (g m) [ \sink -> action (sink . f) | action <- actions ]
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect action model
noEff m = Effect m []
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with exactly one action.
infixl 0 <#
(<#) :: model -> JSM action -> Effect action model
(<#) m a = effectSub m $ \sink -> a >>= sink
-----------------------------------------------------------------------------
-- | `Effect` smart constructor, flipped
infixr 0 #>
(#>) :: JSM action -> model -> Effect action model
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple actions.
batchEff :: model -> [JSM action] -> Effect action model
batchEff model actions =
  Effect model [ \sink -> action >>= sink | action <- actions ]
-----------------------------------------------------------------------------
-- | Like '<#' but schedules a subscription which is an IO computation which has
-- access to a 'Sink' which can be used to asynchronously dispatch actions to
-- the 'Miso.Types.update' function.
--
-- A use-case is scheduling an IO computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
effectSub :: model -> Sub action -> Effect action model
effectSub model sub = Effect model [sub]
-----------------------------------------------------------------------------
-- | A monad for succinctly expressing model transitions in the @update@ function.
--
-- @Transition@ is a state monad so it abstracts over manually passing the model
-- around. It's also a writer monad where the accumulator is a list of scheduled
-- IO actions. Multiple actions can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library and a single action
-- can be scheduled using 'scheduleIO'.
--
-- Tip: use the @Transition@ monad in combination with the stateful
-- <http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myApp = App
--   { update = 'fromTransition' . \\case
--       MyAction1 -> do
--         field1 .= value1
--         counter += 1
--       MyAction2 -> do
--         field2 %= f
--         scheduleIO $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Transition action model = TransitionCore action model ()

newtype TransitionCore action model a = TransitionCore (StateT model (Writer [Sub action]) a)
  deriving (Functor, Applicative, Monad, MonadState model, MonadWriter [Sub action])
-----------------------------------------------------------------------------
-- | Turn a transition that schedules subscriptions that consume
-- actions of type @a@ into a transition that schedules subscriptions
-- that consume actions of type @b@ using the supplied function of
-- type @a -> b@.
mapAction :: (a -> b) -> Transition a m -> Transition b m
mapAction _ (TransitionCore _)) = undefined
--  = Transition (mapStateT . mapWriter . second . fmap . mapSub $ x)
-----------------------------------------------------------------------------
-- | Convert a @Transition@ computation to a function that can be given to @update@.
fromTransition
    :: Transition action model
    -> (model -> Effect action model) -- ^ model @update@ function.
fromTransition (Transition (TransitionCore act)) = \m ->
  case runWriter (execStateT act m) of
    (n, actions) -> Effect n actions
-----------------------------------------------------------------------------
-- | Convert an @update@ function to a @Transition@ computation.
toTransition
    :: (model -> Effect action model) -- ^ model @update@ function
    -> Transition action model
toTransition f =
  Transition . TransitionCore . StateT $ \m ->
    case f m of
      Effect n actions ->
        ((), n) <$ tell actions
-----------------------------------------------------------------------------
-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: JSM action -> Transition action model
scheduleIO action = scheduleSub (\sink -> action >>= sink)
-----------------------------------------------------------------------------
-- | Like 'scheduleIO' but doesn't cause an action to be dispatched to
-- the @update@ function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
scheduleIO_ :: JSM () -> Transition action model
scheduleIO_ action = scheduleSub (\_ -> action)
-----------------------------------------------------------------------------
-- | Like 'scheduleIO_ but generalized to any instance of 'Foldable'
--
-- This is handy for scheduling IO computations that return a `Maybe` value
scheduleIOFor_ :: Foldable f => JSM (f action) -> Transition action model
scheduleIOFor_ action = scheduleSub $ \sink -> action >>= flip for_ sink
-----------------------------------------------------------------------------
-- | Like 'scheduleIO' but schedules a subscription which is an IO
-- computation that has access to a 'Sink' which can be used to
-- asynchronously dispatch actions to the @update@ function.
--
-- A use-case is scheduling an IO computation which creates a
-- 3rd-party JS widget which has an associated callback. The callback
-- can then call the sink to turn events into actions. To do this
-- without accessing a sink requires going via a @'Sub'scription@
-- which introduces a leaky-abstraction.
scheduleSub :: Sub action -> Transition action model
scheduleSub sub = Transition $ TransitionCore $ lift $ tell [ sub ]
----------------------------------------------------------------------------- 
