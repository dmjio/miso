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
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT, mapStateT)
import           Control.Monad.Trans.Writer.Strict (Writer, tell, mapWriter, runWriter)
import           Data.Bifunctor (second, Bifunctor(..))
import           Data.Foldable (for_)
-----------------------------------------------------------------------------
import           Miso.FFI (JSM)
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
mapSub :: (actionA -> actionB) -> Sub actionA -> Sub actionB
mapSub f sub = \sinkB -> let sinkA = sinkB . f
                         in sub sinkA
-----------------------------------------------------------------------------
instance Functor (Effect action) where
  fmap f (Effect m acts) = Effect (f m) acts
-----------------------------------------------------------------------------
instance Applicative (Effect action) where
  pure m = Effect m []
  Effect fModel fActs <*> Effect xModel xActs =
    Effect (fModel xModel) (fActs ++ xActs)
-----------------------------------------------------------------------------
instance Monad (Effect action) where
  return = pure
  Effect m acts >>= f =
    case f m of
      Effect m' acts' -> Effect m' (acts ++ acts')
-----------------------------------------------------------------------------
instance Bifunctor Effect where
  bimap f g (Effect m acts) =
    Effect (g m) (map (\act -> \sink -> act (sink . f)) acts)
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
batchEff model actions = Effect model $
  map (\a sink -> sink =<< a) actions
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
type Transition action model = StateT model (Writer [Sub action])
-----------------------------------------------------------------------------
-- | Turn a transition that schedules subscriptions that consume
-- actions of type @a@ into a transition that schedules subscriptions
-- that consume actions of type @b@ using the supplied function of
-- type @a -> b@.
mapAction :: (actionA -> actionB) -> Transition actionA model r -> Transition actionB model r
mapAction = mapStateT . mapWriter . second . fmap . mapSub
-----------------------------------------------------------------------------
-- | Convert a @Transition@ computation to a function that can be given to @update@.
fromTransition
    :: Transition action model ()
    -> (model -> Effect action model) -- ^ model @update@ function.
fromTransition act = \m ->
  case runWriter (execStateT act m) of
    (n, actions) -> Effect n actions
-----------------------------------------------------------------------------
-- | Convert an @update@ function to a @Transition@ computation.
toTransition
    :: (model -> Effect action model) -- ^ model @update@ function
    -> Transition action model ()
toTransition f =
  StateT $ \m ->
    case f m of
      Effect n actions -> do
        tell actions
        pure ((), n)
-----------------------------------------------------------------------------
-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: JSM action -> Transition action model ()
scheduleIO ioAction = scheduleSub $ \sink -> ioAction >>= sink
-----------------------------------------------------------------------------
-- | Like 'scheduleIO' but doesn't cause an action to be dispatched to
-- the @update@ function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
scheduleIO_ :: JSM () -> Transition action model ()
scheduleIO_ ioAction = scheduleSub $ \_sink -> ioAction
-----------------------------------------------------------------------------
-- | Like 'scheduleIO_ but generalized to any instance of 'Foldable'
--
-- This is handy for scheduling IO computations that return a `Maybe` value
scheduleIOFor_ :: Foldable f => JSM (f action) -> Transition action model ()
scheduleIOFor_ io = scheduleSub $ \sink -> io >>= flip for_ sink
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
scheduleSub :: Sub action -> Transition action model ()
scheduleSub sub = lift $ tell [ sub ]
----------------------------------------------------------------------------- 
