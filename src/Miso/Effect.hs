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
  ( -- ** Effect
    -- *** Types
    Effect
  , Sub
  , Sink
    -- *** Combinators
  , effectSub
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
  , mapSub
  , noEff
  , (<#)
  , (#>)
  , batchEff
  , io
  , issue
  -- * Internal
  , runEffect
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT)
import           Control.Monad.State (MonadState, put)
import           Control.Monad.Trans.Writer.Strict (Writer, runWriter)
import           Control.Monad.Writer (tell, MonadWriter)
import           Data.Foldable (for_)
-----------------------------------------------------------------------------
import           Miso.FFI (JSM)
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
-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect action model ()
noEff = put
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with exactly one action.
infixl 0 <#
(<#) :: model -> JSM action -> Effect action model ()
(<#) m action = put m >> tell [ \sink -> sink =<< action ]
-----------------------------------------------------------------------------
-- | `Effect` smart constructor, flipped
infixr 0 #>
(#>) :: JSM action -> model -> Effect action model ()
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple actions.
batchEff :: model -> [JSM action] -> Effect action model ()
batchEff model actions = put model >> do
  forM_ actions $ \action ->
    tell [ \sink -> sink =<< action ]
-----------------------------------------------------------------------------
-- | Like '<#' but schedules a subscription which is an IO computation which has
-- access to a 'Sink' which can be used to asynchronously dispatch actions to
-- the 'Miso.Types.update' function.
--
-- A use-case is scheduling an IO computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
effectSub :: model -> Sub action -> Effect action model ()
effectSub model sub = do
  put model
  tell [sub]
-----------------------------------------------------------------------------
-- | A monad for succinctly expressing model transitions in the @update@ function.
--
-- @Effect@ is a state monad so it abstracts over manually passing the model
-- around. It's also a writer monad where the accumulator is a list of scheduled
-- IO actions. Multiple actions can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library and a single action
-- can be scheduled using 'scheduleIO'.
--
-- An @Effect@ represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each 'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
--
-- Tip: use the @Effect@ monad in combination with the stateful
-- <http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myApp = App
--   { update = \\case
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
newtype Effect action model a = Effect (StateT model (Writer [Sub action]) a)
  deriving (Functor, Applicative, Monad, MonadState model, MonadWriter [Sub action])
-----------------------------------------------------------------------------
-- | Internal function used to unwrap an 'Effect'
runEffect
    :: model
    -> Effect action model a
    -> (model, [Sub action])
runEffect m (Effect action) = runWriter (execStateT action m)
-----------------------------------------------------------------------------
-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: JSM action -> Effect action model ()
scheduleIO action = scheduleSub (\sink -> action >>= sink)
-----------------------------------------------------------------------------
-- | Like 'scheduleIO' but doesn't cause an action to be dispatched to
-- the @update@ function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
scheduleIO_ :: JSM () -> Effect action model ()
scheduleIO_ action = scheduleSub (\_ -> action)
-----------------------------------------------------------------------------
-- | Like 'scheduleIO_ but generalized to any instance of 'Foldable'
--
-- This is handy for scheduling IO computations that return a `Maybe` value
scheduleIOFor_ :: Foldable f => JSM (f action) -> Effect action model ()
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
scheduleSub :: Sub action -> Effect action model ()
scheduleSub sub = Effect $ lift $ tell [ sub ]
----------------------------------------------------------------------------- 
-- | A synonym for @tell@, specialized to @Effect@
--
-- > update :: Action -> Effect Action Model ()
-- > update = \case
-- >   Click -> issue HelloWorld
--
-- @since 1.9.0.0
--
-- Used to issue new @action@
issue :: action -> Effect action model ()
issue action = tell [ \sink -> sink action ]
-----------------------------------------------------------------------------
-- | A shorter synonym for @scheduleIO_@
--
-- > update :: Action -> Effect Action Model ()
-- > update = \case
-- >   HelloWorld -> io (consoleLog "Hello World")
--
-- @since 1.9.0.0
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
io :: JSM () -> Effect action model ()
io = scheduleIO_
-----------------------------------------------------------------------------
