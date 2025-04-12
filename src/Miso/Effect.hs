-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
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
  , mapSub
  , (<#)
  , (#>)
  , batch
  , io
  , io_
  , issue
  , withSink
  -- * Internal
  , runEffect
  ) where
-----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import           Control.Monad.Fail (MonadFail, fail)
import qualified Control.Monad.Fail as Fail
#endif
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT)
import           Control.Monad.State (MonadState, put)
import           Control.Monad.Trans.Writer.Strict (Writer, runWriter)
import           Control.Monad.Writer (tell, MonadWriter)
-----------------------------------------------------------------------------
import           Miso.FFI.Internal (JSM, consoleError)
import           Miso.String (ms)
-----------------------------------------------------------------------------
-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'Miso.Types.update' function.
type Sub action = (action -> JSM ()) -> JSM ()
-----------------------------------------------------------------------------
-- | Function to asynchronously dispatch actions to the 'Miso.Types.update' function.
type Sink action = action -> JSM ()
-----------------------------------------------------------------------------
-- | Turn a subscription that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (a -> b) -> Sub a -> Sub b
mapSub f sub = \g -> sub (g . f)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with exactly one action.
infixl 0 <#
(<#) :: model -> JSM action -> Effect model action ()
(<#) m action = put m >> tell [ \sink -> sink =<< action ]
-----------------------------------------------------------------------------
-- | `Effect` smart constructor, flipped
infixr 0 #>
(#>) :: JSM action -> model -> Effect model action ()
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple actions.
batch :: [JSM action] -> Effect model action ()
batch actions = tell
  [ \sink -> sink =<< action
  | action <- actions
  ]
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
--         io $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
newtype Effect model action a = Effect (StateT model (Writer [Sub action]) a)
  deriving (Functor, Applicative, Monad, MonadState model, MonadWriter [Sub action])
-----------------------------------------------------------------------------
-- | @MonadFail@ instance for @Effect@
instance MonadFail (Effect model action) where
  fail s = do
    io_ $ consoleError (ms s)
#if __GLASGOW_HASKELL__ <= 881
    Fail.fail s
#else
    fail s
#endif
-----------------------------------------------------------------------------
-- | Internal function used to unwrap an 'Effect'
runEffect
    :: model
    -> Effect model action a
    -> (model, [Sub action])
runEffect m (Effect action) = runWriter (execStateT action m)
-----------------------------------------------------------------------------
-- | A synonym for @tell@, specialized to @Effect@
--
-- > update :: Action -> Effect Model Action ()
-- > update = \case
-- >   Click -> issue HelloWorld
--
-- @since 1.9.0.0
--
-- Used to issue new @action@
issue :: action -> Effect model action ()
issue action = withSink $ \sink -> sink action
-----------------------------------------------------------------------------
-- | A synonym for @tell@, specialized to @Effect@
--
-- > update :: Action -> Effect Model Action ()
-- > update = \case
-- >   Click -> withSink $ \sink -> sink HelloWorld
--
-- @since 1.9.0.0
--
-- Used to issue new @action@
withSink :: (Sink action -> JSM ()) -> Effect model action ()
withSink action = tell [ action ]
-----------------------------------------------------------------------------
-- | A shorter synonym for @scheduleIO_@
--
-- > update :: Action -> Effect Model Action ()
-- > update = \case
-- >   HelloWorld -> io (consoleLog "Hello World")
--
-- @since 1.9.0.0
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
io_ :: JSM () -> Effect model action ()
io_ action = withSink (\_ -> action)
-----------------------------------------------------------------------------
-- | A shorter synonym for @scheduleIO_@
--
-- > update :: Action -> Effect Model Action ()
-- > update = \case
-- >   HelloWorld -> io (consoleLog "Hello World")
--
-- @since 1.9.0.0
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
io :: JSM action -> Effect model action ()
io action = withSink (\sink -> action >>= sink)
-----------------------------------------------------------------------------
