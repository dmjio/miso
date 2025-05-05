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
--
----------------------------------------------------------------------------
module Miso.Effect
  ( -- ** Effect
    -- *** Types
    Effect
  , Sub
  , SubName
  , Sink
    -- *** Combinators
  , noEff
  , (<#)
  , (#>)
  , batch
  , io
  , io_
  , for
  , issue
  , sink
  , mapSub
  -- * Internal
  , runEffect
  -- * Deprecated
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
  , effectSub
  , batchEff
  ) where
-----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import           Control.Monad.Fail (MonadFail, fail)
import qualified Control.Monad.Fail as Fail
#endif
import           Data.Foldable (for_)
import           Control.Monad.RWS ( RWS, put, tell, execRWS
                                   , MonadState, MonadReader, MonadWriter
                                   )
-----------------------------------------------------------------------------
import           Miso.FFI.Internal (JSM, consoleError)
import           Miso.String (ms, MisoString)
-----------------------------------------------------------------------------
-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'Miso.Types.update' function.
type Sub action = Sink action -> JSM ()
-----------------------------------------------------------------------------
-- | SubName
-- The name of a 'Sub' (e.g. "websocket")
--
type SubName = MisoString
-----------------------------------------------------------------------------
-- | Function to asynchronously dispatch actions to the 'Miso.Types.update' function.
type Sink action = action -> JSM ()
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with exactly one action.
infixl 0 <#
(<#) :: model -> JSM action -> Effect model action
(<#) m action = put m >> tell [ \f -> f =<< action ]
-----------------------------------------------------------------------------
-- | `Effect` smart constructor, flipped
infixr 0 #>
(#>) :: JSM action -> model -> Effect model action
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple actions.
batch :: [JSM action] -> Effect model action
batch actions
  = sequence_
  [ tell [ \f -> f =<< action ]
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
--         scheduleIO $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Effect model action = EffectCore model action ()
-----------------------------------------------------------------------------
-- | The EffectCore Monad, underlies @Effect@
type ComponentName = MisoString
-----------------------------------------------------------------------------
-- | The EffectCore Monad, underlies @Effect@
newtype EffectCore model action a
  = EffectCore
  { runEffectCore :: RWS ComponentName [Sink action -> JSM ()] model a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState model
    , MonadWriter [Sink action -> JSM ()]
    , MonadReader ComponentName
    )
-----------------------------------------------------------------------------
-- | @MonadFail@ instance for @EffectCore@
instance MonadFail (EffectCore model action) where
  fail s = do
    io $ consoleError (ms s)
#if __GLASGOW_HASKELL__ <= 881
    Fail.fail s
#else
    fail s
#endif
-----------------------------------------------------------------------------
-- | Internal function used to unwrap an 'Effect'
runEffect
    :: Effect model action
    -> MisoString
    -> model
    -> (model, [Sink action -> JSM ()])
runEffect = execRWS . runEffectCore
-----------------------------------------------------------------------------
-- | Turn a 'Sub' that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (a -> b) -> Sub a -> Sub b
mapSub f sub = \g -> sub (g . f)
-----------------------------------------------------------------------------
-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
io_ :: JSM action -> Effect model action
io_ action = sink (action >>=)
-----------------------------------------------------------------------------
-- | Like 'io' but doesn't cause an action to be dispatched to
-- the @update@ function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
io :: JSM () -> Effect model action
io action = sink (\_ -> action)
-----------------------------------------------------------------------------
-- | Like 'io' but generalized to any instance of 'Foldable'
--
-- This is handy for scheduling IO computations that return a `Maybe` value
--
for :: Foldable f => JSM (f action) -> Effect model action
for actions = sink $ \write -> actions >>= flip for_ write
-----------------------------------------------------------------------------
-- | 'withSink' allows users to access the sink of the 'Component' or top-level
-- 'App' in their application. This is useful for introducing I/O into the system.
--
-- A use-case is scheduling an 'IO' computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
--
-- > update FetchJSON = withSink $ \sink -> getJSON (sink . ReceivedJSON) (sink . HandleError)
--
sink :: (Sink action -> JSM ()) -> Effect model action
sink f = tell [ f ]
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
issue :: action -> Effect model action
issue action = tell [ \f -> f action ]
-----------------------------------------------------------------------------
{-# DEPRECATED scheduleIO "Please use 'io_' instead" #-}
scheduleIO :: JSM action -> Effect model action
scheduleIO = io_
-----------------------------------------------------------------------------
{-# DEPRECATED scheduleIO_ "Please use 'io' instead" #-}
scheduleIO_ :: JSM () -> Effect model action
scheduleIO_ = io
-----------------------------------------------------------------------------
{-# DEPRECATED scheduleIOFor_ "Please use 'for' instead" #-}
scheduleIOFor_ :: Foldable f => JSM (f action) -> Effect model action
scheduleIOFor_ = for
-----------------------------------------------------------------------------
{-# DEPRECATED scheduleSub "Please use 'sink' instead" #-}
scheduleSub :: (Sink action -> JSM ()) -> Effect model action
scheduleSub = sink
-----------------------------------------------------------------------------
{-# DEPRECATED effectSub "Please use 'put' and 'sink' instead " #-}
effectSub :: model -> (Sink action -> JSM ()) -> Effect model action
effectSub m s = put m >> sink s
-----------------------------------------------------------------------------
{-# DEPRECATED noEff "Please use 'put' instead " #-}
noEff :: model -> Effect model action
noEff = put
-----------------------------------------------------------------------------
{-# DEPRECATED batchEff "Please use 'put' and 'batch' instead " #-}
batchEff :: model -> [JSM action] -> Effect model action
batchEff model actions = do
  put model
  batch actions
-----------------------------------------------------------------------------
