-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines t'Effect', t'Sub' and t'Sink' types, which are used to define
-- 'Miso.Types.update' function and 'Miso.Types.subs' field of the t'Miso.Types.Component'.
--
----------------------------------------------------------------------------
module Miso.Effect
  ( -- ** Effect
    -- *** Types
    Effect
  , Sub
  , Sink
  , DOMRef
  , ComponentInfo (..)
  , ComponentId
  , mkComponentInfo
  -- ** I/O
  , Schedule (..)
  , Synchronicity (..)
    -- *** Combinators
  , (<#)
  , (#>)
  , batch
  , batch_
  , io
  , io_
  , sync
  , sync_
  , for
  , issue
  , withSink
  , mapSub
  , noop
  , beforeAll
  , afterAll
  , modifyAllIO
  -- *** Lens
  , componentInfoDOMRef
  , componentInfoParentId
  , componentInfoId
  -- *** Internal
  , runEffect
  -- *** Deprecated
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
  , effectSub
  , batchEff
  , noEff
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
import           Data.Foldable (for_)
import           Control.Monad.RWS ( RWS, put, tell, execRWS, censor)
-----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.DSL.FFI
-----------------------------------------------------------------------------
-- | Smart constructor for t'ComponentInfo'
mkComponentInfo
  :: ComponentId
  -- ^ Component Id
  -> ComponentId
  -- ^ Parent Component Id
  -> DOMRef
  -- ^ DOM Reference
  -> ComponentInfo parent
mkComponentInfo = ComponentInfo
-----------------------------------------------------------------------------
-- | This is the 'Reader r' in t'Miso.Effect'. Accessible via 'Control.Monad.Reader.ask'. It holds
-- a phantom type for @parent@. This is used as a witness when calling the
-- @parent@ function, and using 'Miso.Property.prop'.
data ComponentInfo parent
  = ComponentInfo
  { _componentInfoId :: ComponentId
  , _componentInfoParentId :: ComponentId
  , _componentInfoDOMRef :: DOMRef
  }
-----------------------------------------------------------------------------
-- | Lens for accessing the t'ComponentId' from t'ComponentInfo'.
--
-- @
--   update = \case
--     SomeAction -> do
--       compId <- view componentInfoId
--       someAction compId
-- @
--
-- @since 1.9.0.0
componentInfoId :: Lens (ComponentInfo parent) ComponentId
componentInfoId = lens _componentInfoId $ \r x -> r { _componentInfoId = x }
-----------------------------------------------------------------------------
-- | Lens for accessing the parents's  t'ComponentId' from t'ComponentInfo'.
--
-- @
--
-- update = \case
--   SomeAction -> do
--     compParentId <- view componentParentId
--     someAction compParentId
-- @
--
-- @since 1.9.0.0
componentInfoParentId :: Lens (ComponentInfo parent) ComponentId
componentInfoParentId = lens _componentInfoParentId $ \r x -> r { _componentInfoParentId = x }
-----------------------------------------------------------------------------
-- | Lens for accessing the underlying t'Miso.Types.Component' t'DOMRef'.
--
-- @
--   update = \case
--     SomeAction -> do
--       domRef <- view componentDOMRef
--       someAction domRef
-- @
--
-- @since 1.9.0.0
componentInfoDOMRef :: Lens (ComponentInfo parent) DOMRef
componentInfoDOMRef = lens _componentInfoDOMRef $ \r x -> r { _componentInfoDOMRef = x }
-----------------------------------------------------------------------------
-- | 'ComponentId' of the current t'Miso.Types.Component'
type ComponentId = Int
-----------------------------------------------------------------------------
-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back into the 'Miso.Types.update' function.
type Sub action = Sink action -> IO ()
-----------------------------------------------------------------------------
-- | Function to asynchronously dispatch actions to the 'Miso.Types.update' function.
type Sink action = action -> IO ()
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with exactly one action.
infixl 0 <#
(<#) :: model -> IO action -> Effect parent model action
(<#) m action = put m >> tell [ async $ \f -> f =<< action ]
-----------------------------------------------------------------------------
async :: (Sink action -> IO ()) -> Schedule action
async = Schedule Async
-----------------------------------------------------------------------------
-- | `Effect` smart constructor, flipped
infixr 0 #>
(#>) :: IO action -> model -> Effect parent model action
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple actions.
--
-- @since 1.9.0.0
batch :: [IO action] -> Effect parent model action
batch actions = sequence_
  [ tell [ async $ \f -> f =<< action ]
  | action <- actions
  ]
-----------------------------------------------------------------------------
-- | Like @batch@ but actions are discarded
--
-- @since 1.9.0.0
batch_ :: [IO ()] -> Effect parent model action
batch_ actions = sequence_
  [ tell [ async (const action) ]
  | action <- actions
  ]
-----------------------------------------------------------------------------
-- | A monad for succinctly expressing model transitions in the @update@ function.
--
-- t'Effect' is a @RWS@, where the @State@ abstracts over manually passing the model
-- around. It's also a @Writer@ @Monad@, where the accumulator is a list of scheduled
-- @IO@ actions. Multiple actions can be scheduled using 'Control.Monad.Writer.Class.tell'
-- from the @mtl@ library and a single action can be scheduled using 'io_'.
--
-- An t'Effect' represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each t'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
--
-- Tip: use the t'Effect' monad in combination with the stateful
-- <https://hackage.haskell.org/package/lens/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myComponent = Component
--   { update = \\case
--       MyAction1 -> do
--         field1 .= value1
--         counter += 1
--       MyAction2 -> do
--         field2 %= f
--         io_ $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Effect parent model action = RWS (ComponentInfo parent) [Schedule action] model ()
-----------------------------------------------------------------------------
-- | Represents a scheduled 'Effect' that is executed either synchronously
-- or asynchronously.
--
-- All t'IO' is by default asynchronous, use the 'sync' function for synchronous
-- execution. Beware 'sync' can block the render thread for a specific
-- t'Miso.Types.Component'.
--
-- N.B. During t'Miso.Types.Component' unmounting, all effects are evaluated
-- synchronously.
--
-- @since 1.9.0.0
data Schedule action = Schedule Synchronicity (Sink action -> IO ())
-----------------------------------------------------------------------------
-- | Type to represent a DOM reference
type DOMRef = JSVal
-----------------------------------------------------------------------------
-- | Internal function used to unwrap an @Effect@
runEffect
    :: Effect parent model action
    -> ComponentInfo parent
    -> model
    -> (model, [Schedule action])
runEffect = execRWS
-----------------------------------------------------------------------------
-- | Turn a 'Sub' that consumes actions of type @a@ into a 'Sub' that consumes
-- actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (a -> b) -> Sub a -> Sub b
mapSub f sub = \g -> sub (g . f)
-----------------------------------------------------------------------------
-- | Schedule a single 'IO' action, executed synchronously. For asynchronous
-- execution, see 'io'.
--
-- Please use this with caution because it will block the render thread.
--
-- @since 1.9.0.0
sync :: IO action -> Effect parent model action
sync action = tell [ Schedule Sync $ \f -> f =<< action ]
-----------------------------------------------------------------------------
-- | Like 'sync', except discards the result.
--
-- @since 1.9.0.0
sync_ :: IO () -> Effect parent model action
sync_ action = tell [ Schedule Sync $ \_ -> action ]
-----------------------------------------------------------------------------
-- | Schedule a single 'IO' action for later execution.
--
-- Note that multiple 'IO' action can be scheduled using
-- 'Control.Monad.Writer.Class.tell' from the @mtl@ library.
--
-- @since 1.9.0.0
io :: IO action -> Effect parent model action
io action = withSink (action >>=)
-----------------------------------------------------------------------------
-- | Like 'io' but doesn't cause an action to be dispatched to
-- the @update@ function.
--
-- This is handy for scheduling @IO@ computations where you don't care
-- about their results or when they complete.
--
-- Note: The result of @IO a@ is discarded.
--
-- @since 1.9.0.0
io_ :: IO a -> Effect parent model action
io_ action = withSink (\_ -> void action)
-----------------------------------------------------------------------------
-- | Like 'io' but generalized to any instance of 'Foldable'
--
-- This is handy for scheduling @IO@ computations that return a @Maybe@ value
--
-- @since 1.9.0.0
for :: Foldable f => IO (f action) -> Effect parent model action
for actions = withSink $ \sink -> actions >>= flip for_ sink
-----------------------------------------------------------------------------
-- | Performs the given IO action before all IO actions collected by the given
-- effect.
--
-- Example usage:
--
-- > -- delays connecting a websocket by 100000 microseconds
-- > beforeAll (liftIO $ threadDelay 100000) $ websocketConnectJSON OnConnect OnClose OnOpen OnError
beforeAll :: IO () -> Effect parent model action -> Effect parent model action
beforeAll = modifyAllIO . (*>)
-----------------------------------------------------------------------------
-- | Performs the given IO action after all IO actions collected by the given
-- effect.
--
-- Example usage:
--
-- > -- log that running the a websocket Effect completed
-- > afterAll (consoleLog "Done running websocket effect") $ websocketConnectJSON OnConnect OnClose OnOpen OnError
afterAll :: IO () -> Effect parent model action -> Effect parent model action
afterAll = modifyAllIO . (<*)
-----------------------------------------------------------------------------
-- | Modifies all IO collected by the given Effect.
--
-- All 'IO' expressions collected by 'Effect' can be evaluated either
-- synchronously or asynchronously (the default).
--
-- This function can be used to adjoin additional actions to all 'IO'
-- expressions in an 'Effect'. For examples see 'beforeAll' and 'afterAll'.
modifyAllIO
  :: (IO () -> IO ())
  -> Effect parent model action
  -> Effect parent model action
modifyAllIO f = censor $ \actions ->
  [ Schedule x (f <$> action)
  | Schedule x action <- actions
  ]
-----------------------------------------------------------------------------
-- | @withSink@ allows users to access the sink of the t'Miso.Types.Component' or top-level
-- t'Miso.Types.Component' in their application. This is useful for introducing 'IO' into the system.
-- A synonym for 'Control.Monad.Writer.tell', specialized to 'Effect'.
--
-- A use-case is scheduling an 'IO' computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions.
--
-- > update FetchJSON = withSink $ \sink -> getJSON (sink . ReceivedJSON) (sink . HandleError)
--
-- @since 1.9.0.0
withSink :: (Sink action -> IO ()) -> Effect parent model action
withSink f = tell [ async f ]
-----------------------------------------------------------------------------
-- | Issue a new @action@ to be processed by 'Miso.Types.update'.
--
-- > data Action = HelloWorld
-- > type Model  = Int
-- >
-- > update :: Action -> Effect Model Action
-- > update = \case
-- >   Click -> issue HelloWorld
--
-- @since 1.9.0.0
issue :: action -> Effect parent model action
issue action = tell [ async $ \f -> f action ]
-----------------------------------------------------------------------------
-- | See 'io'
{-# DEPRECATED scheduleIO "Please use 'io' instead" #-}
scheduleIO :: IO action -> Effect parent model action
scheduleIO = io
-----------------------------------------------------------------------------
-- | See 'io_'
{-# DEPRECATED scheduleIO_ "Please use 'io_' instead" #-}
scheduleIO_ :: IO () -> Effect parent model action
scheduleIO_ = io_
-----------------------------------------------------------------------------
-- | See 'for'
{-# DEPRECATED scheduleIOFor_ "Please use 'for' instead" #-}
scheduleIOFor_ :: Foldable f => IO (f action) -> Effect parent model action
scheduleIOFor_ = for
-----------------------------------------------------------------------------
-- | See 'withSink'
{-# DEPRECATED scheduleSub "Please use 'withSink' instead" #-}
scheduleSub :: (Sink action -> IO ()) -> Effect parent model action
scheduleSub = withSink
-----------------------------------------------------------------------------
-- | See 'withSink', 'put'
{-# DEPRECATED effectSub "Please use 'put' and 'withSink' instead " #-}
effectSub :: model -> (Sink action -> IO ()) -> Effect parent model action
effectSub m s = put m >> withSink s
-----------------------------------------------------------------------------
-- | See 'put'
{-# DEPRECATED noEff "Please use 'put' instead " #-}
noEff :: model -> Effect parent model action
noEff = put
-----------------------------------------------------------------------------
-- | See 'put', 'batch'
{-# DEPRECATED batchEff "Please use 'put' and 'batch' instead " #-}
batchEff :: model -> [IO action] -> Effect parent model action
batchEff model actions = do
  put model
  batch actions
-----------------------------------------------------------------------------
-- | Helper for t'Miso.Types.Component' construction, when you want to ignore the 'Miso.Types.update'
-- function temporarily, or permanently.
--
-- @since 1.9.0.0
noop :: action -> Effect parent model action
noop = const (pure ())
-----------------------------------------------------------------------------
-- | Data type to indicate if effects should be handled asynchronously
-- or synchronously.
--
data Synchronicity
  = Async
  | Sync
  deriving (Show, Eq)
-----------------------------------------------------------------------------
