-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines the t'Effect', t'Sub', and t'Sink' types used to
-- express state transitions and side effects in Miso applications.
--
-- == Overview
--
-- The t'Effect' type is a @RWS@ monad that threads through three concerns:
--
--   * __Reader__: 'ComponentInfo' provides access to the current component's
--     identifier, parent identifier, DOM reference, and @props@.
--   * __Writer__: a list of 'Schedule' values that describe 'IO' actions to
--     be executed (either asynchronously via 'io'\/'io_'\/'withSink', or
--     synchronously via 'sync'\/'sync_').
--   * __State__: the component @model@, updated with the usual @put@\/@modify@
--     operations from @mtl@.
--
-- The 'update' field of a t'Miso.Types.Component' has type
-- @action -> t'Effect' parent props model action@.  Each action is dispatched
-- to 'update', which may change the model and\/or enqueue 'IO' work.
--
-- == Subscriptions
--
-- A t'Sub' is a long-running background computation that can push actions into
-- the component's event queue via its t'Sink' argument.  Subscriptions are
-- registered in the 'subs' field of a t'Miso.Types.Component' and are started
-- once on mount and torn down on unmount.
--
-- == Scheduling
--
-- By default, every 'IO' action enqueued by 'io', 'io_', 'withSink', or
-- 'batch' is executed __asynchronously__ in a separate thread.  Use 'sync' or
-- 'sync_' when you need the action to complete before the next render, but be
-- aware this will block the render thread for that component.
--
-- @since 1.9.0.0
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
  -- ** 'IO'
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
  -- *** Props
  , componentInfoProps
  , props
  , getProps
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
import           Data.Foldable (for_)
import           Control.Monad.RWS (RWS, put, tell, execRWS, censor, MonadReader)
-----------------------------------------------------------------------------
import           Miso.DSL.FFI
import           Miso.Lens
-----------------------------------------------------------------------------
-- | Smart constructor for t'ComponentInfo'.
--
-- Builds a 'ComponentInfo' value from its constituent pieces.  This is
-- primarily used by the Miso runtime when initialising a component; you
-- rarely need to call it directly.
mkComponentInfo
  :: ComponentId
  -- ^ 'ComponentId' of this component
  -> ComponentId
  -- ^ 'ComponentId' of this component's parent (same value as the child when
  --   the component is the root)
  -> DOMRef
  -- ^ DOM node that the component is mounted on
  -> props
  -- ^ Current @props@ passed from the parent component
  -> ComponentInfo parent props
mkComponentInfo = ComponentInfo
-----------------------------------------------------------------------------
-- | The @Reader@ environment carried by the t'Effect' monad.
--
-- 'ComponentInfo' is accessible inside an 'update' handler via
-- 'Control.Monad.Reader.ask' (or the convenience lenses 'componentInfoId',
-- 'componentInfoParentId', 'componentInfoDOMRef', and 'componentInfoProps' /
-- 'props').
--
-- The phantom @parent@ type parameter acts as a witness that ties this
-- component to a specific parent type, which is used when communicating
-- upward through the component tree.
--
-- @since 1.9.0.0
data ComponentInfo parent props
  = ComponentInfo
  { _componentInfoId :: ComponentId
  -- ^ Unique identifier of this component instance
  , _componentInfoParentId :: ComponentId
  -- ^ Unique identifier of the parent component (equals '_componentInfoId'
  --   for a root component)
  , _componentInfoDOMRef :: DOMRef
  -- ^ Reference to the DOM node this component is mounted on
  , _componentInfoProps :: props
  -- ^ Current @props@ supplied by the parent component
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
componentInfoId :: Lens (ComponentInfo parent props) ComponentId
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
componentInfoParentId :: Lens (ComponentInfo parent props) ComponentId
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
componentInfoDOMRef :: Lens (ComponentInfo parent props) DOMRef
componentInfoDOMRef = lens _componentInfoDOMRef $ \r x -> r { _componentInfoDOMRef = x }
-----------------------------------------------------------------------------
-- | Lens for accessing the underlying t'Miso.Types.Component' @props@.
--
-- @
--   update = \case
--     SomeAction -> do
--       props <- view componentInfoProps
--       someAction props
-- @
--
-- @since 1.9.0.0
componentInfoProps :: Lens (ComponentInfo parent props) props
componentInfoProps = lens _componentInfoProps $ \r x -> r { _componentInfoProps = x }
-----------------------------------------------------------------------------
-- | Lens for accessing the underlying t'Miso.Types.Component' @props@.
--
-- This is a shorter convenience lens that is a synonynm for 'componentInfoProps'.
-- See 'getProps' for usage in the 'Effect' monad.
--
-- @
--   update = \case
--     SomeAction ->
--       someAction =<< view props
-- @
--
props :: Lens (ComponentInfo parent props) props
props = componentInfoProps
-----------------------------------------------------------------------------
-- | @props@ retrieval from within the 'Effect' monad.
--
-- @
--   update = \case
--     SomeAction -> do
--       props <- getProps
--       someAction props
-- @
--
getProps :: MonadReader (ComponentInfo parent props) m => m props
getProps = Miso.Lens.view props
-----------------------------------------------------------------------------
-- | Opaque numeric identifier assigned to each mounted t'Miso.Types.Component'
-- instance by the Miso runtime.
--
-- 'ComponentId' values are unique for the lifetime of a component and are
-- used internally for routing messages and managing the component tree.
-- You can read the current component's 'ComponentId' with:
--
-- @
-- myId <- 'Miso.Lens.view' 'componentInfoId'
-- @
type ComponentId = Int
-----------------------------------------------------------------------------
-- | A long-running background computation that feeds actions into a
-- component's event queue.
--
-- A 'Sub' receives a t'Sink' and is free to call it zero or more times, at
-- any point in time (e.g. in response to a timer, a WebSocket message, or
-- a browser event).  Subscriptions are started once when a component mounts
-- and are cancelled when the component unmounts.
--
-- __Example__ — a 15 Hz animation tick:
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad (forever)
--
-- tickSub :: Sub Action
-- tickSub sink = forever $ do
--   threadDelay 66667
--   sink Tick
-- @
--
-- Register subscriptions via the 'Miso.Types.subs' field of a
-- t'Miso.Types.Component'.  For more examples see "Miso.Subscription".
type Sub action = Sink action -> IO ()
-----------------------------------------------------------------------------
-- | A callback that enqueues an @action@ for processing by the component's
-- 'Miso.Types.update' function.
--
-- Calling a 'Sink' is thread-safe and may be done from any 'IO' context,
-- including FFI callbacks and 'Sub' computations.
type Sink action = action -> IO ()
-----------------------------------------------------------------------------
-- | Schedule a single asynchronous 'IO' action that produces the next @action@,
-- and simultaneously set the new @model@.
--
-- This operator is intended as a concise way to write simple update branches:
--
-- @
-- update = \\case
--   Fetch -> myModel '<#' (FetchResult '<$>' fetchData)
-- @
--
-- The @model@ is placed on the left and the @IO@ computation on the right
-- (mnemonic: the hash @#@ looks like a network/queue symbol).
infixl 0 <#
(<#) :: model -> IO action -> Effect parent props model action
(<#) m action = put m >> tell [ async $ \f -> f =<< action ]
-----------------------------------------------------------------------------
async :: (Sink action -> IO ()) -> Schedule action
async = Schedule Async
-----------------------------------------------------------------------------
-- | Flipped version of '<#'.
--
-- Puts the 'IO' action on the left and the new @model@ on the right:
--
-- @
-- update = \\case
--   Fetch -> (FetchResult '<$>' fetchData) '#>' myModel
-- @
infixr 0 #>
(#>) :: IO action -> model -> Effect parent props model action
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple 'IO' actions.
--
-- @since 1.9.0.0
batch
  :: [IO action]
  -- ^ Batch of 'IO' actions to execute
  -> Effect parent props model action
batch actions = sequence_
  [ tell [ async $ \f -> f =<< action ]
  | action <- actions
  ]
-----------------------------------------------------------------------------
-- | Like @batch@ but actions are discarded
--
-- @since 1.9.0.0
batch_ :: [IO ()] -> Effect parent props model action
batch_ actions = sequence_
  [ tell [ async (const action) ]
  | action <- actions
  ]
-----------------------------------------------------------------------------
-- | A monad for succinctly expressing model transitions in the @update@ function.
--
-- t'Effect' is a @RWS@, where the @State@ allows modification to 'model'.
-- It's also a @Writer@ @Monad@, where the accumulator is a list of scheduled
-- @IO@ actions. Multiple actions can be scheduled using 'Control.Monad.Writer.Class.tell'
-- from the @mtl@ library and a single asynchronous action can be scheduled using 'io_'.
--
-- An t'Effect' represents the results of an 'update' action.
--
-- It consists of the updated model and a list of subscriptions. Each t'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
--
-- Tip: use the t'Effect' monad in combination with the stateful "Miso.Lens"
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myComponent = Component
--   { update = \\case
--       MyAction1 -> do
--         field1 '.=' value1
--         counter '+=' 1
--       MyAction2 -> do
--         field2 '%=' f
--         'io_' $ do
--           'consoleLog' \"Hello\"
--           'consoleLog' \"World!\"
--   , ...
--   }
-- @
type Effect parent props model action = RWS (ComponentInfo parent props) [Schedule action] model ()
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
-- | An opaque reference to a live DOM node.
--
-- 'DOMRef' values are handed out by the runtime when a component is mounted
-- and are stored inside 'ComponentInfo'.  They can be passed to FFI
-- functions that expect a JavaScript object representing a DOM element.
type DOMRef = JSVal
-----------------------------------------------------------------------------
-- | Unwrap an t'Effect' computation into a @(model, [Schedule action])@ pair.
--
-- This is an __internal__ function used by the Miso runtime to evaluate the
-- result of an 'update' call.  Application code should not need to call this
-- directly.
runEffect
    :: Effect parent props model action
    -- ^ The effect to run
    -> ComponentInfo parent props
    -- ^ Reader environment (component metadata)
    -> model
    -- ^ Initial model (state seed)
    -> (model, [Schedule action])
    -- ^ Updated model and list of scheduled 'IO' actions
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
sync
  :: IO action
  -- ^ 'IO' action to execute synchronously
  -> Effect parent props model action
sync action = tell [ Schedule Sync $ \f -> f =<< action ]
-----------------------------------------------------------------------------
-- | Like 'sync', except discards the result.
--
-- @since 1.9.0.0
sync_
  :: IO ()
  -- ^ 'IO' action to execute synchronously
  -> Effect parent props model action
sync_ action = tell [ Schedule Sync $ \_ -> action ]
-----------------------------------------------------------------------------
-- | Schedule a single 'IO' action for later execution.
--
-- Note that multiple 'IO' action can be scheduled using
-- 'Control.Monad.Writer.Class.tell' from the @mtl@ library.
--
-- @since 1.9.0.0
io
  :: IO action
  -- ^ 'IO' action to execute asynchronously
  -> Effect parent props model action
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
io_
  :: IO ()
  -- ^ 'IO' action to execute asynchronously
  -> Effect parent props model action
io_ action = withSink (\_ -> void action)
-----------------------------------------------------------------------------
-- | Like 'io' but generalized to any instance of 'Foldable'
--
-- This is handy for scheduling @IO@ computations that return a @Maybe@ value
--
-- @since 1.9.0.0
for
  :: Foldable f
  => IO (f action)
  -- ^ @actions@ executed in batch.
  -> Effect parent props model action
for actions = withSink $ \sink -> actions >>= flip for_ sink
-----------------------------------------------------------------------------
-- | Performs the given 'IO' action before all IO actions collected by the given
-- effect.
--
-- @
-- -- delays connecting a websocket by 100000 microseconds
-- beforeAll (liftIO $ threadDelay 100000) $ websocketConnectJSON OnConnect OnClose OnOpen OnError
-- @
--
-- @since 1.9.0.0
beforeAll :: IO () -> Effect parent props model action -> Effect parent props model action
beforeAll = modifyAllIO . (*>)
-----------------------------------------------------------------------------
-- | Performs the given 'IO' action after all IO actions collected by the given
-- effect.
--
-- Example usage:
--
-- > -- log that running the a websocket Effect completed
-- > afterAll (consoleLog "Done running websocket effect") $ websocketConnectJSON OnConnect OnClose OnOpen OnError
afterAll :: IO () -> Effect parent props model action -> Effect parent props model action
afterAll = modifyAllIO . (<*)
-----------------------------------------------------------------------------
-- | Modifies all 'IO' collected by the given Effect.
--
-- All 'IO' expressions collected by 'Effect' can be evaluated either
-- synchronously or asynchronously (the default).
--
-- This function can be used to adjoin additional actions to all 'IO'
-- expressions in an 'Effect'. For examples see 'beforeAll' and 'afterAll'.
modifyAllIO
  :: (IO () -> IO ())
  -> Effect parent props model action
  -> Effect parent props model action
modifyAllIO f = censor $ \actions ->
  [ Schedule x (f <$> action)
  | Schedule x action <- actions
  ]
-----------------------------------------------------------------------------
-- | @withSink@ allows users to write to the global event queue. This is useful for introducing 'IO' into the system.
-- A synonym for 'Control.Monad.Writer.tell', specialized to 'Effect'.
--
-- A use-case is scheduling an 'IO' computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions.
--
-- @
-- 'update' FetchJSON = 'withSink' $ \\sink -> getJSON (sink . ReceivedJSON) (sink . HandleError)
-- @
--
-- @since 1.9.0.0
withSink
  :: (Sink action -> IO ())
  -- ^ Callback function that provides access to the underlying 'Sink'.
  -> Effect parent props model action
withSink f = tell [ async f ]
-----------------------------------------------------------------------------
-- | Issue a new @action@ to be processed by 'Miso.Types.update'.
--
-- @
-- data Action = HelloWorld
-- type Model  = Int
--
-- 'update' :: Action -> 'Effect' parent Model Action
-- 'update' = \\case
--   Click -> 'issue' HelloWorld
-- @
--
-- @since 1.9.0.0
issue
  :: action
  -- ^ @action@ to raise
  -> Effect parent props model action
issue action = tell [ async $ \f -> f action ]
-----------------------------------------------------------------------------
-- | Helper for t'Miso.Types.Component' construction, when you want to ignore the 'Miso.Types.update'
-- function temporarily, or permanently.
--
-- @since 1.9.0.0
noop :: action -> Effect parent props model action
noop = const (pure ())
-----------------------------------------------------------------------------
-- | Indicates whether a scheduled 'IO' action should run on the render thread
-- (synchronously) or in a background thread (asynchronously).
--
-- Most 'IO' in Miso is 'Async'.  Only use 'Sync' (via 'sync' \/ 'sync_') when
-- the result of the action must be available before the next render cycle,
-- and be aware that blocking the render thread will delay the next paint.
--
-- During component unmounting, all remaining effects are flushed
-- synchronously regardless of this flag.
data Synchronicity
  = Async
  -- ^ Run the action in a background thread (default)
  | Sync
  -- ^ Run the action on the render thread before the next paint
  deriving (Show, Eq)
-----------------------------------------------------------------------------
