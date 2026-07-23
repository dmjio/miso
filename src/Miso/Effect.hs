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
-- = Overview
--
-- "Miso.Effect" defines the three core abstractions used in the
-- Model-View-Update loop:
--
-- * 'Effect' — the monad returned by every 'Miso.Types.update' handler.
--   Combines a state update on @model@ with a list of 'IO' actions to
--   schedule.
--
-- * 'Sub' — a long-running subscription (@'Sink' action -> IO ()@) that
--   feeds actions into the event queue from threads, timers, WebSockets, etc.
--
-- * 'Sink' — a function (@action -> IO ()@) that enqueues a single action
--   for processing by 'Miso.Types.update'.
--
-- = The Effect monad
--
-- @
-- type 'Effect' context props model action
--      = RWS ('ComponentInfo' context props) ['Schedule' context action] model ()
-- @
--
-- The @RWS@ decomposition:
--
-- * __Reader__ — 'ComponentInfo': component metadata ('componentInfoId',
--   'componentInfoDOMRef', 'componentInfoProps') accessible via 'Control.Monad.Reader.ask'
--   or the convenience lenses.
-- * __Writer__ — accumulated list of 'Schedule'd 'IO' actions to run after
--   the model update.
-- * __State__ — the @model@, updated via 'Control.Monad.State.put',
--   'Control.Monad.State.modify', or the lens operators from "Miso.Lens".
--
-- = Scheduling IO
--
-- By default all 'IO' runs asynchronously in a separate thread after the
-- VDOM has been patched. Use 'sync' \/ 'sync_' to block the render thread:
--
-- @
-- update = \\case
--   Fetch    -> 'io'   (fetchData >>= pure . GotData)  -- async
--   LogIt    -> 'io_'  (consoleLog \"hi\")               -- async, no action
--   Urgent   -> 'sync' (pure SomeSyncAction)           -- blocks render
--   Many     -> 'batch' [a1, a2, a3]                   -- multiple async
--   Opt      -> 'for'  (fetchMaybe >>= pure)           -- Maybe\/Foldable
-- @
--
-- = Subscriptions
--
-- A 'Sub' is a function that receives a 'Sink' and runs forever (typically
-- on a forked thread). Register subscriptions in 'Miso.Types.subs':
--
-- @
-- tickSub :: 'Sub' Action
-- tickSub sink = forever $ do
--   threadDelay 16667
--   sink Tick
--
-- myComponent = ('Miso.component' model update view) { 'Miso.Types.subs' = [tickSub] }
-- @
--
-- Use 'mapSub' to adapt a @Sub a@ into a @Sub b@ with a mapping function.
--
-- = Component metadata
--
-- Within 'update', access the current component's runtime info through
-- 'Control.Monad.Reader.ask' or the provided lenses:
--
-- @
-- update = \\case
--   Init -> do
--     domRef <- 'Miso.Lens.view' 'componentInfoDOMRef'
--     compId <- 'Miso.Lens.view' 'componentInfoId'
--     myProps <- 'getProps'
--     io_ (initThirdParty domRef)
-- @
--
-- = See also
--
-- * "Miso.Types" — 'Miso.Types.Component', 'Miso.Types.update', 'Miso.Types.subs'
-- * "Miso.Lens" — lens operators (@'.='@, @'+='@, @'%='@) for model updates
-- * "Miso.Subscription" — pre-built subscriptions (mouse, keyboard, history, …)
-----------------------------------------------------------------------------
module Miso.Effect
  ( -- ** Effect
    -- *** Types
    Effect
  , Sub
  , Sink
  , DOMRef
  , ComponentInfo (..)
  , ComponentId
  , Thread (..)
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
  , modifyContext
  , modifyContext_
  , putContext
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
  -- *** Context
  , componentInfoContext
  , context
  , getContext
  -- *** Dual-thread
  , runOnBG
  , runOnMain
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
import           Data.Foldable (traverse_)
import           Control.Monad.RWS (RWS, put, tell, execRWS, censor, MonadReader)
import           Control.Monad.State (State, execState)
-----------------------------------------------------------------------------
import           Miso.DSL.FFI
import           Miso.Lens
-----------------------------------------------------------------------------
-- | Smart constructor for t'ComponentInfo'
mkComponentInfo
  :: ComponentId
  -- ^ 'ComponentId'
  -> ComponentId
  -- ^ @parent@ 'ComponentId'
  -> DOMRef
  -- ^ 'DOMRef'
  -> props
  -- ^ props
  -> context
  -- ^ context
  -> ComponentInfo context props
mkComponentInfo = ComponentInfo
-----------------------------------------------------------------------------
-- | This is the 'Reader r' in t'Miso.Effect'. Accessible via 'Control.Monad.Reader.ask'. It holds
-- a phantom type for @context@ (the app-global React-style context, which is
-- write-only from within 'update'). It gives access to 'Component' metadata such
-- as the 'DOMRef' the 'Component' was mounted on and the 'ComponentId' associated
-- with it.
data ComponentInfo context props
  = ComponentInfo
  { _componentInfoId :: ComponentId
  -- ^ Unique identifier for this component instance
  , _componentInfoParentId :: ComponentId
  -- ^ Unique identifier of the parent component (same as '_componentInfoId' for root components)
  , _componentInfoDOMRef :: DOMRef
  -- ^ The DOM node this component is mounted on
  , _componentInfoProps :: props
  -- ^ The current @props@ value passed into this component
  , _componentInfoContext :: context
  -- ^ The current @context@ value passed into this component
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
componentInfoId :: Lens (ComponentInfo context props) ComponentId
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
componentInfoParentId :: Lens (ComponentInfo context props) ComponentId
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
componentInfoDOMRef :: Lens (ComponentInfo context props) DOMRef
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
componentInfoProps :: Lens (ComponentInfo context props) props
componentInfoProps = lens _componentInfoProps $ \r x -> r { _componentInfoProps = x }
-----------------------------------------------------------------------------
-- | Lens for accessing the underlying t'Miso.Types.Component' @context@.
--
-- @
--   update = \case
--     SomeAction -> do
--       ctx <- view componentInfoContext
--       someAction ctx
-- @
--
-- @since 1.13.0.0
componentInfoContext :: Lens (ComponentInfo context props) context
componentInfoContext = lens _componentInfoContext $ \r x -> r { _componentInfoContext = x }
-----------------------------------------------------------------------------
-- | Lens for accessing the underlying t'Miso.Types.Component' @context@.
--
-- This is a shorter convenience lens that is a synonynm for 'componentInfoContext'.
-- See 'getContext' for usage in the 'Effect' monad.
--
-- @
--   update = \case
--     SomeAction ->
--       someAction =<< view context
-- @
--
-- __Note:__ this lens is __read-only__ within 'Effect'. It targets the
-- 'ComponentInfo' reader environment, so setting through it (e.g. with
-- 'Miso.Lens.set' \/ 'Miso.Lens..=') has no observable effect. To change the
-- global @context@ from 'Miso.Types.update', use 'Miso.Effect.modifyContext',
-- 'Miso.Effect.putContext', or 'Miso.Effect.modifyContext_' (the 'State'-monad
-- variant, which supports lens operators like @'Miso.Lens..='@) instead.
--
-- @since 1.13.0.0
context :: Lens (ComponentInfo context props) context
context = componentInfoContext
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
props :: Lens (ComponentInfo context props) props
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
getProps :: MonadReader (ComponentInfo context props) m => m props
getProps = Miso.Lens.view props
-----------------------------------------------------------------------------
-- | Read-only @context@ retrieval from within the 'Effect' monad.
--
-- @
--   update = \case
--     SomeAction -> do
--       ctx <- getContext
--       someAction ctx
-- @
--
-- @since 1.13.0.0
getContext :: MonadReader (ComponentInfo context props) m => m context
getContext = Miso.Lens.view context
-----------------------------------------------------------------------------
-- | 'ComponentId' of the current t'Miso.Types.Component'
type ComponentId = Int
-----------------------------------------------------------------------------
-- | Type synonym for constructing subscriptions.
--
-- For example usage see "Miso.Subscription"
--
-- The 'Sink' function is used to write to the global event queue.
type Sub action = Sink action -> IO ()
-----------------------------------------------------------------------------
-- | Function to write to the global event queue for processing by the scheduler.
type Sink action = action -> IO ()
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with exactly one action.
infixl 0 <#
(<#) :: model -> IO action -> Effect context props model action
(<#) m action = put m >> tell [ async $ \f -> f =<< action ]
-----------------------------------------------------------------------------
async :: (Sink action -> IO ()) -> Schedule context action
async = Schedule Nothing Async
-----------------------------------------------------------------------------
runOnBG :: (Sink action -> IO ()) -> Effect context props model action
runOnBG f = tell [ Schedule (Just BTS) Async f ]
-----------------------------------------------------------------------------
runOnMain :: (Sink action -> IO ()) -> Effect context props model action
runOnMain f = tell [ Schedule (Just MTS) Async f ]
-----------------------------------------------------------------------------
-- | `Effect` smart constructor, flipped
infixr 0 #>
(#>) :: IO action -> model -> Effect context props model action
(#>) = flip (<#)
-----------------------------------------------------------------------------
-- | Smart constructor for an 'Effect' with multiple 'IO' actions.
--
-- @since 1.9.0.0
batch
  :: [IO action]
  -- ^ Batch of 'IO' actions to execute
  -> Effect context props model action
batch actions = sequence_
  [ tell [ async $ \f -> f =<< action ]
  | action <- actions
  ]
-----------------------------------------------------------------------------
-- | Like @batch@ but actions are discarded
--
-- @since 1.9.0.0
batch_ :: [IO ()] -> Effect context props model action
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
type Effect context props model action = RWS (ComponentInfo context props) [Schedule context action] model ()
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
-- The 'ContextModify' constructor carries a pending mutation to the app-global
-- React-style @context@. It is emitted by 'modifyContext' \/ 'putContext' and
-- applied to the global context during the scheduler's commit phase, triggering
-- a re-render of every 'Component' with @useContext@ enabled.
--
-- @since 1.9.0.0
data Schedule context action
  = Schedule (Maybe Thread) Synchronicity (Sink action -> IO ())
  | ContextModify (context -> context)
-----------------------------------------------------------------------------
-- | Type to represent a DOM reference
type DOMRef = JSVal
-----------------------------------------------------------------------------
-- | Internal function used to unwrap an @Effect@
runEffect
    :: Effect context props model action
    -> ComponentInfo context props
    -> model
    -> (model, [Schedule context action])
runEffect = execRWS
-----------------------------------------------------------------------------
-- | Turn a 'Sub' that consumes actions of type @a@ into a 'Sub' that consumes
-- actions of type @b@ using the supplied function of type @a -> b@.
mapSub
  :: (a -> b)
  -- ^ Function to map actions produced by the subscription
  -> Sub a
  -- ^ Source subscription delivering @a@ actions
  -> Sub b
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
  -> Effect context props model action
sync action = tell [ Schedule Nothing Sync $ \f -> f =<< action ]
-----------------------------------------------------------------------------
-- | Like 'sync', except discards the result.
--
-- @since 1.9.0.0
sync_
  :: IO ()
  -- ^ 'IO' action to execute synchronously
  -> Effect context props model action
sync_ action = tell [ Schedule Nothing Sync $ \_ -> action ]
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
  -> Effect context props model action
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
  -> Effect context props model action
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
  -> Effect context props model action
for actions = withSink $ \sink -> actions >>= traverse_ sink
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
beforeAll
  :: IO ()
  -- ^ 'IO' action to prepend before all scheduled effects
  -> Effect context props model action
  -- ^ Effect whose IO actions are modified
  -> Effect context props model action
beforeAll = modifyAllIO . (*>)
-----------------------------------------------------------------------------
-- | Performs the given 'IO' action after all IO actions collected by the given
-- effect.
--
-- Example usage:
--
-- > -- log that running the a websocket Effect completed
-- > afterAll (consoleLog "Done running websocket effect") $ websocketConnectJSON OnConnect OnClose OnOpen OnError
afterAll
  :: IO ()
  -- ^ 'IO' action to append after all scheduled effects
  -> Effect context props model action
  -- ^ Effect whose IO actions are modified
  -> Effect context props model action
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
  -- ^ Transform to apply to every scheduled 'IO' action in the effect
  -> Effect context props model action
  -- ^ Effect whose IO actions are modified
  -> Effect context props model action
modifyAllIO f = censor $ \actions ->
  [ Schedule mb x (f <$> action)
  | Schedule mb x action <- actions
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
  -> Effect context props model action
withSink f = tell [ async f ]
-----------------------------------------------------------------------------
-- | Mutate the app-global React-style @context@ from within 'update'.
--
-- The supplied function is scheduled as a 'ContextModify' and folded over the
-- current global context during the scheduler's commit phase. If the context
-- value changes (per its 'Eq' instance), every 'Component' with @useContext@
-- enabled is re-rendered.
--
-- Note that @context@ is __write-only__ inside 'update'; to read it, use the
-- @context@ argument threaded into the 'Miso.Types.view' function.
--
-- @
-- 'update' Toggle = 'modifyContext' (\\theme -> if theme == Light then Dark else Light)
-- @
--
-- @since 1.13.0.0
modifyContext
  :: (context -> context)
  -- ^ Transformation to apply to the global @context@
  -> Effect context props model action
modifyContext f = tell [ ContextModify f ]
-----------------------------------------------------------------------------
-- | Replace the app-global React-style @context@ with a new value.
--
-- A convenience wrapper around 'modifyContext'. See 'modifyContext' for details
-- of when re-renders are triggered.
--
-- @since 1.13.0.0
putContext
  :: context
  -- ^ New global @context@ value
  -> Effect context props model action
putContext = modifyContext . const
-----------------------------------------------------------------------------
-- | Mutate the app-global React-style @context@ using a 'State' computation.
--
-- A convenience wrapper around 'modifyContext' that runs the supplied
-- @'State' context@ action over the current global context (via 'execState'),
-- scheduling the resulting @context -> context@ transformation as a
-- 'ContextModify'. This lets you use @put@ \/ @modify@ and the lens operators
-- from "Miso.Lens" to update @context@, mirroring how @model@ is updated.
--
-- @
-- 'update' Toggle = 'modifyContext_' $ theme '.=' Dark
-- @
--
-- @since 1.13.0.0
modifyContext_
  :: State context ()
  -- ^ 'State' computation describing the @context@ mutation
  -> Effect context props model action
modifyContext_ = modifyContext . execState
-----------------------------------------------------------------------------
-- | Issue a new @action@ to be processed by 'Miso.Types.update'.
--
-- @
-- data Action = HelloWorld
-- type Model  = Int
--
-- 'update' :: Action -> 'Effect' context props Model Action
-- 'update' = \\case
--   Click -> 'issue' HelloWorld
-- @
--
-- @since 1.9.0.0
issue
  :: action
  -- ^ @action@ to raise
  -> Effect context props model action
issue action = tell [ async $ \f -> f action ]
-----------------------------------------------------------------------------
-- | Helper for t'Miso.Types.Component' construction, when you want to ignore the 'Miso.Types.update'
-- function temporarily, or permanently.
--
-- @since 1.9.0.0
noop :: action -> Effect context props model action
noop = const (pure ())
-----------------------------------------------------------------------------
-- | Type to indicate if effects should be handled asynchronously
-- or synchronously.
--
data Synchronicity
  = Async
  | Sync
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Type to indicate where the 'Effect' should execute.
--
data Thread
  = MTS
  | BTS
  deriving (Show, Eq)
-----------------------------------------------------------------------------
