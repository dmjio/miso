-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
#ifdef PRODUCTION
#define MISO_JS_PATH "js/miso.prod.js"
#else
#define MISO_JS_PATH "js/miso.js"
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Runtime
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Runtime
  ( -- * Internal functions
    initialize
  , freshComponentId
  , buildVTree
  , renderStyles
  , renderScripts
  , Hydrate(..)
  -- * Subscription
  , startSub
  , stopSub
  -- * Pub / Sub
  , subscribe
  , unsubscribe
  , publish
  , Topic (..)
  , topic
  -- * Component
  , ComponentState (..)
  -- ** Communication
  , mail
  , checkMail
  , broadcast
  , mailParent
  , mailChildren
  , mailAncestors
  , mailDescendants
  -- ** WebSocket
  , websocketConnect
  , websocketConnectJSON
  , websocketConnectText
  , websocketConnectArrayBuffer
  , websocketConnectBLOB
  , websocketSend
  , websocketClose
  , socketState
  , emptyWebSocket
  , WebSocket (..)
  , URL
  , SocketState (..)
  , CloseCode (..)
  , Closed (..)
  -- ** EventSource
  , eventSourceConnectText
  , eventSourceConnectJSON
  , eventSourceClose
  , emptyEventSource
  , EventSource (..)
  -- ** Payload
  , Payload (..)
  , json
  , blob
  , arrayBuffer
  -- ** Internal Component state
  , components
  , globalContext
  , setContext
  , schedulerThread
  , componentIds
  , rootComponentId
  , componentId
  , modifyComponent
  , componentModel
  -- ** Scheduler
  , scheduler
#ifdef WASM
  , evalFile
#endif
  , topLevelComponentId
  , initComponent
  , withJS
  -- * Lynx cross-thread
  , MTS (..)
  , BTS (..)
  , getMTSContext
  , getBTSContext
  , dispatchEvent
  , mts
  , bts
  , web
  -- ** Protocol types
  , ComponentType (..)
  , COMPONENT (..)
  , EFFECT (..)
  ) where
-----------------------------------------------------------------------------
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import           Data.Proxy (Proxy(Proxy))
import           Control.Category ((.))
import           Control.Concurrent
import           Control.Exception (SomeException, catch, evaluate)
import           Control.Monad (forM, forM_, when, void, (<=<), zipWithM_, forever, foldM, unless)
import           Control.Monad.Reader (ask, asks)
import           Control.Monad.State hiding (state)
import           Miso.JSON (FromJSON, ToJSON, Result(..), Value, encode, fromJSON, jsonStringify, toJSON)

#if !MIN_VERSION_base(4,20,0)
import           Data.List (foldl')
#endif
import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, atomicWriteIORef)
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import           GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), threadStatus)
import           Data.Word (Word64)
import           GHC.Fingerprint (Fingerprint(..))
import           Numeric (readHex)
import           GHC.StaticPtr (StaticKey, staticKey, deRefStaticPtr)
#ifdef NATIVE
import           GHC.StaticPtr (unsafeLookupStaticPtr)
#endif
import           Prelude hiding ((.))
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
import           System.Mem (performMajorGC)
#ifdef BENCH
import           Text.Printf
#endif
-----------------------------------------------------------------------------
import           Miso.Concurrent (Waiter(..), waiter)
#ifdef NATIVE
import           Miso.Concurrent (oneshot)
#endif
import           Miso.CSS (renderStyleSheet)
import           Miso.Delegate (delegator)
import qualified Miso.Diff as Diff
import           Miso.DSL
#ifdef WASM
import           Miso.DSL.TH.File (evalFile)
#endif
import           Miso.Effect
  ( ComponentInfo(..), Sub, Sink, Effect, Schedule(..), runEffect
  , io_, withSink, Synchronicity(..)
  )
import qualified Miso.Effect as E (Thread(..))
import qualified Miso.FFI.Internal as FFI
import           Miso.FFI.Internal (Blob(..), ArrayBuffer(..))
import qualified Miso.Hydrate as Hydrate
import           Miso.Lens hiding (view)
import           Miso.String (ToMisoString(..), FromMisoString(..))
import           Miso.Types
import           Miso.Util
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of t'Miso.Types.Component' between top-level API functions.
initialize
#ifdef NATIVE
  :: (Eq context, Eq model, Eq props, ToJSON model, ToJSON action, FromJSON action)
#else
  :: (Eq context, Eq model, Eq props)
#endif
  => Events
  -> ComponentId
  -> Hydrate
  -> Bool
  -- ^ Is the root node being rendered?
  -> props
  -- ^ Initial props for this component
  -> Maybe Key
  -- ^ Optional key for stable hot-reload model recovery
  -> StaticKey
  -- ^ 'StaticPtr' key for cross-thread (Lynx) child component lifecycle
  -> Component context props model action
  -> IO DOMRef
  -- ^ Callback function is used for obtaining the t'Miso.Types.Component' 'DOMRef'.
  -> IO (ComponentState context props model action)
initialize events _componentParentId hydrate isRoot initialProps maybeKey _componentStaticKey comp@Component {..} getComponentMountPoint = do
  _componentId <- freshComponentId
  let
    _componentProps = initialProps
    _componentSink = \action -> do
      atomicModifyIORef' globalQueue (\q -> (enqueue _componentId action q, ()))
      notify globalWaiter

  initializedModel <-
    case (hydrate, hydrateModel) of
      (Hydrate, Just m) -> m
      (Draw, _) -> do
        vcomps <- readIORef components
        case maybeKey of
          Just k  -> pure $ fromMaybe model $ listToMaybe
            [ cs ^. componentModel
            | cs <- IM.elems vcomps
            , cs ^. componentKey == Just k
            ]
          Nothing -> pure model
      _ -> pure model
  _componentScripts <-
    if web
    then
      IM.lookup _componentId <$> readIORef components >>= \case
        Nothing -> (++) <$> renderScripts scripts <*> renderStyles styles
        Just cs -> pure (_componentScripts cs) -- hot reload scenario, reuse already mounted scripts
    else
      pure []

  _componentDOMRef <- getComponentMountPoint
  _componentVTree <- newIORef (VTree (Object jsNull))
  _componentSubThreads <- newIORef M.empty

  frame <- newEmptyMVar :: IO (MVar Double)
  let _componentMailbox = S.empty

  rAFCallback <-
    asyncCallback1 $ \jsval -> do
      putMVar frame =<< fromJSValUnchecked jsval

  let _componentDraw = \newModel -> do
        currentProps <- (^. componentProps) . (IM.! _componentId) <$> readIORef components
        currentContext <- readIORef globalContext
        newVTree <-
          buildVTree events _componentParentId _componentId Draw
            _componentSink logLevel (view currentContext currentProps newModel)
        oldVTree <- readIORef _componentVTree
        _frame <- requestAnimationFrame rAFCallback
        _timestamp :: Double <- takeMVar frame
        Diff.diff (Just oldVTree) (Just newVTree) _componentDOMRef
        FFI.updateRef oldVTree newVTree
        atomicWriteIORef _componentVTree newVTree
        FFI.flush

#ifdef NATIVE
  let _componentHydrate = \newModel -> do
        when bts $ postComponent HYDRATE _componentStaticKey _componentId _componentParentId
          (Just (toJSON newModel))

  let _componentPostEffect = \action ->
        postEffect _componentStaticKey _componentId (toJSON action)
#else
  let _componentHydrate = \_ -> pure ()
  let _componentPostEffect = \_ -> pure ()
#endif

  let _componentApplyActions = \(actions :: Seq action) model_ currentProps ctx -> do
        let info = ComponentInfo _componentId _componentParentId _componentDOMRef currentProps ctx
        foldl' (\(m, ss) a ->
          case runEffect (update a) info m of
            (n, sss) -> (n, ss <> [(a, s) | s <- sss ]))
          (model_, []) actions

  let vcomponent = ComponentState
        { _componentEvents = events
        , _componentKey = maybeKey
        , _componentMailbox = mailbox
        , _componentUseContext = useContext
        , _componentTopics = mempty
        , _componentModelDirty = modelCheck
        , _componentChildren = mempty
        , _componentModel = initializedModel
        , _prevComponentProps = _componentProps
        , _componentPropsPhase = \oldProps newProps ->
            case onPropsChanged of
              Just f -> _componentSink (f oldProps newProps)
              _ -> pure ()
        , ..
        }

  when isRoot (delegator _componentDOMRef _componentVTree events (logLevel `elem` [DebugEvents, DebugAll]))
  registerComponent vcomponent
  initSubs subs _componentSubThreads _componentSink
  initialDraw initializedModel events hydrate isRoot comp vcomponent
  forM_ mount _componentSink
#ifdef NATIVE
  when (bts && not isRoot) $
    postComponent MOUNT _componentStaticKey _componentId _componentParentId Nothing
#endif
  pure vcomponent
-----------------------------------------------------------------------------
initSubs :: [Sub action] -> IORef (Map MisoString ThreadId) -> Sink action -> IO ()
initSubs subs_ _componentSubThreads _componentSink = do
  forM_ subs_ $ \sub_ -> do
    threadId <- forkIO (sub_ _componentSink)
    subKey <- freshSubId
    atomicModifyIORef' _componentSubThreads $ \m ->
      (M.insert subKey threadId m, ())
-----------------------------------------------------------------------------
-- | Diffs two models, returning True if a redraw is necessary
modelCheck :: Eq model => model -> model -> Bool
modelCheck c n = unsafePerformIO $ do
  currentName <- c `seq` makeStableName c
  updatedName <- n `seq` makeStableName n
  pure (currentName /= updatedName && c /= n)
-----------------------------------------------------------------------------
-- | Checks if the Component is mounted before executing actions
isMounted :: ComponentId -> IO Bool
isMounted vcompId = isJust . IM.lookup vcompId <$> readIORef components
-----------------------------------------------------------------------------
-- | The scheduler processes all events in the system and is responsible
-- for propagating changes across model states both asynchronously
-- and synchronously. It also is responsible for
-- top-down rendering of the UI Component tree.
scheduler :: forall context . Eq context => Proxy context -> IO ()
scheduler Proxy =
  forever $ do
#ifdef NATIVE
    when mts (wait btsReady)
#endif
    getBatch >>= \case
      Nothing -> wait globalWaiter
      Just (vcompId, S.Empty)
        | vcompId == minBound -> do
            -- context propagation, 'minBound' sentinel indicates a global
            -- context change: re-render every 'Component' with 'useContext' set.
            -- 'minBound' is the one 'Int' that can be neither a real (positive)
            -- 'ComponentId' nor a negated one, so it never collides.
            vcomps <- readIORef components
            forM_ (IM.elems vcomps) $ \ComponentState {..} ->
              when _componentUseContext (_componentDraw _componentModel)
        | vcompId < 0 -> do
            -- props propagation, negated 'ComponentId' indicates render-phase only.
            vcomps <- readIORef components
            forM_ (IM.lookup (negate vcompId) vcomps) $ \ComponentState {..} -> do
              _componentDraw _componentModel
              _componentPropsPhase _prevComponentProps _componentProps

      Just (vcompId, actions) -> do
        mounted <- isMounted vcompId
        when mounted (run vcompId actions)
  where
    -----------------------------------------------------------------------------
    -- | Execute the commit phase against the model, perform top-down render
    -- of the entire Component tree.
    run :: ComponentId -> Seq action -> IO ()
    run vcompId = mapM_ renderComponent <=< commit vcompId
    -----------------------------------------------------------------------------
    -- | Apply the actions across the model, evaluate async and sync IO.
    commit :: ComponentId -> Seq action -> IO (Maybe ComponentId)
    commit vcompId events = do
      currentContext <- readIORef @context globalContext
      vcomps <- readIORef components
      let ComponentState {..} = vcomps IM.! vcompId
          (updatedModel, schedules) =
            _componentApplyActions events _componentModel _componentProps currentContext
      -- Route each scheduled effect. When the effect is tagged for the
      -- opposite Lynx thread, do not evaluate its 'IO' here; forward the
      -- originating @action@ via 'postEffect' so 'update' re-runs on the other
      -- thread, where the same effect is now same-thread and executes its 'IO'
      -- locally. Otherwise run it locally against the component's own sink.
      let
        crossThread = \case
            Just E.BTS -> mts   -- on MTS, effect wants BTS
            Just E.MTS -> bts   -- on BTS, effect wants MTS
            _          -> False -- same thread / no preference / web
      forM_ schedules $ \(originating, sched) -> case sched of
        ContextModify f ->
          atomicModifyIORef' globalContext $ \ctx -> (f ctx, ())
        Schedule mThread synch effect ->
          if crossThread mThread
            then _componentPostEffect originating
            else evalScheduled synch (effect _componentSink)
      updatedContext <- readIORef globalContext
      when (currentContext /= updatedContext) enqueueContextPropagation
      if _componentModelDirty _componentModel updatedModel
        then do
          modifyComponent _componentId (componentModel .= updatedModel)
          pure (Just vcompId)
        else
          pure Nothing
-----------------------------------------------------------------------------
-- | Perform a top-down rendering of the 'Component' tree.
--
-- We lookup the components each time to account for unmounting.
--
renderComponent :: ComponentId -> IO ()
renderComponent vcompId = IM.lookup vcompId <$> readIORef components >>= mapM_ \ComponentState {..} -> do
  _componentDraw _componentModel
  _componentHydrate _componentModel
-----------------------------------------------------------------------------
-- | Modify a single t'Component p m a' at a t'ComponentId'.
--
-- Auxiliary function
modifyComponent
  :: ComponentId
  -> State (ComponentState context props model action) a
  -> IO ()
modifyComponent vcompId go =
  atomicModifyIORef' components $ \vcomps ->
    (IM.adjust (execState go) vcompId vcomps, ())
-----------------------------------------------------------------------------
type ComponentIds = IntSet
-----------------------------------------------------------------------------
initialDraw
  :: (Eq m, Eq props, Eq context)
  => m
  -> Events
  -> Hydrate
  -> Bool
  -> Component context props m a
  -> ComponentState context props m a
  -> IO ()
initialDraw initializedModel events hydrate isRoot Component {..} ComponentState {..} = do
#ifdef BENCH
  start <- FFI.now
#endif
  currentContext <- readIORef globalContext
  vtree <- buildVTree events _componentParentId _componentId hydrate _componentSink logLevel
    (view currentContext _componentProps initializedModel)
#ifdef BENCH
  end <- FFI.now
  when isRoot $ FFI.consoleLog $ ms (printf "buildVTree: %.3f ms" (end - start) :: String)
#endif
  case hydrate of
    Draw -> do
      Diff.diff Nothing (Just vtree) _componentDOMRef
      atomicWriteIORef _componentVTree vtree
    Hydrate -> do
      if isRoot
        then do
          hydrated <- Hydrate.hydrate logLevel _componentDOMRef vtree
          if hydrated
            then do
              atomicWriteIORef _componentVTree vtree
            else do
              newTree <-
                buildVTree events _componentParentId _componentId Draw
                  _componentSink logLevel (view currentContext _componentProps initializedModel)
              Diff.diff Nothing (Just newTree) _componentDOMRef
              atomicWriteIORef _componentVTree newTree
        else
          atomicWriteIORef _componentVTree vtree
-----------------------------------------------------------------------------
-- | Pulls the next Component for processing out of the queue, along with
-- its events.
getBatch :: IO (Maybe (ComponentId, Seq action))
getBatch = do
  atomicModifyIORef' globalQueue $ \q ->
    case dequeue q of
      Nothing -> (q, Nothing)
      Just (vcompId, actions, newQueue) ->
        (newQueue, Just (vcompId, actions))
-----------------------------------------------------------------------------
-- | Helper for event extraction at a specific 'ComponentId'
drainQueueAt :: ComponentId -> IO (Seq a)
drainQueueAt vcompId = atomicModifyIORef' globalQueue (dequeueAt vcompId)
-----------------------------------------------------------------------------
-- | Data type for holding the events in the system along with
-- the schedule of what events should be processed next
data Queue action
  = Queue
  { _queue :: IntMap (Seq action)
  , _queueSchedule :: Seq ComponentId
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
emptyQueue :: Queue action
emptyQueue = mempty
-----------------------------------------------------------------------------
instance Semigroup (Queue action) where
  Queue q1 s1 <> Queue q2 s2 = Queue (q1 <> q2) (s1 <> s2)
-----------------------------------------------------------------------------
instance Monoid (Queue action) where
  mempty = Queue mempty mempty
-----------------------------------------------------------------------------
queue :: Lens (Queue action) (IntMap (Seq action))
queue = lens _queue $ \r f -> r { _queue = f }
-----------------------------------------------------------------------------
queueSchedule :: Lens (Queue action) (Seq ComponentId)
queueSchedule = lens _queueSchedule $ \r f -> r { _queueSchedule = f }
-----------------------------------------------------------------------------
enqueue :: ComponentId -> action -> Queue action -> Queue action
enqueue vcompId action q =
  q & queue %~ IM.insertWith (flip (<>)) vcompId (S.singleton action)
    & queueSchedule %~ (S.|> vcompId)
-----------------------------------------------------------------------------
-- | Used to fast track to render phase, bypassing commit phase. Used in 'props'
-- feature.
enqueueSchedule :: ComponentId -> IO ()
enqueueSchedule vcompId =
  atomicModifyIORef' globalQueue $ \q ->
     (q & queueSchedule %~ (S.|> negate vcompId), ())
-----------------------------------------------------------------------------
-- | Enqueues the context-propagation sentinel (@'minBound' :: 'Int'@). When the
-- scheduler dequeues it, every t'Miso.Types.Component' with @useContext@ enabled
-- is re-rendered against the updated global context. Used by the @context@
-- feature (see 'Miso.Effect.modifyContext').
enqueueContextPropagation :: IO ()
enqueueContextPropagation =
  atomicModifyIORef' globalQueue $ \q ->
     (q & queueSchedule %~ (S.|> minBound), ())
-----------------------------------------------------------------------------
-- | Case on queue schedule, get first item, span on the rest of queueSchedule, get length.
-- set schedule with whatever remains.
--
-- Take the length of the queue schedule found, looking up with vcompId (from first element)
-- in the queue, splitAt the queue.
--
dequeue
  :: forall action
   . Queue action
  -> Maybe (ComponentId, Seq action, Queue action)
dequeue q =
  case q ^. queueSchedule of
    S.Empty -> Nothing
    sched@(vcompId S.:<| _) ->
      case q ^. queue . at vcompId of
        Nothing ->
          let (_, remaining) = S.spanl (== vcompId) sched
          in Just (vcompId, S.empty, q & queueSchedule .~ remaining)
        Just actions ->
          case S.spanl (==vcompId) sched of
            (scheduled, remaining) ->
              case S.splitAt (length scheduled) actions of
                (process, rest) -> do
                  let updated =
                        q & queueSchedule .~ remaining
                          & queue.at vcompId .~ do if null rest then Nothing else Just rest
                  Just (vcompId, process, updated)
-----------------------------------------------------------------------------
-- | Dequeues everything from the Queue at a specific t'ComponentId', draining
-- both the queue events and the queue schedule.
dequeueAt
  :: forall action
   . ComponentId
  -> Queue action
  -> (Queue action, Seq action)
dequeueAt vcompId q =
  case q ^. queue . at vcompId of
    Nothing -> (q, S.empty)
    Just actions -> do
      -- dmj: remove from schedule, extract all events
      let updated = q & queueSchedule %~ S.filter (/=vcompId)
                      & queue.at vcompId .~ Nothing
      (updated, actions)
-----------------------------------------------------------------------------
globalWaiter :: Waiter
{-# NOINLINE globalWaiter #-}
globalWaiter = unsafePerformIO waiter
-----------------------------------------------------------------------------
#ifdef NATIVE
btsReady :: Waiter
{-# NOINLINE btsReady #-}
btsReady = unsafePerformIO oneshot
#endif
-----------------------------------------------------------------------------
globalQueue :: IORef (Queue action)
{-# NOINLINE globalQueue #-}
globalQueue = unsafePerformIO (newIORef emptyQueue)
-----------------------------------------------------------------------------
-- | The global React-style @context@. Seeded in 'initComponent' (via
-- 'Miso.startAppWithContext', defaulting to @()@) and mutated by
-- 'Miso.Effect.modifyContext' during the scheduler's commit phase.
--
-- N.B. like 'components', this holds a single value whose type is fixed for the
-- lifetime of the application; it is written before any draw occurs.
globalContext :: IORef context
{-# NOINLINE globalContext #-}
globalContext = unsafePerformIO (newIORef undefined)
-----------------------------------------------------------------------------
-- | Seed the global @context@ 'IORef' with a value.
--
-- 'Miso.startAppWithContext' seeds this before the first draw, so client
-- applications never call it. It exists for __server-side rendering__, where a
-- t'Miso.Types.View' is serialized to HTML without ever starting the runtime
-- and the 'globalContext' cell would otherwise still hold @undefined@. See
-- 'Miso.setContext' for the full explanation.
--
-- @since 1.12.0.0
setContext :: Eq context => context -> IO ()
setContext = atomicWriteIORef globalContext
-----------------------------------------------------------------------------
componentId :: Lens (ComponentState context props model action) ComponentId
componentId = lens _componentId $ \record field -> record { _componentId = field }
-----------------------------------------------------------------------------
componentKey :: Lens (ComponentState context props model action) (Maybe Key)
componentKey = lens _componentKey $ \record field -> record { _componentKey = field }
-----------------------------------------------------------------------------
children :: Lens (ComponentState context props model action) ComponentIds
children = lens _componentChildren $ \record field -> record { _componentChildren = field }
-----------------------------------------------------------------------------
componentTopics :: Lens (ComponentState context props model action) (Map MisoString (Value -> IO ()))
componentTopics = lens _componentTopics $ \record field -> record { _componentTopics = field }
-----------------------------------------------------------------------------
componentModel :: Lens (ComponentState context props model action) model
componentModel = lens _componentModel $ \record field -> record { _componentModel = field }
-----------------------------------------------------------------------------
componentProps :: Lens (ComponentState context props model action) props
componentProps = lens _componentProps $ \record field -> record { _componentProps = field }
-----------------------------------------------------------------------------
prevComponentProps :: Lens (ComponentState context props model action) props
prevComponentProps = lens _prevComponentProps $ \record field -> record { _prevComponentProps = field }
-----------------------------------------------------------------------------
-- | Hydrate avoids calling @diff@, and instead calls @hydrate@
-- 'Draw' invokes 'Miso.Diff.diff'
data Hydrate
  = Draw
  | Hydrate
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' state, data associated with the lifetime of a t'Miso.Types.Component'
data ComponentState context props model action
  = ComponentState
  { _componentId :: ComponentId
  -- ^ The ID of the current t'Miso.Types.Component'
  , _componentKey :: Maybe Key
  -- ^ Optional key for stable hot-reload model recovery
  , _componentStaticKey :: StaticKey
  -- ^ 'StaticPtr' key of the originating t'VComp', used to instruct the MTS
  -- to mount, hydrate, or unmount this child across the Lynx thread boundary.
  -- 'Nothing' for the root (each thread mounts the root locally).
  , _componentParentId :: ComponentId
  -- ^ The ID of the t'Miso.Types.Component''s parent
  , _componentProps :: props
  -- ^ The current props passed to this t'Miso.Types.Component'
  , _prevComponentProps :: props
  -- ^ The previous Component props passed to this t'Miso.Types.Component'
  , _componentSubThreads :: IORef (Map MisoString ThreadId)
  -- ^ Mapping of all 'Sub' in use by t'Miso.Types.Component'
  , _componentDOMRef :: DOMRef
  -- ^ The DOM reference the t'Miso.Types.Component' is mounted on
  , _componentVTree :: IORef VTree
  -- ^ A reference to the current virtual DOM (i.e. t'VTree')
  , _componentSink :: action -> IO ()
  -- ^ t'Miso.Types.Component' t'Sink' used to enter events into the system
  , _componentPostEffect :: Sink action
  -- ^ Cross-thread (Lynx) t'Sink': serializes the @action@ and ships it to the
  -- opposite thread via 'postEffect'. Captures the t'Miso.Types.Component''s
  -- 'ToJSON' instance at 'initialize' time. Used for effects tagged
  -- 'Miso.Effect.runOnMain' \/ 'Miso.Effect.runOnBackground'.
  , _componentModel :: model
  -- ^ t'Miso.Types.Component' state
  , _componentScripts :: [DOMRef]
  -- ^ DOM references for \<script\> and \<style\> appended to \<head\>
  , _componentEvents :: Events
  -- ^ List of events a t'Miso.Types.Component' listens on
  , _componentUseContext :: Bool
  -- ^ Whether this t'Miso.Types.Component' re-renders when the global
  --   @context@ changes.
  , _componentMailbox :: Value -> Maybe action
  -- ^ Mailbox for asynchronous t'Miso.Types.Component' communication
  , _componentDraw :: model -> IO ()
  -- ^ Helper function for t'Miso.Types.Component' rendering
  , _componentHydrate :: model -> IO ()
  -- ^ Posts the model to the MTS for cross-thread (Lynx) hydration.
  -- Captures the t'Miso.Types.Component''s 'ToJSON' instance at 'initialize'
  -- time; a no-op unless running on the background thread ('bts').
  , _componentPropsPhase :: props -> props -> IO ()
  -- ^ Helper function for t'Miso.Types.Component' props changed phase.
  , _componentModelDirty :: model -> model -> Bool
  -- ^ Model diffing
  , _componentApplyActions
      :: Seq action
      -> model
      -> props
      -> context
      -> (model, [(action, Schedule context action)])
  -- ^ t'Miso.Types.Component' actions application. Given the pending actions,
  --   current @model@ and @props@, returns the updated @model@ and the
  --   'Schedule's to run (async \/ sync IO, cross-thread effects, and
  --   'ContextModify's). Each 'Schedule' is paired with the @action@ whose
  --   'update' produced it, so cross-thread effects can be forwarded (via
  --   'postEffect') to run on the opposite Lynx thread.
  , _componentTopics :: Map MisoString (Value -> IO ())
  -- ^ t'Miso.Types.Component' topics using for Pub Sub async communication.
  , _componentChildren :: ComponentIds
  -- ^ 'IntSet' of children t'Miso.Types.ComponentId'
  }
-----------------------------------------------------------------------------
-- | A @Topic@ represents a place to send and receive messages. @Topic@ is used to facilitate
-- communication between t'Miso.Types.Component'. t'Miso.Types.Component' can 'subscribe' to or 'publish' to any @Topic@,
-- within the same t'Miso.Types.Component' or across t'Miso.Types.Component'.
--
-- This requires creating a custom 'ToJSON' / 'FromJSON'. Any other t'Miso.Types.Component'
-- can 'publish' or 'subscribe' to this @Topic message@. It is a way to provide
-- loosely-coupled communication between @Components@.
--
-- See 'publish', 'subscribe', 'unsubscribe' for more details.
--
-- When distributing t'Miso.Types.Component' for third-party use, it is recommended to export
-- the @Topic@, where message is the JSON protocol.
--
--
-- @since 1.9.0.0
newtype Topic a = Topic MisoString
  deriving stock (Ord, Eq, Show)
-----------------------------------------------------------------------------
instance ToMisoString (Topic a) where
  toMisoString (Topic x) = x
-----------------------------------------------------------------------------
-- | Smart constructor for creating a @Topic message@ to write to
--
-- @
--
-- data Message
--   = Increment
--   | Decrement
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)
--
-- arithmetic :: Topic Message
-- arithmetic = topic "arithmetic"
--
-- data Action
--   = Notification (Result Message)
--   | Subscribe
--   | Unsubscribe
--
-- update_ :: Action -> Effect context props Int Action
-- update_ = \case
--   Unsubscribe ->
--     unsubscribe arithmetic
--   Subscribe ->
--     subscribe arithmetic Notification
--   Notification (Success Increment) ->
--     update_ AddOne
--   Notification (Success Decrement) ->
--     update_ SubtractOne
--   Notification (Error msg) ->
--     io_ $ consoleError ("Decode failure: " <> ms msg)
--
-- @
--
-- @since 1.9.0.0
topic :: MisoString -> Topic a
topic = Topic
-----------------------------------------------------------------------------
-- | Subscribes a t'Miso.Types.Component' to a t'Topic'.
--
-- Registers a callback in the component that decodes incoming messages
-- using its own 'FromJSON' instance and dispatches them to the component's
-- 'Sink'. If the component is already subscribed to the named topic the
-- previous callback is replaced.
--
-- Because each subscriber uses its own 'FromJSON', components can use
-- different Haskell types for the same topic as long as the underlying
-- JSON is compatible, enabling loose coupling between t'Miso.Types.Component'.
--
-- @
--
-- data Message = Increment | Decrement
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)
--
-- arithmetic :: Topic Message
-- arithmetic = topic "arithmetic"
--
-- data Action
--   = Notify Message
--   | NotifyError MisoString
--   | Subscribe
--   | Unsubscribe
--   | AddOne
--   | SubtractOne
--
-- update_ :: Action -> Effect context props Int Action
-- update_ = \\case
--   Subscribe ->
--     subscribe arithmetic Notify NotifyError
--   Unsubscribe ->
--     unsubscribe arithmetic
--   Notify Increment -> update_ AddOne
--   Notify Decrement -> update_ SubtractOne
--   NotifyError msg ->
--     io_ $ consoleError ("Decode failure: " <> msg)
--   AddOne -> _count += 1
--   SubtractOne -> _count -= 1
--
-- @
--
-- @since 1.9.0.0
subscribe
  :: FromJSON message
  => Topic message
  -> (message -> action)
  -> (MisoString -> action)
  -> Effect context props model action
subscribe (Topic topicName) successful errorful = do
  ComponentInfo {..} <- ask
  withSink $ \sink ->
    modifyComponent _componentInfoId $ do
      componentTopics %= do
        M.insert topicName $ \value ->
          sink (case fromJSON value of
                  Success s -> successful s
                  Error e -> errorful e)
-----------------------------------------------------------------------------
-- | Unsubscribes a t'Miso.Types.Component' from a t'Topic'.
--
-- Removes the callback registered by 'subscribe' so the component no longer
-- receives messages published to the topic. If the component is not
-- currently subscribed this is a no-op.
--
-- See 'subscribe' for example usage.
--
-- @since 1.9.0.0
unsubscribe :: Topic message -> Effect context props model action
unsubscribe (Topic topicName) = do
  ComponentInfo {..} <- ask
  io_ $ modifyComponent _componentInfoId $ do
    componentTopics %= M.delete topicName
-----------------------------------------------------------------------------
-- | Publish to a t'Topic message'
--
-- t'Topic message' are generated dynamically if they do not exist. When using 'publish'
-- all subscribers are immediately notified of a new message. A message is distributed as a 'Value'
-- The underlying 'ToJSON' instance is used to construct this 'Value'.
--
-- We recommend documenting a public API for the JSON protocol message when distributing a t'Miso.Types.Component'
-- downstream to end users for consumption (be it inside a single cabal project or across multiple
-- cabal projects).
--
-- @
--
-- arithmetic :: Topic Message
-- arithmetic = topic "arithmetic"
--
-- server :: Component context props () Action
-- server = component () update_ $ \() ->
--   div_
--   []
--   [ "Server component"
--   , button_ [ onClick AddOne ] [ "+" ]
--   , button_ [ onClick SubtractOne ] [ "-" ]
--   , component_ (client_ "client 1")
--   , component_ (client_ "client 2")
--   ] where
--       update_ :: Action -> Effect context props () Action
--       update_ = \case
--         AddOne ->
--           publish arithmetic Increment
--         SubtractOne ->
--           publish arithemtic Decrement
--
-- @
--
-- @since 1.9.0.0
publish
  :: ToJSON message
  => Topic message
  -> message
  -> IO ()
publish (Topic topicName) message = mapM_ go . IM.elems =<< readIORef components
  where
    go ComponentState {..} =
      case M.lookup topicName _componentTopics of
        Nothing ->
          pure ()
        Just f ->
          f (toJSON message)
-----------------------------------------------------------------------------
subIds :: IORef Int
{-# NOINLINE subIds #-}
subIds = unsafePerformIO $ newIORef 0
-----------------------------------------------------------------------------
freshSubId :: IO MisoString
freshSubId = do
  x <- atomicModifyIORef' subIds $ \y -> (y + 1, y)
  pure ("miso-sub-id-" <> ms x)
-----------------------------------------------------------------------------
-- | This is used to demarcate the ROOT of a page. This ID will *never*
-- exist in the `components` map.
rootComponentId :: ComponentId
rootComponentId = 0
-----------------------------------------------------------------------------
-- | This is the top-level ComponentId, hardcoded
topLevelComponentId :: ComponentId
topLevelComponentId = 1
-----------------------------------------------------------------------------
-- | The global store of 'ComponentId', for internal-use only.
--
-- Used internally @freshComponentId@ to allocate new 'ComponentId' on
-- mount.
--
componentIds :: IORef Int
{-# NOINLINE componentIds #-}
componentIds = unsafePerformIO $ newIORef topLevelComponentId
-----------------------------------------------------------------------------
freshComponentId :: IO ComponentId
freshComponentId = atomicModifyIORef' componentIds $ \y -> (y + 1, y)
-----------------------------------------------------------------------------
-- | 'cleanup' is used to remove previous application state (when using miso w/ GHCi).
--
-- As seen in <https://try.haskell-miso.org>
--
-- * Detect if previous 'Component' tree is present.
-- * Unmount in descending order (top-level 'Component' removed last), invoking finalizers
-- * Kill the scheduler thread (a new one is created on ':r').
-- * Erase all 'Component'
-- * Erase 'Queue'
-- * Reset 'componentId'
-- * Recreate 'DOMRef', GCs previous event listeners in JS.
-- * Yield to the scheduler (unwind thread stacks).
-- * Perform major garbage collection (cleans out old state).
--
-- This GC should remove the previous 'Notify' / 'MVar' as well since the 'sink'
-- closure should go out of scope.
--
cleanup :: forall context. Eq context => Proxy context -> Bool -> DOMRef -> IO ()
cleanup Proxy live domRef = do
  vcomps <- readIORef components
  when (IM.size vcomps > 0) $ do
    killThread =<< readIORef schedulerThread
    if live
      then do
        -- In hot reload we want to reset subs and connections, and free lifecycle hooks
        forM_ (IM.toDescList vcomps) $ \(_, cs@ComponentState{..}) -> do
          mapM_ killThread =<< readIORef _componentSubThreads
          finalizeWebSockets _componentId
          finalizeEventSources _componentId
          freeLifecycleHooks cs
      else do
        -- We can do a full unmount if we're not doing hot reload
        forM_ (IM.toDescList vcomps) $ \(_, vcomp_) ->
          unmountComponent @context vcomp_
    atomicWriteIORef componentIds topLevelComponentId
    atomicWriteIORef globalQueue mempty
    unless live (atomicWriteIORef components mempty)
    abort <- domRef ! "abort"
    isnull <- isNull abort
    unless isnull $ do
      void $ (domRef # "abort") ()
    yield
    performMajorGC
-----------------------------------------------------------------------------
-- | componentMap
--
-- This is a global t'Miso.Types.Component' @Map@ that holds the state of all currently
-- mounted t'Miso.Types.Component's
components :: IORef (IntMap (ComponentState context props model action))
{-# NOINLINE components #-}
components = unsafePerformIO (newIORef mempty)
-----------------------------------------------------------------------------
-- | This function evaluates effects according to 'Synchronicity'.
evalScheduled :: Synchronicity -> IO () -> IO ()
evalScheduled Sync x = x `catch` (void . exception)
evalScheduled Async x = void (forkIO (x `catch` (void . exception)))
-----------------------------------------------------------------------------
exception :: SomeException -> IO ()
exception ex = FFI.consoleError ("[EXCEPTION]: " <> ms ex)
-----------------------------------------------------------------------------
-- | Drains the event queue before unmounting, executed synchronously.
drain
  :: forall context props model action . Eq context
  => ComponentState context props model action
  -> IO ()
drain ComponentState {..} = do
  drainQueueAt _componentId >>= \case
    S.Empty -> pure ()
    actions -> do
       currentContext <- readIORef @context globalContext
       case _componentApplyActions actions _componentModel _componentProps currentContext of
         (_, schedules) -> do
           forM_ schedules $ \case
             -- dmj: process all actions synchronously during unmount
             (_, Schedule _ _ effect) -> -- todo: dmj cross thread effects ...
               effect _componentSink
                 `catch` (\(e :: SomeException) -> void (evaluate e))
             (_, ContextModify f) ->
               atomicModifyIORef' globalContext $ \ctx -> (f ctx, ())
           newContext <- readIORef globalContext
           when (currentContext /= newContext) enqueueContextPropagation
           -- dmj: One last context propagation before aborting.
           -- Don't recurse on drain, we only fire-off the last set
           -- of events for 'onBeforeUnmounted' hooks. The queue will
           -- ignore the rest of these.
-----------------------------------------------------------------------------
-- | Post unmount call to drop the <style> and <script> in <head>
unloadScripts :: ComponentState context props model action -> IO ()
unloadScripts ComponentState {..} = do
  head_ <- FFI.getHead
  forM_ _componentScripts $ \domRef -> do
    contains <- fromJSValUnchecked =<< do head_ # "contains" $ [domRef]
    when contains (FFI.removeChild head_ domRef)
-----------------------------------------------------------------------------
-- | Helper to drop all lifecycle and mounting hooks if defined.
freeLifecycleHooks :: ComponentState context props model action -> IO ()
freeLifecycleHooks ComponentState {..} = do
  VTree (Object comp) <- readIORef _componentVTree
  mapM_ freeFunction =<< fromJSVal =<< comp ! ("mount" :: MisoString)
  mapM_ freeFunction =<< fromJSVal =<< comp ! ("unmount" :: MisoString)
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a t'Miso.Types.Component'
unmountComponent
  :: Eq context
  => ComponentState context props model action
  -> IO ()
unmountComponent cs@ComponentState {..} = do
  mapM_ killThread =<< readIORef _componentSubThreads
  drain cs
  finalizeWebSockets _componentId
  finalizeEventSources _componentId
  unloadScripts cs
  freeLifecycleHooks cs
  modifyComponent _componentParentId $ do
    children.at _componentId .= Nothing
  atomicModifyIORef' components $ \m -> (IM.delete _componentId m, ())
#ifdef NATIVE
  when bts $ do
    postComponent UNMOUNT _componentStaticKey _componentId _componentParentId Nothing
#endif
-----------------------------------------------------------------------------
-- | Internal function for construction of a Virtual DOM.
--
-- Component mounting should be synchronous.
-- Mounting causes a recursive diffing to occur
-- (creating sub components as detected), setting up
-- infrastructure for each sub-component. During this
-- process we go between the Haskell heap and the JS heap.
buildVTree
  :: forall context action . Eq context
  => Events
  -> ComponentId
  -> ComponentId
  -> Hydrate
  -> Sink action
  -> LogLevel
  -> View context action
  -> IO VTree
buildVTree events_ parentId_ vcompId hydrate snk logLevel_ = \case
  VComp ptr -> case deRefStaticPtr ptr of
    SomeComponent maybeKey newProps app -> do
      vcomp_ <- create

      mountCallback <- do
        syncCallback1' $ \parent_ -> do
          ComponentState {..} <- initialize events_ vcompId hydrate False newProps maybeKey (staticKey ptr) app (pure parent_)
          modifyComponent vcompId (children %= IS.insert _componentId)
          vtree <- toJSVal =<< readIORef _componentVTree
          FFI.set "parent" vcomp_ (Object vtree)
          obj <- create
          setProp "componentId" _componentId obj
          setProp "componentTree" vtree obj
          toJSVal obj

      unmountCallback <- toJSVal =<< do
        FFI.syncCallback1 $ \vcompId_ -> do
          componentId_ <- fromJSValUnchecked vcompId_
          IM.lookup componentId_ <$> readIORef components >>= \case
            Nothing -> pure ()
            Just componentState -> do
              forM_ (unmount app) (_componentSink componentState)
              unmountComponent @context componentState

      -- When props are present, install a diffProps callback.
      -- Comparison happens in Haskell against _componentLastProps — no round-trip.
      -- TypeScript calls diffProps() unconditionally; Haskell decides whether to dispatch.
      diffPropsCallback <- toJSVal =<< do
        syncCallback $ do
          componentId_ <- fromJSValUnchecked =<< vcomp_ ! ("componentId" :: MisoString)
          currentProps <- _componentProps . (IM.! componentId_) <$> readIORef components
          when (currentProps /= newProps) $ do
            modifyComponent componentId_ $ do
              componentProps .= newProps
              prevComponentProps .= currentProps
            enqueueSchedule componentId_

      FFI.set "diffProps" diffPropsCallback vcomp_
      FFI.set "child" jsNull vcomp_
      forM_ maybeKey (\key -> FFI.set "key" key vcomp_)
      FFI.set "mount" mountCallback vcomp_
      FFI.set "unmount" unmountCallback vcomp_
      FFI.set "eventPropagation" (eventPropagation app) vcomp_
      FFI.set "type" VCompType vcomp_
      pure (VTree vcomp_)
  VNode ns tag attrs kids -> do
    vnode_ <- createNode "vnode" ns tag
    setAttrs vnode_ attrs snk logLevel_ events_
    vchildren <- toJSVal =<< procreate vnode_
    FFI.set "children" vchildren vnode_
    flip (FFI.set "type") vnode_ =<< toJSVal VNodeType
    pure (VTree vnode_)
      where
        procreate parentVTree = do
          kidsViews <- foldM (buildKid parentVTree) [] kids
          let ordered = reverse kidsViews
          setNextSibling ordered
          pure ordered
            where
              setNextSibling xs =
                zipWithM_ (flip setField "nextSibling")
                  xs (drop 1 xs)
              buildKid _ acc (VFrag _ []) = pure acc
              buildKid p acc kid = do
                VTree child <- buildVTree events_ parentId_ vcompId hydrate snk logLevel_ kid
                FFI.set "parent" p child
                pure (child : acc)
  VText key t -> do
    vtree <- create
    flip (FFI.set "type") vtree =<< toJSVal VTextType
    forM_ key $ \k -> FFI.set "key" (ms k) vtree
    FFI.set "ns" ("text" :: MisoString) vtree
    FFI.set "text" t vtree
    pure (VTree vtree)
  VFrag maybeKey kids -> do
    frag <- create
    FFI.set "type" VFragType frag
    forM_ maybeKey $ \(Key k) -> FFI.set "key" k frag
    vchildren <- toJSVal =<< procreateFragChildren frag
    FFI.set "children" vchildren frag
    pure (VTree frag)
      where
        procreateFragChildren parentVTree = do
          kidsViews <- foldM buildKid [] kids
          let ordered = reverse kidsViews
          zipWithM_ (flip setField "nextSibling") ordered (drop 1 ordered)
          pure ordered
            where
              buildKid acc (VFrag _ []) = pure acc
              buildKid acc kid = do
                VTree child <- buildVTree events_ parentId_ vcompId hydrate snk logLevel_ kid
                FFI.set "parent" parentVTree child
                pure (child : acc)
-----------------------------------------------------------------------------
-- | @createNode@
-- A helper function for constructing a vtree (used for @vcomp@ and @vnode@)
-- Doesn't handle children
createNode :: MisoString -> Namespace -> MisoString -> IO Object
createNode typ ns tag = do
  vnode_ <- create
  cssObj <- create
  propsObj <- create
  eventsObj <- create
  captures <- create
  bubbles <- create
  FFI.set "css" cssObj vnode_
  FFI.set "type" typ vnode_
  FFI.set "props" propsObj vnode_
  FFI.set "events" eventsObj vnode_
  FFI.set "captures" captures eventsObj
  FFI.set "bubbles" bubbles eventsObj
  FFI.set "ns" ns vnode_
  FFI.set "tag" tag vnode_
  pure vnode_
-----------------------------------------------------------------------------
-- | Helper function for populating "props" and "css" fields on a virtual
-- DOM node
setAttrs
  :: Object
  -> [Attribute action]
  -> Sink action
  -> LogLevel
  -> Events
  -> IO ()
setAttrs vnode_@(Object jval) attrs snk logLevel events =
  forM_ attrs $ \case
    Property "key" v -> do
      value <- toJSVal v
      FFI.set "key" value vnode_
    ClassList classes ->
      FFI.populateClass jval classes
    Property k v -> do
      value <- toJSVal v
      o <- getProp "props" vnode_
      FFI.set k value (Object o)
    On callback ->
      callback snk (VTree vnode_) logLevel events
    Styles styles -> do
      cssObj <- getProp "css" vnode_
      forM_ (M.toList styles) $ \(k,v) -> do
        FFI.set k v (Object cssObj)
-----------------------------------------------------------------------------
-- | Registers components in the global state
registerComponent :: MonadIO m => ComponentState context props model action -> m ()
registerComponent componentState = liftIO $
  atomicModifyIORef' components $ \vcomps' ->
    (IM.insert (_componentId componentState) componentState vcomps', ())
-----------------------------------------------------------------------------
-- | Renders styles
--
-- Meant for development purposes
-- Appends CSS to <head>
--
renderStyles :: [CSS] -> IO [DOMRef]
renderStyles styles =
  forM styles $ \case
    Href url cacheBust -> FFI.addStyleSheet url cacheBust
    Style css -> FFI.addStyle css
    Sheet sheet -> FFI.addStyle (renderStyleSheet sheet)
-----------------------------------------------------------------------------
-- | Renders scripts
--
-- Meant for development purposes
-- Appends JS to <head>
--
renderScripts :: [JS] -> IO [DOMRef]
renderScripts scripts =
  forM scripts $ \case
    Src src cacheBust ->
      FFI.addSrc src cacheBust
    Script script ->
      FFI.addScript False script
    Module src ->
      FFI.addScript True src
    ImportMap importMap -> do
      o <- create
      imports <- create
      forM_ importMap $ \(k,v) ->
        FFI.set k v imports
      FFI.set "imports" imports o
      FFI.addScriptImportMap
        =<< jsonStringify
        =<< toJSVal o
-----------------------------------------------------------------------------
-- | Starts a named 'Sub' dynamically, during the life of a t'Miso.Types.Component'.
-- The 'Sub' can be stopped by calling @Ord subKey => stop subKey@ from the 'update' function.
-- All 'Sub' started will be stopped if a t'Miso.Types.Component' is unmounted.
--
-- @
-- data SubType = LoggerSub | TimerSub
--   deriving (Eq, Ord)
--
-- update Action =
--   startSub LoggerSub $ \\sink -> forever (threadDelay (secs 1) >> consoleLog "test")
-- @
--
-- @since 1.9.0.0
startSub
  :: ToMisoString subKey
  => subKey
  -- ^ The key used to track the 'Sub'
  -> Sub action
  -- ^ The 'Sub'
  -> Effect context props model action
startSub subKey sub = do
  ComponentInfo {..} <- ask
  io_ $ do
    IM.lookup _componentInfoId <$> liftIO (readIORef components) >>= \case
      Nothing -> pure ()
      Just compState@ComponentState {..} -> do
        mtid <- liftIO (M.lookup (ms subKey) <$> readIORef _componentSubThreads)
        case mtid of
          Nothing ->
            startThread compState
          Just tid -> do
            status <- threadStatus tid
            case status of
              ThreadFinished -> startThread compState
              ThreadDied -> startThread compState
              _ -> pure ()
  where
    startThread ComponentState {..} = do
      tid <- forkIO (sub _componentSink)
      atomicModifyIORef' _componentSubThreads $ \m ->
        (M.insert (ms subKey) tid m, ())
-----------------------------------------------------------------------------
-- | Stops a named 'Sub' dynamically, during the life of a t'Miso.Types.Component'.
-- All 'Sub' started will be stopped automatically if a t'Miso.Types.Component' is unmounted.
--
-- @
-- data SubType = LoggerSub | TimerSub
--   deriving (Eq, Ord)
--
-- update Action = do
--   stopSub LoggerSub
-- @
--
-- @since 1.9.0.0
stopSub
  :: ToMisoString subKey
  => subKey
  -- ^ The key used to stop the 'Sub'
  -> Effect context props model action
stopSub subKey = do
  vcompId <- asks _componentInfoId
  io_ $ do
    IM.lookup vcompId <$> readIORef components >>= \case
      Nothing -> do
        pure ()
      Just ComponentState {..} -> do
        mtid <- liftIO (M.lookup (ms subKey) <$> readIORef _componentSubThreads)
        forM_ mtid $ \tid ->
          liftIO $ do
            atomicModifyIORef' _componentSubThreads $ \m -> (M.delete (ms subKey) m, ())
            killThread tid
-----------------------------------------------------------------------------
-- | Send any @ToJSON message => message@ to a t'Miso.Types.Component' mailbox, by 'ComponentId'
--
-- @
-- io_ $ mail componentId ("test message" :: MisoString) :: Effect context props model action
-- @
--
-- @since 1.9.0.0
mail
  :: ToJSON message
  => ComponentId
  -- ^ 'ComponentId' to receive 'mail'
  -> message
  -- ^ The message to send
  -> IO ()
mail vcompId msg =
  IM.lookup vcompId <$> readIORef components >>= \case
    Nothing -> pure ()
    Just ComponentState{..} ->
      case _componentMailbox (toJSON msg) of
        Nothing -> pure ()
        Just action ->
          _componentSink action
-----------------------------------------------------------------------------
-- | Send any @ToJSON message => message@ to the parent's t'Miso.Types.Component' mailbox
--
-- @
-- mailParent ("test message" :: MisoString) :: Effect context props model action
-- @
--
-- @since 1.9.0.0
mailParent
  :: ToJSON message
  => message
  -- ^ Message to send
  -> Effect context props model action
mailParent msg = do
  ComponentInfo {..} <- ask
  io_ (mail _componentInfoParentId msg)
-----------------------------------------------------------------------------
-- | Send any @ToJSON message => message@ to all ancestor t'Miso.Types.Component' 'mailbox'.
--
-- This function walks the t'Miso.Types.Component' ancestor hierarchy, delivering mail
-- along the way.
--
-- @
-- mailAncestors ("test message" :: MisoString) :: Effect context props model action
-- @
--
-- @since 1.11.0.0
mailAncestors
  :: ToJSON message
  => message
  -- ^ Message to send
  -> Effect context props model action
mailAncestors msg = do
  ComponentInfo {..} <- ask
  io_ (climb _componentInfoParentId)
    where
      climb vcompId = do
        mail vcompId msg
        IM.lookup vcompId <$> readIORef components >>= \case
          Nothing -> pure ()
          Just cs -> climb (_componentParentId cs)
-----------------------------------------------------------------------------
-- | Send any @ToJSON message => message@ to the children's t'Miso.Types.Component' mailbox
--
-- N.B. this is only relevant for immediate descendants (not all descendants).
--
-- @
-- mailChildren ("test message" :: MisoString) :: Effect context props model action
-- @
--
-- @since 1.9.0.0
mailChildren
  :: ToJSON message
  => message
  -- ^ Message to send
  -> Effect context props model action
mailChildren msg = do
  ComponentInfo {..} <- ask
  io_ $ do
    ComponentState {..} <- (IM.! _componentInfoId) <$> readIORef components
    forM_ (IS.toList _componentChildren) (flip mail msg)
-----------------------------------------------------------------------------
-- | Send any @ToJSON message => message@ to all descendants t'Miso.Types.Component' mailbox
--
-- Unlike 'mailChildren', this is relevant for all descendants 'Component'.
--
-- @
-- mailDescendants ("test message" :: MisoString) :: Effect context props model action
-- @
--
-- @since 1.12.0.0
mailDescendants
  :: ToJSON message
  => message
  -- ^ Message to send
  -> Effect context props model action
mailDescendants msg = do
  ComponentInfo {..} <- ask
  io_ $ do
    cs <- (IM.! _componentInfoId) <$> readIORef components
    forM_ (IS.toList (_componentChildren cs)) $ \child -> do
      walk . (IM.! child) =<< readIORef components
  where
    walk ComponentState {..} = do
      mail _componentId msg
      forM_ (IS.toList _componentChildren) $ \child -> do
        walk . (IM.! child) =<< readIORef components
----------------------------------------------------------------------------
-- | Helper function for processing @Mail@ from 'mail'.
--
-- @
--
-- data Action
--   = ParsedMail Message
--   | ErrorMail MisoString
--
-- main :: IO ()
-- main = app { mailbox = checkMail ParsedMail ErrorMail }
-- @
--
-- @since 1.9.0.0
checkMail
  :: FromJSON value
  => (value -> action)
  -- ^ Successful callback
  -> (MisoString -> action)
  -- ^ Errorful callback
  -> Value
  -- ^ The message received to parse.
  -> Maybe action
checkMail successful errorful value =
  pure $ case fromJSON value of
    Success x -> successful x
    Error err -> errorful (ms err)
-----------------------------------------------------------------------------
-- | Sends a message to all t'Miso.Types.Component' 'mailbox', excluding oneself.
--
-- @
--
-- update :: action -> Effect context props model action
-- update _ = broadcast (String "public service announcement")
-- @
--
-- @since 1.9.0.0
broadcast
  :: Eq model
  => ToJSON message
  => message
  -- ^ Message to broadcast to all other 'Component'
  -> Effect context props model action
broadcast msg = do
  ComponentInfo {..} <- ask
  io_ $ do
    vcompIds <- IM.keys <$> readIORef components
    forM_ vcompIds $ \vcompId ->
      when (_componentInfoId /= vcompId) $ do
        IM.lookup vcompId <$> readIORef components >>= \case
          Nothing -> pure ()
          Just ComponentState{..} ->
            case _componentMailbox (toJSON msg) of
              Nothing -> pure ()
              Just action -> _componentSink action
-----------------------------------------------------------------------------
type Socket = JSVal
-----------------------------------------------------------------------------
type WebSockets = IM.IntMap (IM.IntMap Socket)
-----------------------------------------------------------------------------
type EventSources = IM.IntMap (IM.IntMap Socket)
-----------------------------------------------------------------------------
websocketConnections :: IORef WebSockets
{-# NOINLINE websocketConnections #-}
websocketConnections = unsafePerformIO (newIORef IM.empty)
-----------------------------------------------------------------------------
websocketConnectionIds :: IORef Int
{-# NOINLINE websocketConnectionIds #-}
websocketConnectionIds = unsafePerformIO (newIORef (0 :: Int))
-----------------------------------------------------------------------------
websocketConnectText
  :: URL
  -- ^ t'WebSocket' 'URL'
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (MisoString -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
websocketConnectText url onOpen onClosed onMessage onError =
  websocketCore $ \webSocketId sink ->
    FFI.websocketConnect url
      (sink $ onOpen webSocketId)
      (sink . onClosed <=< fromJSValUnchecked)
      (pure (sink . onMessage <=< fromJSValUnchecked))
      Nothing
      Nothing
      Nothing
      (sink . onError <=< fromJSValUnchecked)
      True
-----------------------------------------------------------------------------
websocketConnectBLOB
  :: URL
  -- ^ t'WebSocket' 'URL'
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (Blob -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
websocketConnectBLOB url onOpen onClosed onMessage onError =
  websocketCore $ \webSocketId sink ->
    FFI.websocketConnect url
      (sink $ onOpen webSocketId)
      (sink . onClosed <=< fromJSValUnchecked)
      Nothing
      Nothing
      (pure (sink . onMessage . Blob))
      Nothing
      (sink . onError <=< fromJSValUnchecked)
      False
-----------------------------------------------------------------------------
websocketConnectArrayBuffer
  :: URL
  -- ^ t'WebSocket' 'URL'
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (ArrayBuffer -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
websocketConnectArrayBuffer url onOpen onClosed onMessage onError =
  websocketCore $ \webSocketId sink ->
    FFI.websocketConnect url
      (sink $ onOpen webSocketId)
      (sink . onClosed <=< fromJSValUnchecked)
      Nothing
      Nothing
      Nothing
      (pure (sink . onMessage . ArrayBuffer))
      (sink . onError <=< fromJSValUnchecked)
      False
-----------------------------------------------------------------------------
websocketConnectJSON
  :: FromJSON json
  => URL
  -- ^ WebSocket URL
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (json -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
websocketConnectJSON url onOpen onClosed onMessage onError =
  websocketCore $ \webSocketId sink ->
    FFI.websocketConnect url
      (sink $ onOpen webSocketId)
      (sink . onClosed <=< fromJSValUnchecked)
      Nothing
      (pure (\bytes -> do
          value :: Value <- fromJSValUnchecked bytes
          case fromJSON value of
            Error msg -> sink $ onError (ms msg)
            Success x -> sink $ onMessage x))
      Nothing
      Nothing
      (sink . onError <=< fromJSValUnchecked)
      False
-----------------------------------------------------------------------------
websocketConnect
  :: FromJSON json
  => URL
  -- ^ WebSocket URL
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (Payload json -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
websocketConnect url onOpen onClosed onMessage onError =
  websocketCore $ \webSocketId sink ->
    FFI.websocketConnect url
      (sink $ onOpen webSocketId)
      (sink . onClosed <=< fromJSValUnchecked)
      (pure (sink . onMessage . TEXT <=< fromJSValUnchecked))
      (pure (\bytes -> do
          value :: Value <- fromJSValUnchecked bytes
          case fromJSON value of
            Error msg -> sink $ onError (ms msg)
            Success x -> sink $ onMessage (JSON x)))
      (pure (sink . onMessage . BLOB . Blob))
      (pure (sink . onMessage . BUFFER . ArrayBuffer))
      (sink . onError <=< fromJSValUnchecked)
      False
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/WebSocket>
websocketCore
  :: (WebSocket -> Sink action -> IO Socket)
  -> Effect context props model action
websocketCore core = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    webSocketId <- freshWebSocket
    socket <- core webSocketId sink
    insertWebSocket _componentInfoId webSocketId socket
  where
    insertWebSocket :: ComponentId -> WebSocket -> Socket -> IO ()
    insertWebSocket componentId_ (WebSocket socketId) socket =
      atomicModifyIORef' websocketConnections $ \websockets ->
          (update websockets, ())
      where
        update websockets =
          IM.unionWith IM.union websockets
            $ IM.singleton componentId_
            $ IM.singleton socketId socket

    freshWebSocket :: IO WebSocket
    freshWebSocket = WebSocket <$>
      atomicModifyIORef' websocketConnectionIds (\x -> (x + 1, x))
-----------------------------------------------------------------------------
getWebSocket :: ComponentId -> WebSocket -> WebSockets -> Maybe Socket
getWebSocket vcompId (WebSocket websocketId) =
  IM.lookup websocketId <=< IM.lookup vcompId
-----------------------------------------------------------------------------
finalizeWebSockets :: ComponentId -> IO ()
finalizeWebSockets vcompId = do
  mapM_ (mapM_ FFI.websocketClose . IM.elems) .
    IM.lookup vcompId =<< readIORef websocketConnections
  dropComponentWebSockets
    where
      dropComponentWebSockets :: IO ()
      dropComponentWebSockets =
        atomicModifyIORef' websocketConnections $ \websockets ->
          (IM.delete vcompId websockets, ())
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close>
websocketClose :: WebSocket -> Effect context props model action
websocketClose socketId = do
  ComponentInfo {..} <- ask
  io_ $ do
    result <-
      atomicModifyIORef' websocketConnections $ \imap ->
        dropWebSocket _componentInfoId socketId imap =:
          getWebSocket _componentInfoId socketId imap
    case result of
      Nothing ->
        pure ()
      Just socket ->
        FFI.websocketClose socket
  where
    dropWebSocket :: ComponentId -> WebSocket -> WebSockets -> WebSockets
    dropWebSocket vcompId (WebSocket websocketId) websockets = do
      case IM.lookup vcompId websockets of
        Nothing ->
          websockets
        Just componentSockets ->
          IM.insert vcompId (IM.delete websocketId componentSockets) websockets
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
websocketSend
  :: ToJSON value
  => WebSocket
  -> Payload value
  -> Effect context props model action
websocketSend socketId msg = do
  ComponentInfo {..} <- ask
  io_ $ do
    getWebSocket _componentInfoId socketId <$> readIORef websocketConnections >>= \case
      Nothing -> pure ()
      Just socket ->
        case msg of
          JSON json_ ->
            FFI.websocketSend socket =<< toJSVal (encode json_)
          BUFFER arrayBuffer_ -> do
            FFI.websocketSend socket =<< toJSVal arrayBuffer_
          TEXT txt ->
            FFI.websocketSend socket =<< toJSVal txt
          BLOB blob_ ->
            FFI.websocketSend socket =<< toJSVal blob_
-----------------------------------------------------------------------------
-- | Retrieves current status of t'WebSocket'
--
-- If the t'WebSocket' identifier does not exist a 'CLOSED' is returned.
--
socketState :: WebSocket -> (SocketState -> action) -> Effect context props model action
socketState socketId callback = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
     getWebSocket _componentInfoId socketId <$> readIORef websocketConnections >>= \case
      Just socket -> do
        x <- socket ! ("socketState" :: MisoString)
        socketstate <- toEnum <$> fromJSValUnchecked x
        sink (callback socketstate)
      Nothing ->
        sink (callback CLOSED)
-----------------------------------------------------------------------------
codeToCloseCode :: Int -> CloseCode
codeToCloseCode = \case
  1000 -> CLOSE_NORMAL
  1001 -> CLOSE_GOING_AWAY
  1002 -> CLOSE_PROTOCOL_ERROR
  1003 -> CLOSE_UNSUPPORTED
  1005 -> CLOSE_NO_STATUS
  1006 -> CLOSE_ABNORMAL
  1007 -> Unsupported_Data
  1008 -> Policy_Violation
  1009 -> CLOSE_TOO_LARGE
  1010 -> Missing_Extension
  1011 -> Internal_Error
  1012 -> Service_Restart
  1013 -> Try_Again_Later
  1015 -> TLS_Handshake
  n    -> OtherCode n
-----------------------------------------------------------------------------
-- | Closed message is sent when a t'WebSocket' has closed
data Closed
  = Closed
  { closedCode :: CloseCode
    -- ^ The code used to indicate why a socket closed
  , wasClean :: Bool
    -- ^ If the connection was closed cleanly, or forcefully.
  , reason :: MisoString
    -- ^ The reason for socket closure.
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
instance FromJSVal Closed where
  fromJSVal o = do
    closed_ <- fmap codeToCloseCode <$> do fromJSVal =<< o ! ("code" :: MisoString)
    wasClean_ <- fromJSVal =<< o ! ("wasClean" :: MisoString)
    reason_ <- fromJSVal =<< o ! ("reason" :: MisoString)
    pure (Closed <$> closed_ <*> wasClean_ <*> reason_)
-----------------------------------------------------------------------------
-- | URL that the t'WebSocket' will @connect@ to
type URL = MisoString
-----------------------------------------------------------------------------
-- | 'SocketState' corresponding to current t'WebSocket' connection
data SocketState
  = CONNECTING -- ^ 0
  | OPEN       -- ^ 1
  | CLOSING    -- ^ 2
  | CLOSED     -- ^ 3
  deriving (Show, Eq, Ord, Enum)
-----------------------------------------------------------------------------
-- | Code corresponding to a closed connection
-- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
data CloseCode
  = CLOSE_NORMAL
   -- ^ 1000, Normal closure; the connection successfully completed whatever purpose for which it was created.
  | CLOSE_GOING_AWAY
   -- ^ 1001, The endpoint is going away, either because of a server failure or because the browser is navigating away from the page that opened the connection.
  | CLOSE_PROTOCOL_ERROR
   -- ^ 1002, The endpoint is terminating the connection due to a protocol error.
  | CLOSE_UNSUPPORTED
   -- ^ 1003, The connection is being terminated because the endpoint received data of a type it cannot accept (for example, a textonly endpoint received binary data).
  | CLOSE_NO_STATUS
   -- ^ 1005, Reserved.  Indicates that no status code was provided even though one was expected.
  | CLOSE_ABNORMAL
   -- ^ 1006, Reserved. Used to indicate that a connection was closed abnormally (that is, with no close frame being sent) when a status code is expected.
  | Unsupported_Data
   -- ^ 1007, The endpoint is terminating the connection because a message was received that contained inconsistent data (e.g., nonUTF8 data within a text message).
  | Policy_Violation
   -- ^ 1008, The endpoint is terminating the connection because it received a message that violates its policy. This is a generic status code, used when codes 1003 and 1009 are not suitable.
  | CLOSE_TOO_LARGE
   -- ^ 1009, The endpoint is terminating the connection because a data frame was received that is too large.
  | Missing_Extension
   -- ^ 1010, The client is terminating the connection because it expected the server to negotiate one or more extension, but the server didn't.
  | Internal_Error
   -- ^ 1011, The server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.
  | Service_Restart
   -- ^ 1012, The server is terminating the connection because it is restarting.
  | Try_Again_Later
   -- ^ 1013, The server is terminating the connection due to a temporary condition, e.g. it is overloaded and is casting off some of its clients.
  | TLS_Handshake
   -- ^ 1015, Reserved. Indicates that the connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).
  | OtherCode Int
   -- ^ OtherCode that is reserved and not in the range 0999
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Type for holding a t'WebSocket' file descriptor.
newtype WebSocket = WebSocket Int
  deriving stock Eq
  deriving newtype (ToJSVal, Num)
-----------------------------------------------------------------------------
-- | A null t'WebSocket' is one with a negative descriptor.
emptyWebSocket :: WebSocket
emptyWebSocket = -1
-----------------------------------------------------------------------------
-- | A type for holding an t'EventSource' descriptor.
newtype EventSource = EventSource Int
  deriving stock Eq
  deriving newtype (Num, ToJSVal)
-----------------------------------------------------------------------------
-- | A null t'EventSource' is one with a negative descriptor.
emptyEventSource :: EventSource
emptyEventSource = -1
-----------------------------------------------------------------------------
eventSourceConnections :: IORef EventSources
{-# NOINLINE eventSourceConnections #-}
eventSourceConnections = unsafePerformIO (newIORef IM.empty)
-----------------------------------------------------------------------------
eventSourceConnectionIds :: IORef Int
{-# NOINLINE eventSourceConnectionIds #-}
eventSourceConnectionIds = unsafePerformIO (newIORef (0 :: Int))
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/EventSource>
eventSourceConnectText
  :: URL
  -- ^ EventSource URL
  -> (EventSource -> action)
  -- ^ onOpen
  -> (MisoString -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
eventSourceConnectText url onOpen onMessage onError =
  eventSourceCore $ \eventSourceId sink -> do
    FFI.eventSourceConnect url
      (sink $ onOpen eventSourceId)
      (pure $ \e -> do
          txt <- fromJSValUnchecked e
          sink (onMessage txt))
      Nothing
      (sink . onError <=< fromJSValUnchecked)
      True
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/EventSource>
eventSourceConnectJSON
  :: FromJSON json
  => URL
  -- ^ EventSource URL
  -> (EventSource -> action)
  -- ^ onOpen
  -> (json -> action)
  -- ^ onMessage
  -> (MisoString -> action)
  -- ^ onError
  -> Effect context props model action
eventSourceConnectJSON url onOpen onMessage onError =
  eventSourceCore $ \eventSourceId sink -> do
    FFI.eventSourceConnect url
      (sink $ onOpen eventSourceId)
      Nothing
      (pure $ \e ->
         fromJSON <$> fromJSValUnchecked e >>= \case
            Error errMsg -> sink (onError (ms errMsg))
            Success json_ -> sink $ onMessage json_)
      (sink . onError <=< fromJSValUnchecked)
      False
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/EventSource>
eventSourceCore
  :: (EventSource -> Sink action -> IO Socket)
  -> Effect context props model action
eventSourceCore core = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    eventSourceId <- freshEventSource
    socket <- core eventSourceId sink
    insertEventSource _componentInfoId eventSourceId socket
  where
    insertEventSource :: ComponentId -> EventSource -> Socket -> IO ()
    insertEventSource componentId_ (EventSource socketId) socket =
      atomicModifyIORef' eventSourceConnections $ \eventSources ->
        (update eventSources, ())
      where
        update eventSources =
          IM.unionWith IM.union eventSources
            $ IM.singleton componentId_
            $ IM.singleton socketId socket

    freshEventSource :: IO EventSource
    freshEventSource = EventSource <$>
      atomicModifyIORef' eventSourceConnectionIds (\x -> (x + 1, x))
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close>
eventSourceClose :: EventSource -> Effect context props model action
eventSourceClose socketId = do
  ComponentInfo {..} <- ask
  io_ $ do
    result <-
      atomicModifyIORef' eventSourceConnections $ \imap ->
        dropEventSource _componentInfoId socketId imap =:
          getEventSource _componentInfoId socketId imap
    case result of
      Nothing ->
        pure ()
      Just socket ->
        FFI.eventSourceClose socket
  where
    dropEventSource :: ComponentId -> EventSource -> EventSources -> EventSources
    dropEventSource vcompId (EventSource eventSourceId) eventSources = do
      case IM.lookup vcompId eventSources of
        Nothing ->
          eventSources
        Just componentSockets ->
          IM.insert vcompId (IM.delete eventSourceId componentSockets) eventSources

    getEventSource :: ComponentId -> EventSource -> EventSources -> Maybe Socket
    getEventSource vcompId (EventSource eventSourceId) =
      IM.lookup eventSourceId <=< IM.lookup vcompId
-----------------------------------------------------------------------------
finalizeEventSources :: ComponentId -> IO ()
finalizeEventSources vcompId = do
  mapM_ (mapM_ FFI.eventSourceClose . IM.elems) .
    IM.lookup vcompId =<< readIORef eventSourceConnections
  dropComponentEventSources
    where
      dropComponentEventSources :: IO ()
      dropComponentEventSources =
        atomicModifyIORef' eventSourceConnections $ \eventSources ->
          (IM.delete vcompId eventSources, ())
-----------------------------------------------------------------------------
-- | Payload is used as the potential source of data when working with t'EventSource'
data Payload value
  = JSON value
  -- ^ JSON-encoded data
  | BLOB Blob
  -- ^ Binary encoded data
  | TEXT MisoString
  -- ^ Text encoded data
  | BUFFER ArrayBuffer
  -- ^ Buffered data
-----------------------------------------------------------------------------
-- | Smart constructor for sending JSON encoded data via an t'EventSource'
json :: ToJSON value => value -> Payload value
json = JSON
-----------------------------------------------------------------------------
-- | Smart constructor for sending binary encoded data via an t'EventSource'
blob :: Blob -> Payload value
blob = BLOB
-----------------------------------------------------------------------------
-- | Smart constructor for sending an @ArrayBuffer@ via an t'EventSource'
arrayBuffer :: ArrayBuffer -> Payload value
arrayBuffer = BUFFER
-----------------------------------------------------------------------------
#ifdef WASM
loadedJS :: IORef Bool
{-# NOINLINE loadedJS #-}
loadedJS = unsafePerformIO (newIORef False)
#endif
-----------------------------------------------------------------------------
initComponent
#ifdef NATIVE
  :: forall context props model action . (Eq context, Eq model, Eq props, ToJSON model, ToJSON action, FromJSON action)
#else
  :: forall context props model action . (Eq context, Eq model, Eq props)
#endif
  => Events
  -> Hydrate
  -> Bool
  -> context
  -- ^ Initial global @context@
  -> Component context props model action
  -> Maybe Key
  -> props
  -> StaticKey
  -> IO ()
initComponent events hydrate live initialContext vcomp_@Component {..} key props sk = do
#ifdef WASM
      $(evalFile MISO_JS_PATH)
      atomicWriteIORef loadedJS True
#endif
      withJS $ do
        let proxy = Proxy :: Proxy context
#ifdef NATIVE
        when bts $ do
          effectListener proxy =<< getMTSContext
          postComponent READY sk topLevelComponentId rootComponentId Nothing
        when mts $ do
          effectListener proxy =<< getBTSContext
          componentListener proxy =<< getBTSContext
#endif
        root <- Diff.mountElement (getMountPoint mountPoint)
        when web (cleanup proxy live root)
        atomicWriteIORef globalContext initialContext
        -- dmj: top-level Component always responsive to Context changes
        let vcomp_' = vcomp_ { useContext = True }
        void $ initialize events rootComponentId hydrate True props key sk vcomp_' (pure root)
        atomicWriteIORef schedulerThread =<< forkIO (scheduler proxy)
----------------------------------------------------------------------------
-- | Used for bidirectional cross-thread communication.
#ifdef NATIVE
effectListener :: forall context jsval . ToJSVal jsval => Proxy context -> jsval -> IO ()
effectListener Proxy jsval = void $ do
  ctx <- toJSVal jsval
  FFI.addEventListener ctx "Miso.effects" $ \msgEvent ->
    flip catch (\(e :: SomeException) ->
        FFI.consoleError ("effectListener: exception in callback: " <> ms (show e))) $ do
      msg <- Object msgEvent ! "data"
      EFFECT {..} <- fromJSValUnchecked msg :: IO EFFECT
      unsafeLookupStaticPtr effectStaticKey >>= \case
        Nothing ->
          FFI.consoleError "[effectListener]: staticPtr NOT found for effectStaticKey"
        Just ptr ->
          case deRefStaticPtr ptr of
            SomeComponent _key _props (_ :: Component context props model action) ->
              case fromJSON effectAction :: Result action of
                Success action -> do
                  comps <- readIORef components
                  case IM.lookup effectComponentId comps of
                    Nothing ->
                      FFI.consoleError $ ms $
                        "[effectListener]: ComponentId NOT registered:" <> ms effectComponentId
                    Just ComponentState {..} -> do
                      FFI.consoleLog "[effectListener]: Sinking action into Component"
                      _componentSink action
                Error e ->
                  FFI.consoleError ("[effectListener]: action decode error: " <> ms e)
#endif
----------------------------------------------------------------------------
-- | Used for unidirectional BTS -> MTS communication
#ifdef NATIVE
componentListener :: forall context . Eq context => Proxy context -> BTS -> IO ()
componentListener Proxy (BTS ctx) = void $ do
  FFI.addEventListener ctx "Miso.components" $ \msgEvent -> do
    msg <- Object msgEvent ! "data"
    COMPONENT {..} <- fromJSValUnchecked msg :: IO COMPONENT
    Just ptr <- unsafeLookupStaticPtr componentComponentStaticKey
    case deRefStaticPtr ptr of
      SomeComponent _key _props (vcomp_ :: Component context props model action) ->
        case componentComponentType of
          READY -> do -- dmj: unblocks main thread scheduler
#ifdef NATIVE
            notify btsReady
#endif
            pure ()
          MOUNT -> do
            o@(Object parent_) <- create
            FFI.set "nodeId" componentComponentParentId o
            void $ initialize mempty componentComponentId Draw False _props
              Nothing (staticKey ptr) vcomp_ (pure parent_)
          UNMOUNT ->
            IM.lookup componentComponentId <$> readIORef components >>= \case
              Nothing ->
                FFI.consoleError $ "[COMPONENT]: Couldn't find Component to unmount " <>
                  ms (show componentComponentId)
              Just c -> unmountComponent @context c
          HYDRATE -> do
            case componentComponentModel of
              Nothing ->
                FFI.consoleError "[COMPONENT]: No model to hydrate"
              Just m ->
                case fromJSON m :: Result model of
                  Success newModel ->
                    modifyComponent componentComponentId $ do
                      componentModel .= newModel
                  Error e ->
                    FFI.consoleError ("[COMPONENT]: Could not decode model: " <> e)
#endif
----------------------------------------------------------------------------
-- | Dispatches a 'COMPONENT' lifecycle message (BTS → MTS) on the
-- @\"Miso.components\"@ channel. No-op for components without a 'StaticKey'
-- (e.g. the root), since the MTS locates the component via 'unsafeLookupStaticPtr'.
#ifdef NATIVE
postComponent
  :: ComponentType
  -> StaticKey
  -> ComponentId
  -> ComponentId
  -> Maybe Value
  -> IO ()
postComponent componentType_ sk componentId_ parentId_ model_ = do
  ctx <- getMTSContext
  dispatchEvent ctx "Miso.components"
    (COMPONENT componentType_ sk componentId_ parentId_ model_)
#endif
----------------------------------------------------------------------------
-- | Dispatches an 'EFFECT' message carrying a serialized @action@ across the
-- Lynx thread boundary on the @\"Miso.effects\"@ channel:
--
--   * MTS → BTS when called on the main thread ('mts').
--   * BTS → MTS when called on the background thread ('bts').
--
-- A no-op on plain web builds (neither 'mts' nor 'bts').
#ifdef NATIVE
postEffect :: StaticKey -> ComponentId -> Value -> IO ()
postEffect sk componentId_ action_ = do
  when mts $ do
    ctx <- getBTSContext
    dispatchEvent ctx "Miso.effects" (EFFECT componentId_ action_ sk)
  when bts $ do
    ctx <- getMTSContext
    dispatchEvent ctx "Miso.effects" (EFFECT componentId_ action_ sk)
#endif
----------------------------------------------------------------------------
-- | Global variable to hold the scheduler thread
--
-- N.B. 'undefined' is safe here, it will always get populated.
-- Also, we use this in 'cleanup' when interactive mode (GHCi) is detected
-- in that circumstance 'schedulerThread' will always be populated. It's an
-- invariant.
--
schedulerThread :: IORef ThreadId
{-# NOINLINE schedulerThread #-}
schedulerThread = unsafePerformIO (newIORef undefined)
----------------------------------------------------------------------------
-- | Whether this JS execution context is the Lynx main thread, background
-- thread, or a plain web build.
--
-- N.B. this is invariant for the lifetime of a given JS context, so it's
-- safe to compute once and cache via 'unsafePerformIO' rather than making
-- an FFI call on every 'initialize' \/ 'initComponent'.
--
mts, bts, web :: Bool
{-# NOINLINE mts #-}
{-# NOINLINE bts #-}
{-# NOINLINE web #-}
(mts, bts, web) = unsafePerformIO FFI.getThreads
-----------------------------------------------------------------------------
instance FromJSVal Fingerprint where
  fromJSVal x = fmap (fmap fromMisoString) (fromJSVal x :: IO (Maybe MisoString))
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | Serializes a 'StaticKey' as a 32-character hex string (two zero-padded 'Word64' values).
instance ToMisoString Fingerprint where
  toMisoString fp = ms (show fp)
  {-# INLINE toMisoString #-}
-----------------------------------------------------------------------------
-- | Parses a 'StaticKey' from its 32-character hex 'MisoString' representation.
instance FromMisoString Fingerprint where
  fromMisoStringEither s =
    let str      = fromMisoString s :: String
        (h1, h2) = splitAt 16 str
        parseHex h = case (readHex h :: [(Word64, String)]) of
          [(w, "")] -> Right w
          _         -> Left ("fromMisoString StaticKey: invalid hex chunk " <> h)
    in Fingerprint <$> parseHex h1 <*> parseHex h2
  {-# INLINE fromMisoStringEither #-}
-----------------------------------------------------------------------------
-- | Serializes a 'Fingerprint' ('StaticKey') to its 'Show' representation.
instance ToJSVal Fingerprint where
  toJSVal fp = toJSVal (ms fp :: MisoString)
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
-- | The operation carried by a 'COMPONENT' message.
data ComponentType = MOUNT | UNMOUNT | HYDRATE | READY
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal ComponentType where
  toJSVal = \case
    MOUNT -> toJSVal ("mount"   :: MisoString)
    UNMOUNT -> toJSVal ("unmount" :: MisoString)
    HYDRATE -> toJSVal ("hydrate" :: MisoString)
    READY -> toJSVal ("ready" :: MisoString)
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal ComponentType where
  fromJSVal x = do
    fromJSVal x >>= \case
      Just ("mount" :: MisoString) -> pure (Just MOUNT)
      Just "unmount" -> pure (Just UNMOUNT)
      Just "hydrate" -> pure (Just HYDRATE)
      Just "ready" -> pure (Just READY)
      _ -> pure Nothing
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | Cross-thread component lifecycle message (BTS → MTS).
data COMPONENT = COMPONENT
  { componentComponentType :: ComponentType
  , componentComponentStaticKey :: StaticKey
  , componentComponentId :: ComponentId
  , componentComponentParentId :: ComponentId
  , componentComponentModel :: Maybe Value
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal COMPONENT where
  toJSVal COMPONENT {..} = do
    o <- create
    setField o "componentType" componentComponentType
    setField o "staticKey" componentComponentStaticKey
    setField o "compId" componentComponentId
    setField o "compParentId" componentComponentParentId
    setField o "model" componentComponentModel
    toJSVal o
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal COMPONENT where
  fromJSVal x = do
    let o = Object x
    mct  <- fromJSVal =<< getProp "componentType" o
    msk  <- fromJSVal =<< getProp "staticKey" o
    let key = fromMisoString <$> msk
    mcid <- fromJSVal =<< getProp "compId" o
    mcpid <- fromJSVal =<< getProp "compParentId" o
    mm   <- fromJSVal =<< getProp "model" o
    pure (COMPONENT <$> mct <*> key <*> mcid <*> mcpid <*> mm)
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | Cross-thread effect message (MTS → BTS or BTS → MTS).
data EFFECT = EFFECT
  { effectComponentId :: ComponentId
  , effectAction :: Value
  , effectStaticKey :: StaticKey
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal EFFECT where
  toJSVal EFFECT {..} = do
    o <- create
    setField o "componentId" effectComponentId
    setField o "action" effectAction
    setField o "staticKey" effectStaticKey
    toJSVal o
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal EFFECT where
  fromJSVal x = do
    let o = Object x
    mcid <- fromJSVal =<< getProp "componentId" o
    maction <- fromJSVal =<< getProp "action" o
    mk <- fromJSVal =<< getProp "staticKey" o
    pure (EFFECT <$> mcid <*> maction <*> mk)
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | Opaque handle to the Lynx Main Thread (MTS) context proxy.
-- Obtained via 'getMTSContext' on the background thread.
newtype MTS = MTS JSVal
  deriving stock Eq
  deriving newtype ToJSVal
-----------------------------------------------------------------------------
-- | Opaque handle to the Lynx Background Thread (BTS) context proxy.
-- Obtained via 'getBTSContext' on the main thread.
newtype BTS = BTS JSVal
  deriving stock Eq
  deriving newtype ToJSVal
-----------------------------------------------------------------------------
-- | Returns the MTS context proxy (@lynx.getCoreContext()@).
-- Call from the background thread to dispatch messages to the main thread.
getMTSContext :: IO MTS
{-# INLINABLE getMTSContext #-}
getMTSContext = MTS <$> (jsg "lynx" # "getCoreContext" $ ())
-----------------------------------------------------------------------------
-- | Returns the BTS context proxy (@lynx.getJSContext()@).
-- Call from the main thread to dispatch messages to the background thread.
getBTSContext :: IO BTS
{-# INLINABLE getBTSContext #-}
getBTSContext = BTS <$> (jsg "lynx" # "getJSContext" $ ())
-----------------------------------------------------------------------------
-- | Dispatches a cross-thread message to the BTS via @context.dispatchEvent@.
-- The @protocol@ string names the channel (e.g. @\"Miso.patches\"@).
dispatchEvent :: (ToJSVal ctx, ToJSVal a) => ctx -> MisoString -> a -> IO ()
{-# INLINABLE dispatchEvent #-}
dispatchEvent ctx protocol payload = do
  ctx_ <- toJSVal ctx
  o <- create
  setField o "type" protocol
  setField o "data" =<< toJSVal payload
  _ <- Object ctx_ # "dispatchEvent" $ [o]
  pure ()
----------------------------------------------------------------------------
-- | Loads miso's JavaScript (if not already loaded) and runs an 'IO' action.
--
-- On WASM, @miso.js@ is evaluated once on first call and skipped on subsequent calls.
-- It is safe to call 'withJS' directly (e.g. when implementing WASM tests in Playwright);
-- 'startApp' \/ 'miso' call it for you.
--
withJS
  :: IO a
  -- ^ 'IO' action to execute in between 'evalFile'
  -> IO a
withJS action = do
#ifdef WASM
  loaded <- readIORef loadedJS
  unless loaded $(evalFile MISO_JS_PATH)
  atomicWriteIORef loadedJS True
#endif
  action
-----------------------------------------------------------------------------
