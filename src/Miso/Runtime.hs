-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Runtime
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
  , parent
  , mailParent
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
  , componentIds
  , rootComponentId
  , componentId
  , modifyComponent
  , Event (..)
  -- ** Scheduler
  , scheduler
#ifdef WASM
  , evalFile
#endif
  ) where
-----------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception (SomeException, catch)
import           Control.Monad (forM, forM_, when, void, (<=<), zipWithM_)
import           Control.Monad.Reader (ask, asks)
import           Control.Monad.State
import           Data.Foldable (toList)
import qualified Data.List as List
import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, atomicWriteIORef, modifyIORef')
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import           GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), threadStatus)
#ifdef WASM
import qualified Language.Haskell.TH as TH
#endif
import           Prelude hiding (null)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
#ifdef BENCH
import           Text.Printf
#endif
-----------------------------------------------------------------------------
import           Miso.Concurrent (Waiter(..), waiter)
import           Miso.CSS (renderStyleSheet)
import           Miso.Delegate (delegator)
import qualified Miso.Diff as Diff
import           Miso.DSL
#ifdef WASM
import           Miso.DSL.TH
#endif
import           Miso.Effect
  ( ComponentInfo(..), Sub, Sink, Effect, Schedule(..), runEffect
  , io_, withSink, Synchronicity(..)
  )
import qualified Miso.FFI.Internal as FFI
import           Miso.FFI.Internal (Blob(..), ArrayBuffer(..))
import qualified Miso.Hydrate as Hydrate
import           Miso.JSON (FromJSON, ToJSON, Result(..), fromJSON, encode, Value) -- toJSON
import           Miso.Lens hiding (view)
import           Miso.String hiding (reverse, drop)
import           Miso.Types
import           Miso.Util
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of t'Miso.Types.Component' between top-level API functions.
initialize
  :: (Eq parent, Eq model)
  => ComponentId
  -> Hydrate
  -> Bool
  -- ^ Is the root node being rendered?
  -> Component parent model action
  -> IO DOMRef
  -- ^ Callback function is used for obtaining the t'Miso.Types.Component' 'DOMRef'.
  -> IO (ComponentState parent model action)
initialize _componentParentId hydrate isRoot comp@Component {..} getComponentMountPoint = do
  _componentId <- liftIO freshComponentId
  let
    _componentSink = \action -> liftIO $ do
      atomicModifyIORef' globalEvents $ \actions ->
        (actions S.|> Event _componentId action, ())
      notify globalWaiter
  initializedModel <-
    case (hydrate, hydrateModel) of
      (Hydrate, Just action) ->
#ifdef SSR
         liftIO action
#else
         action
#endif
      _ -> pure model
  _componentScripts <- (++) <$> renderScripts scripts <*> renderStyles styles
  _componentDOMRef <- getComponentMountPoint
  _componentIsDirty <- pure False
  _componentVTree <- liftIO $ newIORef (VTree (Object jsNull))
  _componentSubThreads <- liftIO (newIORef M.empty)
  forM_ subs $ \sub -> do
    threadId <- forkIO (sub _componentSink)
    subKey <- liftIO freshSubId
    liftIO $ atomicModifyIORef' _componentSubThreads $ \m ->
      (M.insert subKey threadId m, ())
  frame <- newEmptyMVar :: IO (MVar Double)
  _componentModel <- liftIO (pure initializedModel)
  -- mailbox
  _componentMailbox <- pure S.empty

  rAFCallback <-
    asyncCallback1 $ \jsval -> do
      putMVar frame =<< fromJSValUnchecked jsval

  let _componentDraw = \newModel -> do
        newVTree <- buildVTree _componentParentId _componentId Draw _componentSink logLevel events (view newModel)
        oldVTree <- liftIO (readIORef _componentVTree)
        _frame <- requestAnimationFrame rAFCallback
        _timestamp :: Double <- takeMVar frame
        Diff.diff (Just oldVTree) (Just newVTree) _componentDOMRef
        liftIO (atomicWriteIORef _componentVTree newVTree)

  let _componentModelDirty :: Eq model => model -> model -> IO Bool
      _componentModelDirty = \currentModel updatedModel -> do
        currentName <- currentModel `seq` makeStableName currentModel
        updatedName <- updatedModel `seq` makeStableName updatedModel
        pure (currentName /= updatedName && currentModel /= updatedModel)


  let _componentApplyActions = \actions model_ -> do
        let info = ComponentInfo _componentId _componentParentId _componentDOMRef
        List.foldl' (\(m, ss) a ->
          case runEffect (update a) info m of
            (n, sss) -> (n, ss <> sss)) (model_, []) actions

  let _componentModelDirty = \currentModel updatedModel -> do
       currentName <- liftIO (currentModel `seq` makeStableName currentModel)
       updatedName <- liftIO (updatedModel `seq` makeStableName updatedModel)
       pure (currentName /= updatedName && currentModel /= updatedModel)

  let vcomp = ComponentState
        { _componentEvents = events
        , _componentMailbox = mailbox
        , _componentBindings = bindings
        , ..
        }

  registerComponent vcomp
  if isRoot
    then
      delegator _componentDOMRef _componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
    else
      addToDelegatedEvents logLevel events

  initialDraw initializedModel hydrate isRoot comp vcomp

  forM_ initialAction _componentSink
  when isRoot $ void (forkIO scheduler)
  pure vcomp
-----------------------------------------------------------------------------
scheduler :: IO ()
scheduler = liftIO (wait globalWaiter) >> do
  getBatch >>= \case
    Nothing ->
      scheduler
    Just (vcompId, actions) -> do
      commit vcompId actions
      renderComponents
      scheduler
-----------------------------------------------------------------------------
-- | Perform a top-down rendering of the Component tree.
--
-- We re-read the components each time to account for unmounting
--
renderComponents :: IO ()
renderComponents = do
  componentIds_ <- IM.keys <$> liftIO (readIORef components)
  forM_ componentIds_ $ \vcompId ->
    IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
      Nothing -> pure () -- component unmounted
      Just ComponentState {..} -> do
        when _componentIsDirty (_componentDraw _componentModel)
        modifyComponent _componentId (isDirty .= False)
-----------------------------------------------------------------------------
-- | Modify a single t'Component p m a' at a t'ComponentId'
modifyComponent
  :: MonadIO m
  => ComponentId
  -> State (ComponentState parent model action) a
  -> m ()
modifyComponent vcompId go = liftIO $ do
  atomicModifyIORef' components $ \vcomps ->
    case IM.lookup vcompId vcomps of
      Nothing -> (vcomps, ())
      Just vcomp -> (IM.insert vcompId (execState go vcomp) vcomps, ())
-----------------------------------------------------------------------------
-- | Runs a t'State' monad action across all t'ComponentState'.
modifyComponents
  :: MonadIO m
  => State (ComponentState parent model action) a
  -> m ()
modifyComponents go = liftIO $ do
  atomicModifyIORef' components $ \vcomps ->
    (IM.map (execState go) vcomps, ())
-----------------------------------------------------------------------------
commit :: ComponentId -> [Event action] -> IO ()
commit vcompId _ = do
  -- todo: handle topics, bindings
  ComponentState {..} <- (IM.! vcompId) <$> liftIO (readIORef components)
  let actions = mempty -- mapMaybe _componentMailbox (toList _componentInbox) <> (event <$> events)
  case _componentApplyActions actions _componentModel of
    (updated, schedules) -> do
      forM_ schedules $ \case
        Schedule Async action ->
          evalScheduled Async (action _componentSink)
        Schedule Sync  action ->
          evalScheduled Sync (action _componentSink)
      dirty <- _componentModelDirty _componentModel updated
      modifyComponent _componentId $ do
        isDirty .= dirty
        componentModel .= updated
-----------------------------------------------------------------------------
-- | Propagate bindings out to all the other Component.
_crawl :: ComponentId -> IO ()
_crawl compId = evalStateT go [compId]
  where
    go :: StateT [ComponentId] IO ()
    go = get >>= \case
      [] -> pure ()
      nextId : _ -> do
        ComponentState {} <- (IM.! nextId) <$> liftIO (readIORef components)
        pure ()
-----------------------------------------------------------------------------
_pop :: StateT [s] IO (Maybe s)
_pop = get >>= \case
  [] -> pure Nothing
  (x:xs) -> do
    put xs
    pure (Just x)
-----------------------------------------------------------------------------
_push :: s -> StateT [s] IO ()
_push s = modify (s:)
-----------------------------------------------------------------------------
initialDraw :: Eq m => m -> Hydrate -> Bool -> Component p m a -> ComponentState p m a -> IO ()
initialDraw initializedModel hydrate isRoot Component {..} ComponentState {..} = do
#ifdef BENCH
  start <- FFI.now
#endif
  vtree <- buildVTree _componentParentId _componentId hydrate _componentSink logLevel events (view initializedModel)
#ifdef BENCH
  end <- FFI.now
  when isRoot $ FFI.consoleLog $ ms (printf "buildVTree: %.3f ms" (end - start) :: String)
#endif
  case hydrate of
    Draw -> do
      Diff.diff Nothing (Just vtree) _componentDOMRef
      atomicWriteIORef _componentVTree vtree
    Hydrate -> do
      when isRoot $ do
        hydrated <- Hydrate.hydrate logLevel _componentDOMRef vtree
        if hydrated
          then atomicWriteIORef _componentVTree vtree
          else do
            liftIO $ do -- dmj: reset state
              atomicWriteIORef components IM.empty
              atomicWriteIORef componentIds topLevelComponentId
              atomicWriteIORef subscribers mempty
              -- atomicWriteIORef mailboxes mempty
            newTree <- buildVTree _componentParentId _componentId Draw _componentSink logLevel events (view initializedModel)
            Diff.diff Nothing (Just newTree) _componentDOMRef
            liftIO (atomicWriteIORef _componentVTree newTree)
-----------------------------------------------------------------------------
addToDelegatedEvents :: LogLevel -> Events -> IO ()
addToDelegatedEvents logLevel events = do
  root <- (IM.! topLevelComponentId) <$> liftIO (readIORef components)
  delegated <- M.unions . fmap _componentEvents . IM.elems <$>
    liftIO (readIORef components)
  forM_ (M.assocs events) $ \(eventName, capture) ->
    case M.lookup eventName delegated of
      Just delegatedCapture
        | delegatedCapture == capture -> pure ()
      _ ->
        delegator (_componentDOMRef root) (_componentVTree root)
          (M.singleton eventName capture)
          (logLevel `elem` [DebugEvents, DebugAll])
-----------------------------------------------------------------------------
data Event action
  = Event
  { eventComponentId :: {-# UNPACK #-} !ComponentId
  , event :: action
  }
-----------------------------------------------------------------------------
getBatch
  :: IO (Maybe (ComponentId, [Event action]))
getBatch = do
  liftIO $ atomicModifyIORef' globalEvents $ \case
    S.Empty -> (mempty, Nothing)
    event@(Event vcompId _) S.:<| rest ->
      case S.spanl ((== vcompId) . eventComponentId) rest of
        (componentActions, xs) ->
          (xs, pure (vcompId, event : toList componentActions))
-----------------------------------------------------------------------------
globalEvents :: IORef (Seq (Event action))
{-# NOINLINE globalEvents #-}
globalEvents = unsafePerformIO (newIORef mempty)
-----------------------------------------------------------------------------
globalWaiter :: Waiter
{-# NOINLINE globalWaiter #-}
globalWaiter = unsafePerformIO waiter
-----------------------------------------------------------------------------
componentId :: Lens (ComponentState parent model action) ComponentId
componentId = lens _componentId $ \record field -> record { _componentId = field }
-----------------------------------------------------------------------------
isDirty :: Lens (ComponentState parent model action) Bool
isDirty = lens _componentIsDirty $ \record field -> record { _componentIsDirty = field }
-----------------------------------------------------------------------------
componentModel :: Lens (ComponentState parent model action) model
componentModel = lens _componentModel $ \record field -> record { _componentModel = field }
-----------------------------------------------------------------------------
-- | Hydrate avoids calling @diff@, and instead calls @hydrate@
-- 'Draw' invokes 'Miso.Diff.diff'
data Hydrate
  = Draw
  | Hydrate
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' state, data associated with the lifetime of a t'Miso.Types.Component'
data ComponentState parent model action
  = ComponentState
  { _componentId :: ComponentId
  -- ^ The ID of the current t'Miso.Types.Component'
  , _componentParentId :: ComponentId
  -- ^ The ID of the t'Miso.Types.Component''s parent
  , _componentSubThreads :: IORef (Map MisoString ThreadId)
  -- ^ Mapping of all 'Sub' in use by Component
  , _componentDOMRef :: DOMRef
  -- ^ The DOM reference the t'Miso.Types.Component' is mounted on
  , _componentVTree :: IORef VTree
  -- ^ A reference to the current virtual DOM (i.e. t'VTree')
  , _componentSink :: action -> IO ()
  -- ^ t'Miso.Types.Component' t'Sink' used to enter events into the system
  , _componentModel :: model
  -- ^ t'Miso.Types.Component' state
  , _componentIsDirty :: Bool
  -- ^ Indicator if t'Miso.Types.Component' needs to be drawn
  , _componentScripts :: [DOMRef]
  -- ^ DOM references for \<script\> and \<style\> appended to \<head\>
  , _componentEvents :: Events
  -- ^ List of events a Component listens on
  , _componentBindings :: [Binding parent model]
  , _componentMailbox :: Value -> Maybe action
  , _componentDraw :: model -> IO ()
  , _componentModelDirty :: model -> model -> IO Bool
  , _componentApplyActions :: [action] -> model -> (model, [Schedule action])
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
  deriving (Ord, Eq, Show, ToMisoString)
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
-- update_ :: Action -> Effect Int Action
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
-- mailboxes :: IORef (Map (Topic a) Mailbox)
-- {-# NOINLINE mailboxes #-}
-- mailboxes = unsafePerformIO $ liftIO (newIORef mempty)
-----------------------------------------------------------------------------
subscribers :: IORef (Map (ComponentId, Topic a) ThreadId)
{-# NOINLINE subscribers #-}
subscribers = unsafePerformIO $ liftIO (newIORef mempty)
-----------------------------------------------------------------------------
-- | Subscribes to a @Topic@, provides callback function that writes to t'Miso.Types.Component' 'Sink'
--
-- If a @Topic message@ does not exist when calling 'subscribe' it is generated dynamically.
-- Each subscriber decodes the received 'Value' using it's own 'FromJSON' instance. This provides
-- for loose-coupling between t'Miso.Types.Component'. As long as the underlying 'Value' are identical
-- t'Miso.Types.Component' can use their own types without serialization issues. @Topic message@ should
-- have their own JSON API specification when being distributed.
--
-- @
--
-- arithmetic :: Topic Message
-- arithmetic = topic "arithmetic"
--
-- clientComponent :: MisoString -> Component Int Action
-- clientComponent name = component 0 update_ $ \m ->
--   div_
--   []
--   [ br_ []
--   , text (name <> " : " <> ms (m ^. _id))
--   , button_ [ onClick Unsubscribe ] [ "unsubscribe" ]
--   , button_ [ onClick Subscribe ] [ "subscribe" ]
--   ] where
--       update_ :: Action -> Effect Int Action
--       update_ = \case
--         AddOne -> do
--           _id += 1
--         SubtractOne ->
--           _id -= 1
--         Unsubscribe ->
--           unsubscribe arithmetic
--         Subscribe ->
--           subscribe arithmetic Notification
--         Notification (Success Increment) -> do
--           update_ AddOne
--         Notification (Success Decrement) -> do
--           update_ SubtractOne
--         Notification (Error msg) ->
--           io_ $ consoleError ("Decode failure: " <> ms msg)
--         _ -> pure ()
--
-- @
--
-- @since 1.9.0.0
subscribe
  :: FromJSON message
  => Topic message
  -> (message -> action)
  -> (MisoString -> action)
  -> Effect parent model action
subscribe _ _ _ = undefined
-- subscribe topicName successful errorful = undefined
-----------------------------------------------------------------------------
-- Pub / Sub implementation
--
-- (Subscribe)
--
-- Check if you're already subscribed to this topic.
--
--  [true]  - If you're already subscribed, then it's a no-op (warn)
--
--  [false] - If you're not subscribed then fork a new thread that holds the duplicated topic
--            and blocks on the read end of the duplicated topic, sink messages into component sink
--
-- (Unsubscribe)
--
-- Check if you're already subscribed to this topic
--
--  [true] - Kill the thread, delete the subscriber entry
--
--  [false] - If you're not subscribed, then it's a no-op (warn)
--
-- (Publish)
--
-- Check if the Topic exists
--
--  [true] - If it exists then write the message to the topic
--
--  [false] - If it doesn't exist, create it.
--
-- N.B. Components can be both publishers and subscribers to their own topics.
-----------------------------------------------------------------------------
-- | Unsubscribe to a t'Topic'
--
-- Unsubscribes a t'Miso.Types.Component' from receiving messages from t'Topic'
--
-- See 'subscribe' for more use.
--
-- @since 1.9.0.0
-- unsubscribe :: Topic message -> Effect parent model action
-- unsubscribe _ = undefined
  -- ComponentInfo {..} <- ask
  -- io_ (unsubscribe_ topicName _componentId)
-----------------------------------------------------------------------------
-- | Internal unsubscribe used in component unmounting and in 'unsubscribe'
unsubscribe :: Topic message -> ComponentId -> IO ()
unsubscribe _ _ = undefined
  -- let key = (vcompId, topicName)
  -- subscribersMap <- liftIO (readIORef subscribers)
  -- case M.lookup key subscribersMap of
  --   Just threadId -> do
  --     liftIO $ do
  --       killThread threadId
  --       atomicModifyIORef' subscribers $ \m ->
  --         (M.delete key m, ())
  --   Nothing ->
  --     pure ()
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
-- server :: Component () Action
-- server = component () update_ $ \() ->
--   div_
--   []
--   [ "Server component"
--   , button_ [ onClick AddOne ] [ "+" ]
--   , button_ [ onClick SubtractOne ] [ "-" ]
--   , component_ (client_ "client 1")
--     [ onMountedWith Mount
--     ]
--   , component_ (client_ "client 2")
--     [ onMountedWith Mount
--     ]
--   ] where
--       update_ :: Action -> Transition () Action
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
  -> Effect parent model action
publish _ _ = undefined
  -- result <- M.lookup topicName <$> liftIO (readIORef _mailboxes)
  -- case result of
  --   Just mailbox ->
  --     liftIO $ sendMail mailbox (toJSON value)
  --   Nothing -> liftIO $ do
  --     mailbox <- newMailbox
  --     void $ atomicModifyIORef' mailboxes $ \m -> (M.insert topicName mailbox m, ())
-----------------------------------------------------------------------------
subIds :: IORef Int
{-# NOINLINE subIds #-}
subIds = unsafePerformIO $ liftIO (newIORef 0)
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
componentIds = unsafePerformIO $ liftIO (newIORef topLevelComponentId)
-----------------------------------------------------------------------------
freshComponentId :: IO ComponentId
freshComponentId = atomicModifyIORef' componentIds $ \y -> (y + 1, y)
-----------------------------------------------------------------------------
-- | componentMap
--
-- This is a global t'Miso.Types.Component' @Map@ that holds the state of all currently
-- mounted t'Miso.Types.Component's
components :: IORef (IntMap (ComponentState parent model action))
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
-- | Helper for processing effects in the event loop.
foldEffects
  :: (action -> Effect parent model action)
  -> Bool
  -- ^ Whether or not the Component is unmounting
  -> ComponentInfo parent
  -> Sink action
  -> [action]
  -> model
  -> IO model
foldEffects _ _ _ _ [] m = pure m
foldEffects update drainSink info snk (e:es) o =
  case runEffect (update e) info o of
    (n, subs) -> do
      forM_ subs $ \(Schedule synchronicity sub) -> do
        let
          action = sub snk `catch` (void . exception)
        if drainSink
          then evalScheduled Sync action
          else evalScheduled synchronicity action
      foldEffects update drainSink info snk es n
-----------------------------------------------------------------------------
-- | Drains the event queue before unmounting, executed synchronously
drain
  :: Component parent model action
  -> ComponentState parent model action
  -> IO ()
drain app@Component{..} cs@ComponentState {..} = do
  actions <- undefined
  -- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
  let info = ComponentInfo _componentId _componentParentId _componentDOMRef
  if S.null actions then pure () else go info actions
      where
        go info actions = do
          x <- pure _componentModel
          _ <- foldEffects update True info _componentSink (toList actions) x
          -- liftIO $ atomically (writeTVar componentModel y)
          drain app cs
-----------------------------------------------------------------------------
-- | Post unmount call to drop the <style> and <script> in <head>
unloadScripts :: ComponentState parent model action -> IO ()
unloadScripts ComponentState {..} = do
  head_ <- FFI.getHead
  forM_ _componentScripts $ \domRef ->
    FFI.removeChild head_ domRef
-----------------------------------------------------------------------------
-- | Helper to drop all lifecycle and mounting hooks if defined.
freeLifecycleHooks :: ComponentState parent model action -> IO ()
freeLifecycleHooks ComponentState {..} = do
  VTree (Object vcomp) <- liftIO (readIORef _componentVTree)
  mapM_ freeFunction =<< fromJSVal =<< vcomp ! ("onMounted" :: MisoString)
  mapM_ freeFunction =<< fromJSVal =<< vcomp ! ("onUnmounted" :: MisoString)
  mapM_ freeFunction =<< fromJSVal =<< vcomp ! ("onBeforeMounted" :: MisoString)
  mapM_ freeFunction =<< fromJSVal =<< vcomp ! ("onBeforeUnmounted" :: MisoString)
  mapM_ freeFunction =<< fromJSVal =<< vcomp ! ("mount" :: MisoString)
  mapM_ freeFunction =<< fromJSVal =<< vcomp ! ("unmount" :: MisoString)
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a t'Miso.Types.Component'
unmount
  :: Component parent model action
  -> ComponentState parent model action
  -> IO ()
unmount app cs@ComponentState {..} = do
  liftIO $ do
    mapM_ killThread =<< readIORef _componentSubThreads
  killSubscribers _componentId
  drain app cs
  finalizeWebSockets _componentId
  finalizeEventSources _componentId
  unloadScripts cs
  freeLifecycleHooks cs
  liftIO $ atomicModifyIORef' components $ \m -> (IM.delete _componentId m, ())
-----------------------------------------------------------------------------
killSubscribers :: ComponentId -> IO ()
killSubscribers _ = pure ()
  -- mapM_ (flip unsubscribe_ componentId) =<<
  --   M.keys <$> liftIO (readIORef _mailboxes)
-----------------------------------------------------------------------------
-- | Internal function for construction of a Virtual DOM.
--
-- Component mounting should be synchronous.
-- Mounting causes a recursive diffing to occur
-- (creating sub components as detected), setting up
-- infrastructure for each sub-component. During this
-- process we go between the Haskell heap and the JS heap.
buildVTree
  :: Eq model
  => ComponentId
  -> ComponentId
  -> Hydrate
  -> Sink action
  -> LogLevel
  -> Events
  -> View model action
  -> IO VTree
buildVTree parentId vcompId hydrate snk logLevel_ events_ = \case
  VComp attrs (SomeComponent app) -> do
    vcomp <- create

    mountCallback <- do
      if hydrate == Hydrate
        then
          toJSVal jsNull
        else
          syncCallback1' $ \parent_ -> do
            ComponentState {..} <- initialize vcompId Draw False app (pure parent_)
            vtree <- toJSVal =<< readIORef _componentVTree
            FFI.set "parent" vcomp (Object vtree)
            obj <- create
            setProp "componentId" _componentId obj
            setProp "componentTree" vtree obj
            toJSVal obj

    unmountCallback <- toJSVal =<< do
      FFI.syncCallback1 $ \vcompId_ -> do
        componentId_ <- fromJSValUnchecked vcompId_
        IM.lookup componentId_ <$> readIORef components >>= \case
          Nothing -> pure ()
          Just componentState ->
            unmount app componentState

    case hydrate of
      Hydrate -> do
        -- Mock .domRef for use during hydration
        domRef <- toJSVal =<< create
        ComponentState {..} <- initialize vcompId hydrate False app (pure domRef)
        vtree <- toJSVal =<< liftIO (readIORef _componentVTree)
        FFI.set "parent" vcomp (Object vtree)
        vcompId_ <- toJSVal _componentId
        FFI.set "componentId" vcompId_ vcomp
        FFI.set "child" vtree vcomp
      Draw -> do
        FFI.set "child" jsNull vcomp

    setAttrs vcomp attrs snk (logLevel app) (events app)
    when (hydrate == Draw) (FFI.set "mount" mountCallback vcomp)
    FFI.set "unmount" unmountCallback vcomp
    FFI.set "eventPropagation" (eventPropagation app) vcomp
    flip (FFI.set "type") vcomp =<< toJSVal VCompType
    pure (VTree vcomp)
  VNode ns tag attrs kids -> do
    vnode <- createNode "vnode" ns tag
    setAttrs vnode attrs snk logLevel_ events_
    vchildren <- toJSVal =<< procreate vnode
    flip (FFI.set "children") vnode vchildren
    flip (FFI.set "type") vnode =<< toJSVal VNodeType
    pure (VTree vnode)
      where
        procreate parentVTree = do
          kidsViews <- forM kids $ \kid -> do
            VTree child <- buildVTree parentId vcompId hydrate snk logLevel_ events_ kid
            FFI.set "parent" parentVTree child
            pure child
          setNextSibling kidsViews
          pure kidsViews
            where
              setNextSibling xs =
                zipWithM_ (flip setField "nextSibling")
                  xs (drop 1 xs)
  VText key t -> do
    vtree <- create
    flip (FFI.set "type") vtree =<< toJSVal VTextType
    forM_ key $ \k -> FFI.set "key" (ms k) vtree
    FFI.set "ns" ("text" :: MisoString) vtree
    FFI.set "text" t vtree
    pure (VTree vtree)
-----------------------------------------------------------------------------
-- | @createNode@
-- A helper function for constructing a vtree (used for @vcomp@ and @vnode@)
-- Doesn't handle children
createNode :: MisoString -> NS -> MisoString -> IO Object
createNode typ ns tag = do
  vnode <- create
  cssObj <- create
  propsObj <- create
  eventsObj <- create
  captures <- create
  bubbles <- create
  FFI.set "css" cssObj vnode
  FFI.set "type" typ vnode
  FFI.set "props" propsObj vnode
  FFI.set "events" eventsObj vnode
  FFI.set "captures" captures eventsObj
  FFI.set "bubbles" bubbles eventsObj
  FFI.set "ns" ns vnode
  FFI.set "tag" tag vnode
  pure vnode
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
setAttrs vnode@(Object jval) attrs snk logLevel events =
  forM_ attrs $ \case
    Property "key" v -> do
      value <- toJSVal v
      FFI.set "key" value vnode
    ClassList classes ->
      FFI.populateClass jval classes
    Property k v -> do
      value <- toJSVal v
      o <- getProp "props" vnode
      FFI.set k value (Object o)
    On callback ->
      callback snk (VTree vnode) logLevel events
    Styles styles -> do
      cssObj <- getProp "css" vnode
      forM_ (M.toList styles) $ \(k,v) -> do
        FFI.set k v (Object cssObj)
-----------------------------------------------------------------------------
-- | Registers components in the global state
registerComponent :: MonadIO m => ComponentState parent model action -> m ()
registerComponent componentState = liftIO
  $ modifyIORef' components
  $ IM.insert (_componentId componentState) componentState
-----------------------------------------------------------------------------
-- | Renders styles
--
-- Meant for development purposes
-- Appends CSS to <head>
--
renderStyles :: [CSS] -> IO [DOMRef]
renderStyles styles =
  forM styles $ \case
    Href url -> FFI.addStyleSheet url
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
    Src src ->
      FFI.addSrc src
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
        =<< fromJSValUnchecked
        =<< do jsg "JSON" # "stringify" $ [o]
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
  -> Sub action
  -> Effect parent model action
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
            status <- liftIO (threadStatus tid)
            case status of
              ThreadFinished -> startThread compState
              ThreadDied -> startThread compState
              _ -> pure ()
  where
    startThread ComponentState {..} = do
      tid <- forkIO (sub _componentSink)
      liftIO $ atomicModifyIORef' _componentSubThreads $ \m ->
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
stopSub :: ToMisoString subKey => subKey -> Effect parent model action
stopSub subKey = do
  vcompId <- asks _componentInfoId
  io_ $ do
    IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
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
-- mail componentId ("test message" :: MisoString) :: Effect parent model action
-- @
--
-- @since 1.9.0.0
mail
  :: ToJSON message
  => ComponentId
  -> message
  -> Effect parent model action
mail vcompId _ = io_ $
  modifyComponent vcompId $ do
    pure ()
    -- componentInbox %= (S.|> toJSON m)
-----------------------------------------------------------------------------
-- | Send any @ToJSON message => message@ to the parent's t'Miso.Types.Component' mailbox
--
-- @
-- mailParent ("test message" :: MisoString) :: Effect parent model action
-- @
--
-- @since 1.9.0.0
mailParent
  :: ToJSON message
  => message
  -> Effect parent model action
mailParent _ = do
  ComponentInfo {..} <- ask
  io_ $ modifyComponent _componentInfoParentId $ do
    pure () -- componentInbox %= (S.|> toJSON m)
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
  -> (MisoString -> action)
  -> Value
  -> Maybe action
checkMail successful errorful value =
  pure $ case fromJSON value of
    Success x -> successful x
    Error err -> errorful (ms err)
-----------------------------------------------------------------------------
-- | Fetches the parent `model` from the child.
--
-- @since 1.9.0.0
parent
  :: (parent -> action)
  -> action
  -> Effect parent model action
parent successful errorful = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    IM.lookup _componentInfoParentId <$> liftIO (readIORef components) >>= \case
      Nothing -> sink errorful
      Just ComponentState {..} -> do
        sink (successful _componentModel)
-----------------------------------------------------------------------------
-- | Sends a message to all t'Miso.Types.Component' 'mailbox', excluding oneself.
--
-- @
--
-- update :: action -> Effect parent model action
-- update _ = broadcast (String "public service announcement")
-- @
--
-- @since 1.9.0.0
broadcast
  :: ToJSON message
  => message
  -> Effect parent model action
broadcast _ = do
  ComponentInfo {..} <- ask
  io_ $ modifyComponents $ do
    vcompId <- use componentId -- dmj: just use mapMaybe mailbox to insert into componentActions
    when (vcompId /= _componentInfoId) $ do
      pure ()
      -- componentInbox %= (S.|> toJSON message)
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
  -> Effect parent model action
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
  -> Effect parent model action
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
  -> Effect parent model action
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
  -> Effect parent model action
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
  -> Effect parent model action
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
  -> Effect parent model action
websocketCore core = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    webSocketId <- freshWebSocket
    socket <- core webSocketId sink
    insertWebSocket _componentInfoId webSocketId socket
  where
    insertWebSocket :: ComponentId -> WebSocket -> Socket -> IO ()
    insertWebSocket componentId_ (WebSocket socketId) socket =
      liftIO $
        atomicModifyIORef' websocketConnections $ \websockets ->
          (update websockets, ())
      where
        update websockets =
          IM.unionWith IM.union websockets
            $ IM.singleton componentId_
            $ IM.singleton socketId socket

    freshWebSocket :: IO WebSocket
    freshWebSocket = WebSocket <$>
      liftIO (atomicModifyIORef' websocketConnectionIds $ \x -> (x + 1, x))
-----------------------------------------------------------------------------
getWebSocket :: ComponentId -> WebSocket -> WebSockets -> Maybe Socket
getWebSocket vcompId (WebSocket websocketId) =
  IM.lookup websocketId <=< IM.lookup vcompId
-----------------------------------------------------------------------------
finalizeWebSockets :: ComponentId -> IO ()
finalizeWebSockets vcompId = do
  mapM_ (mapM_ FFI.websocketClose . IM.elems) =<<
    IM.lookup vcompId <$> liftIO (readIORef websocketConnections)
  dropComponentWebSockets
    where
      dropComponentWebSockets :: IO ()
      dropComponentWebSockets = liftIO $
        atomicModifyIORef' websocketConnections $ \websockets ->
          (IM.delete vcompId websockets, ())
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close>
websocketClose :: WebSocket -> Effect parent model action
websocketClose socketId = do
  ComponentInfo {..} <- ask
  io_ $ do
    result <- liftIO $
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
  -> Effect parent model action
websocketSend socketId msg = do
  ComponentInfo {..} <- ask
  io_ $ do
    getWebSocket _componentInfoId socketId <$> liftIO (readIORef websocketConnections) >>= \case
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
socketState :: WebSocket -> (SocketState -> action) -> Effect parent model action
socketState socketId callback = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
     getWebSocket _componentInfoId socketId <$> liftIO (readIORef websocketConnections) >>= \case
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
  deriving (ToJSVal, Eq, Num)
-----------------------------------------------------------------------------
-- | A null t'WebSocket' is one with a negative descriptor.
emptyWebSocket :: WebSocket
emptyWebSocket = (-1)
-----------------------------------------------------------------------------
-- | A type for holding an t'EventSource' descriptor.
newtype EventSource = EventSource Int
  deriving (ToJSVal, Eq, Num)
-----------------------------------------------------------------------------
-- | A null t'EventSource' is one with a negative descriptor.
emptyEventSource :: EventSource
emptyEventSource = (-1)
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
  -> Effect parent model action
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
  -> Effect parent model action
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
  -> Effect parent model action
eventSourceCore core = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    eventSourceId <- freshEventSource
    socket <- core eventSourceId sink
    insertEventSource _componentInfoId eventSourceId socket
  where
    insertEventSource :: ComponentId -> EventSource -> Socket -> IO ()
    insertEventSource componentId_ (EventSource socketId) socket =
      liftIO $
        atomicModifyIORef' eventSourceConnections $ \eventSources ->
          (update eventSources, ())
      where
        update eventSources =
          IM.unionWith IM.union eventSources
            $ IM.singleton componentId_
            $ IM.singleton socketId socket

    freshEventSource :: IO EventSource
    freshEventSource = EventSource <$>
      liftIO (atomicModifyIORef' eventSourceConnectionIds $ \x -> (x + 1, x))
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close>
eventSourceClose :: EventSource -> Effect parent model action
eventSourceClose socketId = do
  ComponentInfo {..} <- ask
  io_ $ do
    result <- liftIO $
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
  mapM_ (mapM_ FFI.eventSourceClose . IM.elems) =<<
    IM.lookup vcompId <$> liftIO (readIORef eventSourceConnections)
  dropComponentEventSources
    where
      dropComponentEventSources :: IO ()
      dropComponentEventSources = liftIO $
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
-----------------------------------------------------------------------------
-- | Like 'eval', but read the JS code to evaluate from a file.
evalFile :: FilePath -> TH.Q TH.Exp
evalFile path = eval_ =<< TH.runIO (readFile path)
  where
    eval_ :: String -> TH.Q TH.Exp
    eval_ chunk = [| $(Miso.DSL.TH.evalTH chunk []) :: IO () |]
-----------------------------------------------------------------------------
#endif
-----------------------------------------------------------------------------
