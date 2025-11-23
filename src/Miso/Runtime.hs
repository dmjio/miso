-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ) where
-----------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Exception (SomeException)
import           Control.Monad (forM, forM_, when, void, forever, (<=<), zipWithM_)
import           Control.Monad.Reader (ask, asks)
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON, ToJSON, Result(..), fromJSON, toJSON, Value(Null))
import           Data.Foldable (toList)
import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, atomicWriteIORef, modifyIORef')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
#ifndef GHCJS_BOTH
import           Language.Javascript.JSaddle hiding (Sync, Result, Success)
#else
import           Language.Javascript.JSaddle
#endif
import           GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), ThreadId, killThread, threadStatus)
import           Prelude hiding (null)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
-----------------------------------------------------------------------------
import           Miso.Concurrent (Waiter(..), waiter, Mailbox, copyMailbox, readMail, sendMail, newMailbox)
import           Miso.Delegate (delegator, undelegator)
import qualified Miso.Diff as Diff
import qualified Miso.Hydrate as Hydrate
import qualified Miso.FFI.Internal as FFI
import           Miso.FFI.Internal (Blob(..), ArrayBuffer(..))
import           Miso.String hiding (reverse, drop)
import           Miso.Types
import           Miso.Util
import           Miso.CSS (renderStyleSheet)
import           Miso.Effect (ComponentInfo(..), Sub, Sink, Effect, runEffect, io_, withSink)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of t'Miso.Types.Component' between top-level API functions.
initialize
  :: (Eq parent, Eq model)
  => Hydrate
  -> Component parent model action
  -> JSM DOMRef
  -- ^ Callback function is used for obtaining the t'Miso.Types.Component' 'DOMRef'.
  -> JSM (ComponentState model action)
initialize hydrate Component {..} getComponentMountPoint = do
  Waiter {..} <- liftIO waiter
  componentActions <- liftIO (newIORef S.empty)
  let
    componentSink = \action -> liftIO $ do
      atomicModifyIORef' componentActions $ \actions -> (actions S.|> action, ())
      notify
  componentId <- liftIO freshComponentId
  componentDiffs <- liftIO newMailbox
  initializedModel <-
    case (hydrate, hydrateModel) of
      (Hydrate, Just action) ->
#ifdef SSR
         liftIO action
#else
         action
#endif
      _ -> pure model
  componentScripts <- (++) <$> renderScripts scripts <*> renderStyles styles
  componentDOMRef <- getComponentMountPoint
  componentIsDirty <- liftIO (newTVarIO False)
  componentVTree <- do
    vtree <- buildVTree hydrate (view initializedModel) componentSink logLevel events
    case hydrate of
      Draw -> do
        Diff.diff Nothing (Just vtree) componentDOMRef
      Hydrate -> do
        Hydrate.hydrate logLevel componentDOMRef vtree
    liftIO (newIORef vtree)
  componentDOMRef <# ("componentId" :: MisoString) $ componentId
  componentParentId <- do
    FFI.getParentComponentId componentDOMRef >>= \case
      Nothing -> pure rootComponentId
      Just parentId -> pure parentId
  componentSubThreads <- liftIO (newIORef M.empty)
  forM_ subs $ \sub -> do
    threadId <- FFI.forkJSM (sub componentSink)
    subKey <- liftIO freshSubId
    liftIO $ atomicModifyIORef' componentSubThreads $ \m ->
      (M.insert subKey threadId m, ())
  componentModel <- liftIO (newTVarIO initializedModel)
  let
    eventLoop = liftIO wait >> do
      currentModel <- liftIO (readTVarIO componentModel)
      let info = ComponentInfo componentId componentParentId componentDOMRef
      as <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
      updatedModel <- foldEffects update Async info componentSink (toList as) currentModel
      currentName <- liftIO $ currentModel `seq` makeStableName currentModel
      updatedName <- liftIO $ updatedModel `seq` makeStableName updatedModel
      isDirty <- liftIO (readTVarIO componentIsDirty)
      when ((currentName /= updatedName && currentModel /= updatedModel) || isDirty) $ do
        newVTree <- buildVTree Draw (view updatedModel) componentSink logLevel events
        oldVTree <- liftIO (readIORef componentVTree)
        void waitForAnimationFrame
        Diff.diff (Just oldVTree) (Just newVTree) componentDOMRef
        liftIO $ do
          atomicWriteIORef componentVTree newVTree
          mounted <- IM.size <$> readIORef components
          atomically $ do
            writeTVar componentModel updatedModel
            writeTVar componentIsDirty False
            -- dmj: reset the dirty bit
            when (mounted > 1) (writeTChan componentDiffs Null)
            -- dmj: child wake-up call for model synch.
      syncPoint
      eventLoop

  -- mailbox
  componentMailbox <- liftIO newMailbox
  componentMailboxThreadId <- do
    FFI.forkJSM . forever $ do
      message <- liftIO (readMail =<< copyMailbox componentMailbox)
      mapM_ componentSink (mailbox message)

  -- Bindings (aka. "reactive" mutable variable synchronization)
  -- Between immediate ancestor / descendant (and sibling if bidi via parent)
  let
    bidirectional = [ b | b@Bidirectional {} <- bindings ]
    parentToChild = [ b | b@ParentToChild {} <- bindings ] ++ bidirectional
    childToParent = [ b | b@ChildToParent {} <- bindings ] ++ bidirectional

  componentParentToChildThreadId <-
    synchronizeParentToChild
      componentParentId
      componentModel
      componentIsDirty
      parentToChild
      notify

  componentChildToParentThreadId <-
    synchronizeChildToParent
      componentParentId
      componentModel
      componentDiffs
      childToParent

  let vcomp = ComponentState
        { componentNotify = notify
        , ..
        }
  registerComponent vcomp
  delegator componentDOMRef componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  forM_ initialAction componentSink
  _ <- FFI.forkJSM eventLoop
  pure vcomp
-----------------------------------------------------------------------------
synchronizeChildToParent
  :: Eq parent
  => ComponentId
  -> TVar model
  -> Mailbox
  -> [ Binding parent model ]
  -> JSM (Maybe ThreadId)
synchronizeChildToParent _ _ _ [] = pure Nothing
synchronizeChildToParent parentId componentModel componentDiffs bindings = do
  -- Get parent's componentNotify, subscribe to it, on notification
  -- update the current Component model using the user-specified lenses
  IM.lookup parentId <$> liftIO (readIORef components) >>= \case
    Nothing -> do
      -- dmj: another impossible case, parent always mounted in children
      pure Nothing
    Just parentComponentState -> do
      bindProperty parentComponentState
      -- dmj: ^ parent assumes child state on initialization
      fmap Just $ FFI.forkJSM $ do
        forever $ do
          _ <- liftIO (readMail =<< copyMailbox componentDiffs)
          -- dmj: ^ listen on child this time
          bindProperty parentComponentState
  where
    bindProperty parentComponentState = do
      isDirty <- or <$> forM bindings (bindChildToParent parentComponentState componentModel)
      when isDirty $ do
        liftIO $ do
          atomically $ writeTVar (componentIsDirty parentComponentState) True
          componentNotify parentComponentState
-----------------------------------------------------------------------------
bindChildToParent
  :: forall parent model action
   . Eq parent
  => ComponentState parent action
  -- ^ Parent model
  -> TVar model
  -- ^ Child new model
  -> Binding parent model
  -- ^ Binding
  -> JSM Bool
bindChildToParent ComponentState {..} childRef = \case
  ChildToParent setParent getChild ->
    childToParent setParent getChild
  Bidirectional _ setParent getChild _ ->
    childToParent setParent getChild
  _ ->
    pure False
  where
    childToParent setParent getChild = do
       liftIO $ atomically $ do
         childModel <- readTVar childRef
         let f = setParent (getChild childModel)
         currentParent <- readTVar componentModel
         modifyTVar' componentModel f
         newParent <- readTVar componentModel
         pure (currentParent /= newParent)
-----------------------------------------------------------------------------
synchronizeParentToChild
  :: Eq model
  => ComponentId
  -> TVar model
  -> TVar Bool
  -> [ Binding type_ model ]
  -> IO ()
  -> JSM (Maybe ThreadId)
synchronizeParentToChild _ _ _ [] _ = pure Nothing
synchronizeParentToChild parentId componentModel_ componentIsDirty bindings notify= do
  -- Get parent's componentNotify, subscribe to it, on notification
  -- update the current Component model using the user-specified lenses
  IM.lookup parentId <$> liftIO (readIORef components) >>= \case
    Nothing -> do
      -- dmj: another impossible case, parent always mounted
      pure Nothing
    Just parentComponentState -> do
      bindProperty parentComponentState
      -- dmj: ^ assume parent state on initialization
      fmap Just $ FFI.forkJSM $ forever $ do
        Null <- liftIO $ readMail =<< copyMailbox (componentDiffs parentComponentState)
        bindProperty parentComponentState
  where
    bindProperty parentComponentState = do
      isDirty <- or <$> forM bindings (bindParentToChild parentComponentState componentModel_)
      when isDirty . liftIO $ do
        atomically (writeTVar componentIsDirty True)
        notify
-----------------------------------------------------------------------------
bindParentToChild
  :: forall props model action
   . Eq model
  => ComponentState props action
  -- ^ Parent model
  -> TVar model
  -- ^ Child new model
  -> Binding props model
  -- ^ binding
  -> JSM Bool
bindParentToChild ComponentState {..} modelRef = \case
  ParentToChild getParent setChild -> do
    parentToChild getParent setChild
  Bidirectional getParent _ _ setChild ->
    parentToChild getParent setChild
  _ ->
    pure False
  where
    parentToChild getParent setChild = liftIO $ atomically $ do
      parentModel <- readTVar componentModel
      let f = setChild (getParent parentModel)
      currentChild <- readTVar modelRef
      modifyTVar' modelRef f
      newChild <- readTVar modelRef
      pure (currentChild /= newChild)
-----------------------------------------------------------------------------
-- | Hydrate avoids calling @diff@, and instead calls @hydrate@
-- 'Draw' invokes 'Miso.Diff.diff'
data Hydrate
  = Draw
  | Hydrate
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' state, data associated with the lifetime of a t'Miso.Types.Component'
data ComponentState model action
  = ComponentState
  { componentId :: ComponentId
  -- ^ The ID of the current t'Miso.Types.Component'
  , componentParentId :: ComponentId
  -- ^ The ID of the t'Miso.Types.Component''s parent
  , componentSubThreads :: IORef (Map MisoString ThreadId)
  -- ^ Mapping of all 'Sub' in use by Component
  , componentDOMRef :: DOMRef
  -- ^ The DOM reference the t'Miso.Types.Component' is mounted on
  , componentVTree :: IORef VTree
  -- ^ A reference to the current virtual DOM (i.e. t'VTree')
  , componentSink :: action -> JSM ()
  -- ^ t'Miso.Types.Component' t'Sink' used to enter events into the system
  , componentModel :: TVar model
  -- ^ t'Miso.Types.Component' state
  , componentIsDirty :: TVar Bool
  -- ^ Indicator if t'Miso.Types.Component' needs to be drawn
  , componentActions :: IORef (Seq action)
  -- ^ Set of actions raised by the system
  , componentMailbox :: Mailbox
  -- ^ t'Mailbox' for receiving messages from other t'Miso.Types.Component'
  , componentScripts :: [DOMRef]
  -- ^ DOM references for \<script\> and \<style\> appended to \<head\>
  , componentMailboxThreadId :: ThreadId
  -- ^ Thread responsible for taking actions from t'Mailbox' and
  -- putting them into 'componentActions'
  , componentDiffs :: Mailbox
  -- ^ Used with t'Binding' to synchronize other t'Miso.Types.Component' state
  -- at the granularity of a t'Miso.Lens.Lens'
  , componentNotify :: IO ()
  -- ^ t'IO' action to unblock event loop thread
  , componentParentToChildThreadId :: Maybe ThreadId
  -- ^ Thread responsible for parent t'Binding' synchronization
  , componentChildToParentThreadId :: Maybe ThreadId
  -- ^ Thread responsible for child t'Binding' synchronization
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
mailboxes :: IORef (Map (Topic a) Mailbox)
{-# NOINLINE mailboxes #-}
mailboxes = unsafePerformIO $ liftIO (newIORef mempty)
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
subscribe topicName successful errorful = do
  ComponentInfo {..} <- ask
  io_ $ do
    let vcompId = _componentId
    subscribersMap <- liftIO (readIORef subscribers)
    let key = (vcompId, topicName)
    case M.lookup key subscribersMap of
      Just _ ->
        FFI.consoleWarn ("Already subscribed to: " <> ms topicName)
      Nothing -> do
        M.lookup topicName <$> liftIO (readIORef mailboxes) >>= \case
          Nothing -> do
            -- no mailbox exists, create a new one, register it and subscribe
            mailbox <- liftIO $ do
              mailbox <- newMailbox
              atomicModifyIORef' mailboxes $ \m -> (M.insert topicName mailbox m, ())
              pure mailbox
            subscribeToMailbox key mailbox vcompId
          Just mailbox -> do
            subscribeToMailbox key mailbox vcompId
  where
    subscribeToMailbox key mailbox vcompId = do
      threadId <- FFI.forkJSM $ do
        clonedMailbox <- liftIO (copyMailbox mailbox)
        ComponentState {..} <- (IM.! vcompId) <$> liftIO (readIORef components)
        forever $ do
          fromJSON <$> liftIO (readMail clonedMailbox) >>= \case
            Success msg ->
              componentSink (successful msg)
            Error msg ->
              componentSink (errorful (ms msg))
      liftIO $ atomicModifyIORef' subscribers $ \m ->
        (M.insert key threadId m, ())
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
unsubscribe :: Topic message -> Effect parent model action
unsubscribe topicName = do
  ComponentInfo {..} <- ask
  io_ (unsubscribe_ topicName _componentId)
-----------------------------------------------------------------------------
-- | Internal unsubscribe used in component unmounting and in 'unsubscribe'
unsubscribe_ :: Topic message -> ComponentId -> JSM ()
unsubscribe_ topicName vcompId = do
  let key = (vcompId, topicName)
  subscribersMap <- liftIO (readIORef subscribers)
  case M.lookup key subscribersMap of
    Just threadId -> do
      liftIO $ do
        killThread threadId
        atomicModifyIORef' subscribers $ \m ->
          (M.delete key m, ())
    Nothing ->
      pure ()
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
publish topicName value = io_ $ do
  result <- M.lookup topicName <$> liftIO (readIORef mailboxes)
  case result of
    Just mailbox ->
      liftIO $ sendMail mailbox (toJSON value)
    Nothing -> liftIO $ do
      mailbox <- newMailbox
      void $ atomicModifyIORef' mailboxes $ \m -> (M.insert topicName mailbox m, ())
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
-- | The global store of 'ComponentId', for internal-use only.
--
-- Used internally @freshComponentId@ to allocate new 'ComponentId' on
-- mount.
--
componentIds :: IORef Int
{-# NOINLINE componentIds #-}
componentIds = unsafePerformIO $ liftIO (newIORef 1)
-----------------------------------------------------------------------------
freshComponentId :: IO ComponentId
freshComponentId = atomicModifyIORef' componentIds $ \y -> (y + 1, y)
-----------------------------------------------------------------------------
-- | componentMap
--
-- This is a global t'Miso.Types.Component' @Map@ that holds the state of all currently
-- mounted t'Miso.Types.Component's
components :: IORef (IntMap (ComponentState model action))
{-# NOINLINE components #-}
components = unsafePerformIO (newIORef mempty)
-----------------------------------------------------------------------------
-- | Data type to indicate if effects should be handled asynchronously
-- or synchronously.
--
data Synchronicity
  = Async
  | Sync
  deriving (Show, Eq)
-----------------------------------------------------------------------------
syncWith :: Synchronicity -> JSM () -> JSM ()
syncWith Sync  x = x
syncWith Async x = void (FFI.forkJSM x)
-----------------------------------------------------------------------------
-- | Helper for processing effects in the event loop.
foldEffects
  :: (action -> Effect parent model action)
  -> Synchronicity
  -> ComponentInfo parent
  -> Sink action
  -> [action]
  -> model
  -> JSM model
foldEffects _ _ _ _ [] m = pure m
foldEffects update synchronicity info snk (e:es) o =
  case runEffect (update e) info o of
    (n, subs) -> do
      forM_ subs $ \sub -> do
        syncWith synchronicity $
          sub snk `catch` (void . exception)
      foldEffects update synchronicity info snk es n
  where
    exception :: SomeException -> JSM ()
    exception ex = FFI.consoleError ("[EXCEPTION]: " <> ms ex)
-----------------------------------------------------------------------------
-- | Drains the event queue before unmounting, executed synchronously
drain
  :: Component parent model action
  -> ComponentState model action
  -> JSM ()
drain app@Component{..} cs@ComponentState {..} = do
  actions <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
  let info = ComponentInfo componentId componentParentId componentDOMRef
  if S.null actions then pure () else go info actions
      where
        go info actions = do
          x <- liftIO (readTVarIO componentModel)
          y <- foldEffects update Sync info componentSink (toList actions) x
          liftIO $ atomically (writeTVar componentModel y)
          drain app cs
-----------------------------------------------------------------------------
-- | Post unmount call to drop the <style> and <script> in <head>
unloadScripts :: ComponentState model action -> JSM ()
unloadScripts ComponentState {..} = do
  forM_ componentScripts $ \domRef ->
    -- dmj: abstract this out into context
    jsg @MisoString "document"
      ! ("head" :: MisoString)
      # ("removeChild" :: MisoString)
      $ [domRef]
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a t'Miso.Types.Component'
unmount
  :: Function
  -> Component parent model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback app@Component {..} cs@ComponentState {..} = do
  undelegator componentDOMRef componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO $ do
    killThread componentMailboxThreadId
    mapM_ killThread =<< readIORef componentSubThreads
    mapM_ killThread componentParentToChildThreadId
    mapM_ killThread componentChildToParentThreadId
  killSubscribers componentId
  drain app cs
  liftIO $ atomicModifyIORef' components $ \m -> (IM.delete componentId m, ())
  finalizeWebSockets componentId
  finalizeEventSources componentId
  unloadScripts cs
-----------------------------------------------------------------------------
killSubscribers :: ComponentId -> JSM ()
killSubscribers componentId =
  mapM_ (flip unsubscribe_ componentId) =<<
    M.keys <$> liftIO (readIORef mailboxes)
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
  => Hydrate
  -> View model action
  -> Sink action
  -> LogLevel
  -> Events
  -> JSM VTree
buildVTree hydrate (VComp ns tag attrs (SomeComponent app)) snk _ _ = do
  mountCallback <- do
    FFI.syncCallback2 $ \domRef continuation -> do
      ComponentState {..} <- initialize hydrate app (pure domRef)
      vtree <- toJSVal =<< liftIO (readIORef componentVTree)
      vcompId <- toJSVal componentId
      FFI.set "componentId" vcompId (Object domRef)
      void $ call continuation global [vcompId, vtree]
  unmountCallback <- toJSVal =<< do
    FFI.syncCallback1 $ \domRef -> do
      componentId <- liftJSM (FFI.getComponentId domRef)
      IM.lookup componentId <$> liftIO (readIORef components) >>= \case
        Nothing -> pure ()
        Just componentState ->
          unmount mountCallback app componentState
  vcomp <- createNode "vcomp" ns tag
  setAttrs vcomp attrs snk (logLevel app) (events app)
  flip (FFI.set "children") vcomp =<< toJSVal ([] :: [MisoString])
  flip (FFI.set "mount") vcomp =<< toJSVal mountCallback
  FFI.set "unmount" unmountCallback vcomp
  pure (VTree vcomp)
buildVTree hydrate (VNode ns tag attrs kids) snk logLevel events = do
  vnode <- createNode "vnode" ns tag
  setAttrs vnode attrs snk logLevel events
  vchildren <- toJSVal =<< procreate
  FFI.set "children" vchildren vnode
  pure $ VTree vnode
    where
      procreate = do
        kidsViews <- forM kids $ \kid -> do
          VTree (Object vtree) <- buildVTree hydrate kid snk logLevel events
          pure vtree
        setNextSibling kidsViews
        pure kidsViews
          where
            setNextSibling xs =
              zipWithM_ (<# ("nextSibling" :: MisoString)) xs (drop 1 xs)
buildVTree _ (VText t) _ _ _ = do
  vtree <- create
  FFI.set "type" ("vtext" :: JSString) vtree
  FFI.set "ns" ("text" :: JSString) vtree
  FFI.set "text" t vtree
  pure $ VTree vtree
-----------------------------------------------------------------------------
-- | @createNode@
-- A helper function for constructing a vtree (used for @vcomp@ and @vnode@)
-- Doesn't handle children
createNode :: MisoString -> NS -> MisoString -> JSM Object
createNode typ ns tag = do
  vnode <- create
  cssObj <- create
  propsObj <- create
  eventsObj <- create
  FFI.set "css" cssObj vnode
  FFI.set "type" typ vnode
  FFI.set "props" propsObj vnode
  FFI.set "events" eventsObj vnode
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
  -> JSM ()
setAttrs vnode attrs snk logLevel events =
  forM_ attrs $ \case
    Property "key" v -> do
      value <- toJSVal v
      FFI.set "key" value vnode
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
registerComponent :: MonadIO m => ComponentState model action -> m ()
registerComponent componentState = liftIO
  $ modifyIORef' components
  $ IM.insert (componentId componentState) componentState
-----------------------------------------------------------------------------
-- | Renders styles
--
-- Meant for development purposes
-- Appends CSS to <head>
--
renderStyles :: [CSS] -> JSM [DOMRef]
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
renderScripts :: [JS] -> JSM [DOMRef]
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
        =<< (jsg @MisoString "JSON" # ("stringify" :: MisoString) $ [o])
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
    let vcompId = _componentId
    IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
      Nothing -> pure ()
      Just compState@ComponentState {..} -> do
        mtid <- liftIO (M.lookup (ms subKey) <$> readIORef componentSubThreads)
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
      tid <- FFI.forkJSM (sub componentSink)
      liftIO $ atomicModifyIORef' componentSubThreads $ \m ->
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
  vcompId <- asks _componentId
  io_ $ do
    IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
      Nothing -> do
        pure ()
      Just ComponentState {..} -> do
        mtid <- liftIO (M.lookup (ms subKey) <$> readIORef componentSubThreads)
        forM_ mtid $ \tid ->
          liftIO $ do
            atomicModifyIORef' componentSubThreads $ \m -> (M.delete (ms subKey) m, ())
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
mail vcompId message = io_ $
  IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
    Nothing ->
      -- dmj: TODO add DebugMail here
      pure ()
    Just ComponentState {..} ->
      liftIO $ sendMail componentMailbox (toJSON message)
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
mailParent message = do
  vcompId <- asks _componentParentId
  io_ $ do
    IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
      Nothing ->
        -- dmj: TODO add DebugMail here, if '0' then you're at the root
        -- w/o a parent, so no message can be sent.
        pure ()
      Just ComponentState {..} ->
        liftIO $ sendMail componentMailbox (toJSON message)
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
    IM.lookup _componentParentId <$> liftIO (readIORef components) >>= \case
      Nothing -> sink errorful
      Just ComponentState {..} -> do
        model <- liftIO (readTVarIO componentModel)
        sink (successful model)
-----------------------------------------------------------------------------
-- | Sends a message to all t'Miso.Types.Component' 'mailbox', excluding oneself.
--
-- @
--
-- update :: action -> Effect parent model actionx
-- update _ = broadcast (String "public service announcement")
-- @
--
-- @since 1.9.0.0
broadcast
  :: ToJSON message
  => message
  -> Effect parent model action
broadcast message = do
  ComponentInfo {..} <- ask
  io_ $ do
    vcomps <- liftIO (readIORef components)
    forM_ (IM.toList vcomps) $ \(vcompId, ComponentState {..}) ->
      when (_componentId /= vcompId) $ do
        liftIO $ sendMail componentMailbox (toJSON message)
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
  :: (WebSocket -> Sink action -> JSM Socket)
  -> Effect parent model action
websocketCore core = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    webSocketId <- freshWebSocket
    socket <- core webSocketId sink
    insertWebSocket _componentId webSocketId socket
  where
    insertWebSocket :: ComponentId -> WebSocket -> Socket -> JSM ()
    insertWebSocket componentId (WebSocket socketId) socket =
      liftIO $
        atomicModifyIORef' websocketConnections $ \websockets ->
          (update websockets, ())
      where
        update websockets =
          IM.unionWith IM.union websockets
            $ IM.singleton componentId
            $ IM.singleton socketId socket

    freshWebSocket :: JSM WebSocket
    freshWebSocket = WebSocket <$>
      liftIO (atomicModifyIORef' websocketConnectionIds $ \x -> (x + 1, x))
-----------------------------------------------------------------------------
getWebSocket :: ComponentId -> WebSocket -> WebSockets -> Maybe Socket
getWebSocket vcompId (WebSocket websocketId) =
  IM.lookup websocketId <=< IM.lookup vcompId
-----------------------------------------------------------------------------
finalizeWebSockets :: ComponentId -> JSM ()
finalizeWebSockets vcompId = do
  mapM_ (mapM_ FFI.websocketClose . IM.elems) =<<
    IM.lookup vcompId <$> liftIO (readIORef websocketConnections)
  dropComponentWebSockets
    where
      dropComponentWebSockets :: JSM ()
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
        dropWebSocket _componentId socketId imap =:
          getWebSocket _componentId socketId imap
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
    getWebSocket _componentId socketId <$> liftIO (readIORef websocketConnections) >>= \case
      Nothing -> pure ()
      Just socket ->
        case msg of
          JSON json_ ->
            FFI.websocketSend socket =<< FFI.jsonStringify json_
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
     getWebSocket _componentId socketId <$> liftIO (readIORef websocketConnections) >>= \case
      Just socket -> do
        x <- socket ! ("socketState" :: MisoString)
        socketstate <- toEnum <$> fromJSValUnchecked x
        sink (callback socketstate)
      Nothing ->
        sink (callback CLOSED)
-----------------------------------------------------------------------------
codeToCloseCode :: Int -> CloseCode
codeToCloseCode = go
  where
    go 1000 = CLOSE_NORMAL
    go 1001 = CLOSE_GOING_AWAY
    go 1002 = CLOSE_PROTOCOL_ERROR
    go 1003 = CLOSE_UNSUPPORTED
    go 1005 = CLOSE_NO_STATUS
    go 1006 = CLOSE_ABNORMAL
    go 1007 = Unsupported_Data
    go 1008 = Policy_Violation
    go 1009 = CLOSE_TOO_LARGE
    go 1010 = Missing_Extension
    go 1011 = Internal_Error
    go 1012 = Service_Restart
    go 1013 = Try_Again_Later
    go 1015 = TLS_Handshake
    go n    = OtherCode n
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
  :: (EventSource -> Sink action -> JSM Socket)
  -> Effect parent model action
eventSourceCore core = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    eventSourceId <- freshEventSource
    socket <- core eventSourceId sink
    insertEventSource _componentId eventSourceId socket
  where
    insertEventSource :: ComponentId -> EventSource -> Socket -> JSM ()
    insertEventSource componentId (EventSource socketId) socket =
      liftIO $
        atomicModifyIORef' eventSourceConnections $ \eventSources ->
          (update eventSources, ())
      where
        update eventSources =
          IM.unionWith IM.union eventSources
            $ IM.singleton componentId
            $ IM.singleton socketId socket

    freshEventSource :: JSM EventSource
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
        dropEventSource _componentId socketId imap =:
          getEventSource _componentId socketId imap
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
finalizeEventSources :: ComponentId -> JSM ()
finalizeEventSources vcompId = do
  mapM_ (mapM_ FFI.eventSourceClose . IM.elems) =<<
    IM.lookup vcompId <$> liftIO (readIORef eventSourceConnections)
  dropComponentEventSources
    where
      dropComponentEventSources :: JSM ()
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
