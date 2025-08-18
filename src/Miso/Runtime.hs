-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
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
  , runView
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
  , ComponentState
  -- ** Communication
  , mail
  , checkMail
  , broadcast
  , parent
  , mailParent
  -- ** WebSocket
  , websocketConnect
  , websocketSend
  , websocketClose
  , socketState
  , emptyWebSocket
  , WebSocket
  , URL
  , SocketState (..)
  , CloseCode (..)
  , Closed (..)
  -- ** EventSource
  , eventSourceConnect
  , eventSourceClose
  , emptyEventSource
  , EventSource
  ) where
-----------------------------------------------------------------------------
import           Control.Exception (SomeException)
import           Control.Monad (forM, forM_, when, void, forever, (<=<))
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
import qualified JavaScript.Array as JSArray
#ifndef GHCJS_BOTH
import           Language.Javascript.JSaddle hiding (Sync, Result, Success)
#else
import           Language.Javascript.JSaddle
#endif
import           GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), ThreadId, killThread, threadStatus)
import           GHC.Generics (Generic)
import           Prelude hiding (null)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))
-----------------------------------------------------------------------------
import           Miso.Concurrent (Waiter(..), waiter, Mailbox, copyMailbox, readMail, sendMail, newMailbox)
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff (diff)
import qualified Miso.FFI.Internal as FFI
import           Miso.String hiding (reverse)
import           Miso.Types
import           Miso.Util
import           Miso.CSS (renderStyleSheet)
import           Miso.Event (Events)
import           Miso.Property (textProp)
import           Miso.Effect (ComponentInfo(..), Sub, Sink, Effect, runEffect, io_, withSink)
import           System.Mem.Weak (Weak, mkWeak, deRefWeak)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of @Component@ between top-level API functions.
initialize
  :: Eq model
  => Component parent model action
  -> (Sink action -> JSM ([DOMRef], DOMRef, IORef VTree))
  -- ^ Callback function is used to perform the creation of VTree
  -> JSM (ComponentState model action)
initialize Component {..} getView = do
  Waiter {..} <- liftIO waiter
  componentActions <- liftIO (newIORef S.empty)
  let
    componentSink = \action -> liftIO $ do
      atomicModifyIORef' componentActions $ \actions -> (actions S.|> action, ())
      serve
  componentId <- liftIO freshComponentId
  componentDiffs <- liftIO newMailbox
  (componentScripts, componentDOMRef, componentVTree) <- getView componentSink
  componentDOMRef <# ("componentId" :: MisoString) $ componentId
  componentSubThreads <- liftIO (newIORef M.empty)
  forM_ subs $ \sub -> do
    threadId <- FFI.forkJSM (sub componentSink)
    subKey <- liftIO freshSubId
    liftIO $ atomicModifyIORef' componentSubThreads $ \m ->
      (M.insert subKey threadId m, ())
  componentModelCurrent <- liftIO (newIORef model)
  componentModelNew <- liftIO (newIORef model)
  let
    eventLoop = liftIO wait >> do
      currentModel <- liftIO (readIORef componentModelCurrent)
      newModel <- liftIO (readIORef componentModelNew)
      let
        info = ComponentInfo componentId componentDOMRef
      as <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
      updatedModel <- foldEffects update Async info componentSink (toList as) newModel
      currentName <- liftIO $ currentModel `seq` makeStableName currentModel
      updatedName <- liftIO $ updatedModel `seq` makeStableName updatedModel
      when (currentName /= updatedName && currentModel /= updatedModel) $ do
        newVTree <- runView Draw (view updatedModel) componentSink logLevel events
        oldVTree <- liftIO (readIORef componentVTree)
        void waitForAnimationFrame
        diff (Just oldVTree) (Just newVTree) componentDOMRef
        liftIO $ do
          atomicWriteIORef componentVTree newVTree
          atomicWriteIORef componentModelCurrent updatedModel
          atomicWriteIORef componentModelNew updatedModel
          sendMail componentDiffs Null
          -- dmj: child wake-up call for prop synch
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
      componentDOMRef
      componentModelNew
      parentToChild
      serve

  componentChildToParentThreadId <-
    synchronizeChildToParent
      componentDOMRef
      componentModelNew
      componentDiffs
      childToParent

  let vcomp = ComponentState
        { componentServe = serve
        , ..
        }
  registerComponent vcomp
  delegator componentDOMRef componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  forM_ initialAction componentSink
  _ <- FFI.forkJSM eventLoop
  pure vcomp
-----------------------------------------------------------------------------
synchronizeChildToParent
  :: DOMRef
  -> IORef model
  -> Mailbox
  -> [ Binding parent model ]
  -> JSM (Maybe ThreadId)
synchronizeChildToParent _ _ _ [] = pure Nothing
synchronizeChildToParent componentDOMRef componentModelNew componentDiffs bindings = do
  -- Get parent's componentNotify, subscribe to it, on notification
  -- update the current Component model using the user-specified lenses
  FFI.getParentComponentId componentDOMRef >>= \case
    Nothing ->
      -- dmj: impossible case, parent always mounted
      pure Nothing
    Just parentId -> do
      IM.lookup parentId <$> liftIO (readIORef components) >>= \case
        Nothing -> do
          -- dmj: another impossible case, parent always mounted in children
          pure Nothing
        Just parentComponentState -> do
          bindProperty parentComponentState
          -- dmj: ^ parent assumes child state on initialization
          fmap Just $ FFI.forkJSM $ forever $ do
            Null <- liftIO $ readMail =<< copyMailbox componentDiffs
            -- dmj: ^ listen on child this time
            bindProperty parentComponentState
  where
    bindProperty parentComponentState = do
      forM_ bindings (bindChildToParent parentComponentState componentModelNew)
      liftIO (componentServe parentComponentState)
-----------------------------------------------------------------------------
bindChildToParent
  :: forall parent model action
   . ComponentState parent action
  -- ^ Parent model
  -> IORef model
  -- ^ Child new model
  -> Binding parent model
  -- ^ Binding
  -> JSM ()
bindChildToParent ComponentState {..} childRef = \case
  ChildToParent setParent getChild ->
    childToParent setParent getChild
  Bidirectional _ setParent getChild _ ->
    childToParent setParent getChild
  _ ->
    pure ()
  where
    childToParent setParent getChild = do
      childModel <- liftIO (readIORef childRef)
      let newParent = setParent (getChild childModel)
      liftIO $ atomicModifyIORef' componentModelNew $ \m -> (newParent m, ())
-----------------------------------------------------------------------------
synchronizeParentToChild
  :: DOMRef
  -> IORef model
  -> [ Binding type_ model ]
  -> IO ()
  -> JSM (Maybe ThreadId)
synchronizeParentToChild _ _ [] _ = pure Nothing
synchronizeParentToChild componentDOMRef componentModel_ bindings serve = do
  -- Get parent's componentNotify, subscribe to it, on notification
  -- update the current Component model using the user-specified lenses
  FFI.getParentComponentId componentDOMRef >>= \case
    Nothing ->
      -- dmj: impossible case, parent always mounted
      pure Nothing
    Just parentId -> do
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
      forM_ bindings (bindParentToChild parentComponentState componentModel_)
      liftIO serve
-----------------------------------------------------------------------------
bindParentToChild
  :: forall props model action
   . ComponentState props action
  -- ^ Parent model
  -> IORef model
  -- ^ Child new model
  -> Binding props model
  -- ^ binding
  -> JSM ()
bindParentToChild ComponentState {..} modelRef = \case
  ParentToChild getParent setChild -> do
    parentToChild getParent setChild
  Bidirectional getParent _ _ setChild ->
    parentToChild getParent setChild
  _ ->
    pure ()
  where
    parentToChild getParent setChild = do
      parentModel <- liftIO (readIORef componentModelNew)
      let newChild = setChild (getParent parentModel)
      liftIO $ atomicModifyIORef' modelRef $ \m -> (newChild m, ())
-----------------------------------------------------------------------------
-- | 'Hydrate' avoids calling @diff@, and instead calls @hydrate@
-- 'Draw' invokes 'diff'
data Hydrate
  = Draw
  | Hydrate
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | @Component@ state, data associated with the lifetime of a @Component@
data ComponentState model action
  = ComponentState
  { componentId              :: ComponentId
  , componentSubThreads      :: IORef (Map MisoString ThreadId)
  , componentDOMRef          :: DOMRef
  , componentVTree           :: IORef VTree
  , componentSink            :: action -> JSM ()
  , componentModelCurrent    :: IORef model
  , componentModelNew        :: IORef model
  , componentActions         :: IORef (Seq action)
  , componentMailbox         :: Mailbox
  , componentScripts         :: [DOMRef]
  , componentMailboxThreadId :: ThreadId
  , componentDiffs           :: Mailbox
  , componentServe           :: IO ()
    -- ^ What the current component writes to, to notify anybody about its dirty model
  , componentParentToChildThreadId  :: Maybe ThreadId
  , componentChildToParentThreadId  :: Maybe ThreadId
    -- ^ What the current component listens on to invoke model synchronization
  }
-----------------------------------------------------------------------------
-- | A 'Topic' represents a place to send and receive messages. 'Topic' is used to facilitate
-- communication between 'Component'. 'Component' can 'subscribe' to or 'publish' to any 'Topic',
-- within the same 'Component' or across 'Component'.
--
-- This requires creating a custom 'ToJSON' / 'FromJSON'. Any other 'Component'
-- can 'publish' or 'subscribe' to this @Topic message@. It is a way to provide
-- loosely-coupled communication between 'Components'.
--
-- See 'publish', 'subscribe', 'unsubscribe' for more details.
--
-- When distributing 'Component' for third-party use, it is recommended to export
-- the 'Topic', where 'message' is the JSON protocol.
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
-- | Subscribes to a 'Topic', provides callback function that writes to 'Component' 'Sink'
--
-- If a @Topic message@ does not exist when calling 'subscribe' it is generated dynamically.
-- Each subscriber decodes the received 'Value' using it's own 'FromJSON' instance. This provides
-- for loose-coupling between 'Component'. As long as the underlying 'Value' are identical
-- 'Component' can use their own types without serialization issues. @Topic message@ should
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
-- | Unsubscribe to a 'Topic'
--
-- Unsubscribes a 'Component' from receiving messages from @Topic message@
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
-- | Publish to a @Topic message@
--
-- @Topic message@ are generated dynamically if they do not exist. When using 'publish'
-- all subscribers are immediately notified of a new message. A message is distributed as a 'Value'
-- The underlying 'ToJSON' instance is used to construct this 'Value'.
--
-- We recommend documenting a public API for the JSON protocol message when distributing a 'Component'
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
--       update_ :: Action -> Effect () Action
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
componentIds :: IORef Int
{-# NOINLINE componentIds #-}
componentIds = unsafePerformIO $ liftIO (newIORef 0)
-----------------------------------------------------------------------------
freshComponentId :: IO ComponentId
freshComponentId = atomicModifyIORef' componentIds $ \y -> (y + 1, y)
-----------------------------------------------------------------------------
-- | componentMap
--
-- This is a global @Component@ @Map@ that holds the state of all currently
-- mounted @Component@s
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
--------------------------------------------------
-- | Internally used for runView parent and startComponent
-- Initial draw helper
-- If hydrateing, bypass diff and continue copying
drawComponent
  :: Hydrate
  -> DOMRef
  -> Component parent model action
  -> Sink action
  -> JSM ([DOMRef], JSVal, IORef VTree)
drawComponent hydrate mountElement Component {..} snk = do
  refs <- (++) <$> renderScripts scripts <*> renderStyles styles
  vtree <- runView hydrate (view model) snk logLevel events
  when (hydrate == Draw) (diff Nothing (Just vtree) mountElement)
  ref <- liftIO (newIORef vtree)
  pure (refs, mountElement, ref)
-----------------------------------------------------------------------------
-- | Drains the event queue before unmounting, executed synchronously
drain
  :: Component parent model action
  -> ComponentState model action
  -> JSM ()
drain app@Component{..} cs@ComponentState {..} = do
  actions <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
  let info = ComponentInfo componentId componentDOMRef
  if S.null actions then pure () else go info actions
  unloadScripts cs
      where
        go info actions = do
          x <- liftIO (readIORef componentModelCurrent)
          y <- foldEffects update Sync info componentSink (toList actions) x
          liftIO (atomicWriteIORef componentModelCurrent y)
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
-- | Helper function for cleanly destroying a @Component@
unmount
  :: Function
  -> Component parent model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback app@Component {..} cs@ComponentState {..} = do
  undelegator componentDOMRef componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO (killThread componentMailboxThreadId)
  liftIO (mapM_ killThread =<< readIORef componentSubThreads)
  liftIO (mapM_ killThread componentParentToChildThreadId)
  liftIO (mapM_ killThread componentChildToParentThreadId)
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
runView
  :: Hydrate
  -> View model action
  -> Sink action
  -> LogLevel
  -> Events
  -> JSM VTree
runView hydrate (VComp ns tag attrs (SomeComponent app)) snk _ _ = do
  mountCallback <- do
    FFI.syncCallback2 $ \domRef continuation -> do
      ComponentState {..} <- initialize app (drawComponent hydrate domRef app)
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
runView hydrate (VNode ns tag attrs kids) snk logLevel events = do
  vnode <- createNode "vnode" ns tag
  setAttrs vnode attrs snk logLevel events
  vchildren <- ghcjsPure . jsval =<< procreate
  FFI.set "children" vchildren vnode
  sync <- FFI.shouldSync =<< toJSVal vnode
  FFI.set "shouldSync" sync vnode
  pure $ VTree vnode
    where
      procreate = do
        kidsViews <- forM kids $ \kid -> do
          VTree (Object vtree) <- runView hydrate kid snk logLevel events
          pure vtree
        ghcjsPure (JSArray.fromList kidsViews)
runView _ (VText t) _ _ _ = do
  vtree <- create
  FFI.set "type" ("vtext" :: JSString) vtree
  FFI.set "ns" ("text" :: JSString) vtree
  FFI.set "text" t vtree
  pure $ VTree vtree
runView hydrate (VTextRaw str) snk logLevel events =
  case parseView str of
    [] ->
      runView hydrate (VText (" " :: MisoString)) snk logLevel events
    [parent_] ->
      runView hydrate parent_ snk logLevel events
    kids -> do
      runView hydrate (VNode HTML "div" mempty kids) snk logLevel events
-----------------------------------------------------------------------------
-- | @createNode@
-- A helper function for constructing a vtree (used for 'vcomp' and 'vnode')
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
    Event callback ->
      callback snk (VTree vnode) logLevel events
    Styles styles -> do
      cssObj <- getProp "css" vnode
      forM_ (M.toList styles) $ \(k,v) -> do
        FFI.set k v (Object cssObj)
-----------------------------------------------------------------------------
-- | Used to support RawText, inlining of HTML.
-- Filters tree to only branches and leaves w/ Text tags.
-- converts to `View m a`. Note: if HTML is malformed,
-- (e.g. closing tags and opening tags are present) they will
-- be removed.
parseView :: MisoString -> [View model action]
parseView html = reverse (go (parseTree html) [])
  where
    go [] xs = xs
    go (TagLeaf (TagText s) : next) views =
      go next (VText s : views)
    go (TagLeaf (TagOpen name attrs) : next) views =
      go (TagBranch name attrs [] : next) views
    go (TagBranch name attrs kids : next) views =
      let
        attrs' = [ textProp key value
                 | (key, value) <- attrs
                 ]
        newNode =
          VNode HTML name attrs' (reverse (go kids []))
      in
        go next (newNode:views)
    go (TagLeaf _ : next) views =
      go next views
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
renderStyles :: [CSS] -> JSM [JSVal]
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
renderScripts :: [JS] -> JSM [JSVal]
renderScripts scripts =
  forM scripts $ \case
    Src src ->
      FFI.addSrc src
    Script script ->
      FFI.addScript script
    ImportMap importMap -> do
      o <- create
      forM_ importMap $ \(k,v) ->
        FFI.set k v o
      FFI.addScriptImportMap
        =<< fromJSValUnchecked
        =<< (jsg @MisoString "JSON" # ("stringify" :: MisoString) $ [o])
-----------------------------------------------------------------------------
-- | Starts a named 'Sub' dynamically, during the life of a 'Component'.
-- The 'Sub' can be stopped by calling @Ord subKey => stop subKey@ from the 'update' function.
-- All 'Sub' started will be stopped if a 'Component' is unmounted.
--
-- @
-- data SubType = LoggerSub | TimerSub
--   deriving (Eq, Ord)
--
-- update Action =
--   startSub LoggerSub $ \sink -> forever (threadDelay (secs 1) >> consoleLog "test")
-- @
--
-- @since 1.9.0.0
startSub :: ToMisoString subKey => subKey -> Sub action -> Effect parent model action
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
-- | Stops a named 'Sub' dynamically, during the life of a 'Component'.
-- All 'Sub' started will be stopped automatically if a 'Component' is unmounted.
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
-- | Send any @ToJSON message => message@ to a 'Component' mailbox, by 'ComponentId'
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
-- | Send any @ToJSON message => message@ to the parent's @Component@ mailbox
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
  domRef <- asks _componentDOMRef
  io_ $ do
    FFI.getParentComponentId domRef >>= \case
      Nothing ->
        pure ()
      Just vcompId ->
        IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
          Nothing ->
            -- dmj: TODO add DebugMail here
            pure ()
          Just ComponentState {..} ->
            liftIO $ sendMail componentMailbox (toJSON message)
----------------------------------------------------------------------------
-- | Helper function for processing 'Mail' from 'mail'.
--
-- @
--
-- data Action
--   = ParsedMail Message
--   | ErrorMail MisoString
--
-- main = app { mailbox = checkMail ParsedMail ErrorMail }
--
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
    FFI.getParentComponentId _componentDOMRef >>= \case
      Nothing ->
        sink errorful
      Just parentId -> do
        IM.lookup parentId <$> liftIO (readIORef components) >>= \case
          Nothing -> sink errorful
          Just ComponentState {..} -> do
            model <- liftIO (readIORef componentModelCurrent)
            sink (successful model)
-----------------------------------------------------------------------------
-- | Sends a message to all @Component@ 'mailbox', excluding oneself.
--
-- @
--
--   broadcast (String "public service announcement")
--
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
type Socket = Weak JSVal
-----------------------------------------------------------------------------
type WebSockets = IntMap (IntMap Socket)
-----------------------------------------------------------------------------
type EventSources = IntMap (IntMap Socket)
-----------------------------------------------------------------------------
websocketConnections :: IORef WebSockets
{-# NOINLINE websocketConnections #-}
websocketConnections = unsafePerformIO (newIORef mempty)
-----------------------------------------------------------------------------
websocketConnectionIds :: IORef WebSocketId
{-# NOINLINE websocketConnectionIds #-}
websocketConnectionIds = unsafePerformIO (newIORef (0 :: Int))
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/WebSocket>
websocketConnect
  :: URL
  -- ^ WebSocket URL
  -> (WebSocket -> action)
  -- ^ onOpen
  -> (Closed -> action)
  -- ^ onClosed
  -> (JSVal -> action)
  -- ^ onMessage
  -> (JSVal -> action)
  -- ^ onError
  -> Effect parent model action
websocketConnect url onOpen onClosed onError onMessage = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    webSocketId <- freshWebSocket
    let key = WebSocket webSocketId
    socket <- FFI.websocketConnect url
      (sink $ onOpen key)
      (sink . onClosed <=< fromJSValUnchecked)
      (sink . onMessage)
      (sink . onError)
    ctx <- askJSM
    weakPtr <- liftIO $ mkWeak key socket $ pure $ do
      flip runJSM ctx $ do
        FFI.consoleLog "in finalizer..."
        websocketClose_ _componentId key
    insertWebSocket _componentId webSocketId weakPtr
  where
    insertWebSocket :: ComponentId -> WebSocketId -> Socket -> JSM ()
    insertWebSocket componentId socketId socket =
      liftIO $
        atomicModifyIORef' websocketConnections $ \websockets ->
          (update websockets, ())
      where
        update websockets =
          IM.unionWith IM.union websockets
            $ IM.singleton componentId
            $ IM.singleton socketId socket
  
    freshWebSocket :: JSM WebSocketId
    freshWebSocket =
      liftIO (atomicModifyIORef' websocketConnectionIds $ \x -> (x + 1, x))
-----------------------------------------------------------------------------
getWebSocket :: ComponentId -> WebSocket -> WebSockets -> Maybe Socket
getWebSocket vcompId (WebSocket websocketId) =
  IM.lookup websocketId <=< IM.lookup vcompId
-----------------------------------------------------------------------------
finalizeWebSockets :: ComponentId -> JSM ()
finalizeWebSockets vcompId = do
  IM.lookup vcompId <$> liftIO (readIORef websocketConnections) >>= \case
    Nothing ->
      pure ()
    Just conns ->
      forM_ (IM.elems conns) $ \conn ->
        mapM_ FFI.websocketClose =<<
          liftIO (deRefWeak conn)
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
  io_ (websocketClose_ _componentId socketId)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/close>
websocketClose_ :: ComponentId -> WebSocket -> JSM ()
websocketClose_ componentId socketId = do
    result <- liftIO $
      atomicModifyIORef' websocketConnections $ \imap ->
        dropWebSocket componentId socketId imap =:
          getWebSocket componentId socketId imap
    case result of
      Nothing ->
        pure ()
      Just weakPtr -> do
        liftIO (deRefWeak weakPtr) >>=
          mapM_ FFI.websocketClose
-----------------------------------------------------------------------------
dropWebSocket :: ComponentId -> WebSocket -> WebSockets -> WebSockets
dropWebSocket vcompId (WebSocket websocketId) websockets = do
  case IM.lookup vcompId websockets of
    Nothing ->
      websockets
    Just componentSockets ->
      IM.insert vcompId (IM.delete websocketId componentSockets) websockets
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/send>
websocketSend :: WebSocket -> JSVal -> Effect parent model action
websocketSend socketId msg = do
  ComponentInfo {..} <- ask
  io_ $ do
    getWebSocket _componentId socketId <$> liftIO (readIORef websocketConnections) >>= \case
      Nothing -> pure ()
      Just weak -> do
        liftIO (deRefWeak weak) >>= \case
          Nothing -> pure ()
          Just socket -> do
            FFI.websocketSend socket msg
-----------------------------------------------------------------------------
-- | Retrieves current status of `WebSocket`
--
-- If the 'WebSocket' identifier does not exist a 'CLOSED' is returned.
--
socketState :: WebSocket -> (SocketState -> action) -> Effect parent model action
socketState socketId callback = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    getWebSocket _componentId socketId <$> liftIO (readIORef websocketConnections) >>= \case
      Nothing ->
        sink (callback CLOSED)
      Just weak -> do
        liftIO (deRefWeak weak) >>= \case
          Nothing ->
            sink (callback CLOSED)
          Just socket -> do
            x <- socket ! ("socketState" :: MisoString)
            socketstate <- toEnum <$> fromJSValUnchecked x
            sink (callback socketstate)
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
data Closed
  = Closed
  { closedCode :: CloseCode
  , wasClean :: Bool
  , reason :: MisoString
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
instance FromJSVal Closed where
  fromJSVal o = do
    closed_ <- fmap codeToCloseCode <$> do fromJSVal =<< o ! ("code" :: MisoString)
    wasClean_ <- fromJSVal =<< o ! ("wasClean" :: MisoString)
    reason_ <- fromJSVal =<< o ! ("reason" :: MisoString)
    pure (Closed <$> closed_ <*> wasClean_ <*> reason_)
-----------------------------------------------------------------------------
-- | URL that the @Websocket@ will @connect@ to
type URL = MisoString
-----------------------------------------------------------------------------
-- | `SocketState` corresponding to current WebSocket connection
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
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
instance ToJSVal CloseCode
-----------------------------------------------------------------------------
instance FromJSVal CloseCode
-----------------------------------------------------------------------------
data WebSocket = WebSocket Int
  deriving (Show, Eq)
-----------------------------------------------------------------------------
type WebSocketId = Int
-----------------------------------------------------------------------------
instance ToJSVal WebSocket where
  toJSVal (WebSocket socket) = toJSVal socket
-----------------------------------------------------------------------------
emptyWebSocket :: WebSocket
emptyWebSocket = WebSocket (-1)
-----------------------------------------------------------------------------
data EventSource = EventSource Int
  deriving (Eq, Show)
-----------------------------------------------------------------------------
instance ToJSVal EventSource where
  toJSVal (EventSource socket) = toJSVal socket
-----------------------------------------------------------------------------
emptyEventSource :: EventSource
emptyEventSource = EventSource (-1)
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
eventSourceConnect
  :: URL
  -- ^ EventSource URL
  -> (EventSource -> action)
  -- ^ onOpen
  -> (JSVal -> action)
  -- ^ onMessage
  -> (JSVal -> action)
  -- ^ onError
  -> Effect parent model action
eventSourceConnect url onOpen onMessage onError = do
  ComponentInfo {..} <- ask
  withSink $ \sink -> do
    eventSourceId <- freshEventSource
    socket <- FFI.eventSourceConnect url
      (sink $ onOpen eventSourceId)
      (sink . onMessage)
      (sink . onError)
    ctx <- askJSM
    weakPtr <- liftIO $ mkWeak eventSourceId socket $ pure $ do
      flip runJSM ctx (eventSourceClose_ _componentId eventSourceId)

    insertEventSource _componentId eventSourceId weakPtr
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
  io_ (eventSourceClose_ _componentId socketId)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/EventSource/close>
eventSourceClose_ :: ComponentId -> EventSource -> JSM ()
eventSourceClose_ _componentId socketId = do
    result <- liftIO $
      atomicModifyIORef' eventSourceConnections $ \imap ->
        dropEventSource _componentId socketId imap =:
          getEventSource _componentId socketId imap
    case result of
      Nothing ->
        pure ()
      Just weak -> do
        liftIO (deRefWeak weak) >>= \case
          Nothing -> pure ()
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
  IM.lookup vcompId <$> liftIO (readIORef eventSourceConnections) >>= \case
    Nothing ->
      pure ()
    Just conns ->
      forM_ (IM.elems conns) $ \conn ->
        mapM_ FFI.eventSourceClose =<<
          liftIO (deRefWeak conn)
  dropComponentEventSources
    where
      dropComponentEventSources :: JSM ()
      dropComponentEventSources = liftIO $
        atomicModifyIORef' eventSourceConnections $ \eventSources ->
          (IM.delete vcompId eventSources, ())
-----------------------------------------------------------------------------
