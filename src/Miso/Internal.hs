-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Internal
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Internal
  ( -- * Internal functions
    initialize
  , freshComponentId
  , runView
  , renderStyles
  , Hydrate(..)
  -- * Subscription
  , startSub
  , stopSub
  -- * Pub / Sub
  , subscribe
  , unsubscribe
  , publish
  , TopicName
  , mkTopic
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (forM, forM_, when, void, forever)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON, ToJSON, fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import           Data.Foldable (toList)
import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, atomicWriteIORef, modifyIORef')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import qualified JavaScript.Array as JSArray
#ifndef GHCJS_BOTH
import           Language.Javascript.JSaddle hiding (Sync, Result)
#else
import           Language.Javascript.JSaddle
#endif
import           GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), ThreadId, killThread, threadStatus)
import           Prelude hiding (null)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))
-----------------------------------------------------------------------------
import           Miso.Concurrent (Waiter(..), waiter, Topic, copyTopic, readTopic, writeTopic, newTopic)
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff (diff)
import           Miso.Exception (exception)
import qualified Miso.FFI.Internal as FFI
import           Miso.String hiding (reverse)
import           Miso.Types
import           Miso.Style (renderStyleSheet)
import           Miso.Event (Events)
import           Miso.Property (textProp)
import           Miso.Effect (Sub, Sink, Effect, runEffect, io_)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of @Component@ between top-level API functions.
initialize
  :: Eq model
  => Component model action
  -> (Sink action -> JSM (MisoString, JSVal, IORef VTree))
  -- ^ Callback function is used to perform the creation of VTree
  -> JSM (IORef VTree)
initialize Component {..} getView = do
  Waiter {..} <- liftIO waiter
  componentActions <- liftIO (newIORef S.empty)
  let
    componentSink = \action -> liftIO $ do
      atomicModifyIORef' componentActions $ \actions -> (actions S.|> action, ())
      serve
  (componentId, componentMount, componentVTree) <- getView componentSink
  componentSubThreads <- liftIO (newIORef M.empty)
  forM_ subs $ \sub -> do
    threadId <- FFI.forkJSM (sub componentSink)
    subKey <- liftIO freshSubId
    liftIO $ atomicModifyIORef' componentSubThreads $ \m ->
      (M.insert subKey threadId m, ())
  componentModel <- liftIO (newIORef model)
  let
    eventLoop !oldModel = liftIO wait >> do
      as <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
      newModel <- foldEffects update Async componentId componentSink (toList as) oldModel
      oldName <- liftIO $ oldModel `seq` makeStableName oldModel
      newName <- liftIO $ newModel `seq` makeStableName newModel
      when (oldName /= newName && oldModel /= newModel) $ do
        newVTree <- runView Draw (view newModel) componentSink logLevel events
        oldVTree <- liftIO (readIORef componentVTree)
        void waitForAnimationFrame
        diff (Just oldVTree) (Just newVTree) componentMount
        liftIO $ do
          atomicWriteIORef componentVTree newVTree
          atomicWriteIORef componentModel newModel
      syncPoint
      eventLoop newModel
  _ <- FFI.forkJSM (eventLoop model)
  registerComponent ComponentState {..}
  delegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  forM_ initialAction componentSink
  pure componentVTree
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
  { componentId         :: ComponentId
  , componentSubThreads :: IORef (Map MisoString ThreadId)
  , componentMount      :: JSVal
  , componentVTree      :: IORef VTree
  , componentSink       :: action -> JSM ()
  , componentModel      :: IORef model
  , componentActions    :: IORef (Seq action)
  }
-----------------------------------------------------------------------------
newtype TopicName a = TopicName MisoString
  deriving (Eq, Show, Ord)
-----------------------------------------------------------------------------
topics :: IORef (Map MisoString Topic)
{-# NOINLINE topics #-}
topics = unsafePerformIO $ liftIO (newIORef mempty)
-----------------------------------------------------------------------------
subscribers :: IORef (Map (ComponentId, MisoString) ThreadId)
{-# NOINLINE subscribers #-}
subscribers = unsafePerformIO $ liftIO (newIORef mempty)
-----------------------------------------------------------------------------
-- | Subscribes to a 'TopicName', provides callback function that writes to 'Component' sink
--
-- If a 'TopicName' does not exist when calling 'subscribe' it is generated dynamically.
-- Each subscriber decodes the received 'Value' using it's own 'FromJSON' instance. This provides
-- for loose-coupling between 'Component'. As long as the underlying 'Value' are identical
-- 'Component' can use their own types without serialization issues. 'TopicName' should
-- have their own JSON API specification when being distributed.
--
-- @
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
--           unsubscribe "command"
--         Subscribe ->
--           subscribe "command" Notification
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
subscribe
  :: FromJSON message
  => TopicName message
  -> (message -> action)
  -> Effect model action
subscribe (TopicName topicName) toAction = do
  vcompId <- ask
  io_ $ do
    subscribersMap <- liftIO (readIORef subscribers)
    let key = (vcompId, topicName)
    case M.lookup key subscribersMap of
      Just _ ->
        FFI.consoleWarn ("Already subscribed to: " <> topicName)
      Nothing -> do
        M.lookup topicName <$> liftIO (readIORef topics) >>= \case
          Nothing -> do
            FFI.consoleLog "topic doesn't exist..."
          Just topic -> do
            subscribeTopic key topic vcompId
  where
    subscribeTopic key topic vcompId = do
      threadId <- FFI.forkJSM $ do
        clonedTopic <- liftIO (copyTopic topic)
        ComponentState {..} <- (M.! vcompId) <$> liftIO (readIORef components)
        forever $ do
          message <- liftIO (readTopic clonedTopic)
          case fromJSON message of
            Aeson.Success r -> componentSink $ toAction r
            Aeson.Error r -> FFI.consoleError ("Decode failure: " <> ms (show r))
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
-- Unsubscribes a 'Component' from receiving messages from 'TopicName'.
--
-- See 'subscribe' for use
--
unsubscribe :: TopicName a -> Effect model action
unsubscribe topicName = ask >>= io_ . unsubscribe_ topicName
-----------------------------------------------------------------------------
-- | Internal unsubscribe used in component unmounting and in 'unsubscribe'
unsubscribe_ :: TopicName a -> ComponentId -> JSM ()
unsubscribe_ (TopicName topicName) vcompId = do
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
-- | Publish to a 'Topic'
--
-- 'TopicName' are generated dynamically if they do not exist. When using 'publish'
-- all subscribers are immediately notified of a new message. A message is saved as a 'Value'
-- The underlying 'ToJSON' instance is used to construct this 'Value'.
--
-- We recommend documenting a public API for a JSON object when distributing 'Component'
-- downstream to end users for consumption (be it inside a single cabal project or across multiple
-- cabal projects).
--
-- @
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
--           publish "command" Increment
--         SubtractOne ->
--           publish "command" Decrement
--
-- @
--
publish :: ToJSON value => TopicName value -> value -> Effect model action
publish (TopicName topicName) value = io_ $ do
  result <- M.lookup topicName <$> liftIO (readIORef topics)
  case result of
    Just topic -> do
      liftIO $ writeTopic topic (toJSON value)
    Nothing -> do
      -- TODO this shouldn't happen since topics are never unregistered
      -- i.e. `topics` only ever grows (is this a good thing?)
      -- see also `subscribe`
      FFI.consoleLog "topic doesn't exist..."
mkTopic :: MisoString -> IO (TopicName a)
mkTopic topicName = do
  topic <- newTopic
  -- TODO what if the name already exists?
  -- actually, we should really just generate a unique ID instead, like we do for components
  void $ atomicModifyIORef' topics $ \m -> (M.insert topicName topic m, ())
  pure $ TopicName topicName
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
type ComponentId = MisoString
-----------------------------------------------------------------------------
freshComponentId :: IO ComponentId
freshComponentId = do
  x <- atomicModifyIORef' componentIds $ \y -> (y + 1, y)
  pure ("miso-component-id-" <> ms x)
-----------------------------------------------------------------------------
-- | componentMap
--
-- This is a global @Component@ @Map@ that holds the state of all currently
-- mounted @Component@s
components :: IORef (Map MisoString (ComponentState model action))
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
  :: (action -> Effect model action)
  -> Synchronicity
  -> MisoString
  -> Sink action
  -> [action]
  -> model
  -> JSM model
foldEffects _ _ _ _ [] m = pure m
foldEffects update synchronicity name snk (e:es) o =
  case runEffect (update e) name o of
    (n, subs) -> do
      forM_ subs $ \sub -> do
        syncWith synchronicity $
          sub snk `catch` (void . exception)
      foldEffects update synchronicity name snk es n
--------------------------------------------------
-- | Internally used for runView and startComponent
-- Initial draw helper
-- If hydrateing, bypass diff and continue copying
drawComponent
  :: Hydrate
  -> MisoString
  -> Component model action
  -> Sink action
  -> JSM (MisoString, JSVal, IORef VTree)
drawComponent hydrate name Component {..} snk = do
  vtree <- runView hydrate (view model) snk logLevel events
  mountElement <- FFI.getComponent name
  when (hydrate == Draw) (diff Nothing (Just vtree) mountElement)
  ref <- liftIO (newIORef vtree)
  pure (name, mountElement, ref)
-----------------------------------------------------------------------------
-- | Drains the event queue before unmounting, executed synchronously
drain
  :: Component model action
  -> ComponentState model action
  -> JSM ()
drain app@Component{..} cs@ComponentState {..} = do
  actions <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
  if S.null actions then pure () else go actions
    where
      go as = do
        x <- liftIO (readIORef componentModel)
        y <- foldEffects update Sync componentId componentSink (toList as) x
        liftIO (atomicWriteIORef componentModel y)
        drain app cs
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a @Component@
unmount
  :: Function
  -> Component model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback app@Component {..} cs@ComponentState {..} = do
  undelegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO (mapM_ killThread =<< readIORef componentSubThreads)
  killSubscribers componentId
  drain app cs
  liftIO $ atomicModifyIORef' components $ \m -> (M.delete componentId m, ())
-----------------------------------------------------------------------------
killSubscribers :: ComponentId -> JSM ()
killSubscribers componentId = do
  topics_ <- M.keys <$> liftIO (readIORef topics)
  forM_ topics_ (\topic -> unsubscribe_ (TopicName topic) componentId)
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
  -> View action
  -> Sink action
  -> LogLevel
  -> Events
  -> JSM VTree
runView hydrate (VComp attrs (SomeComponent app)) snk _ _ = do
  compName <- liftIO freshComponentId
  mountCallback <- do
    FFI.syncCallback1 $ \continuation -> do
      vtreeRef <- initialize app (drawComponent hydrate compName app)
      VTree vtree <- liftIO (readIORef vtreeRef)
      void $ call continuation global [vtree]
  unmountCallback <- toJSVal =<< do
    FFI.syncCallback $ do
      M.lookup compName <$> liftIO (readIORef components) >>= \case
        Nothing -> pure ()
        Just componentState ->
          unmount mountCallback app componentState
  vcomp <- createNode "vcomp" HTML "div"
  setAttrs vcomp attrs snk (logLevel app) (events app)
  flip (FFI.set "children") vcomp =<< toJSVal ([] :: [MisoString])
  FFI.set "data-component-id" compName vcomp
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
    [parent] ->
      runView hydrate parent snk logLevel events
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
    Event attr -> attr snk vnode logLevel events
    Styles styles -> do
      cssObj <- getProp "css" vnode
      forM_ (M.toList styles) $ \(k,v) -> do
        FFI.set k v (Object cssObj)
-----------------------------------------------------------------------------
-- | Used to support RawText, inlining of HTML.
-- Filters tree to only branches and leaves w/ Text tags.
-- converts to View a. Note: if HTML is malformed,
-- (e.g. closing tags and opening tags are present) they will
-- be removed.
parseView :: MisoString -> [View a]
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
  $ M.insert (componentId componentState) componentState
-----------------------------------------------------------------------------
-- | Registers components in the global state
renderStyles :: [CSS] -> JSM ()
renderStyles styles =
  forM_ styles $ \case
    Href url -> FFI.addStyleSheet url
    Style css -> FFI.addStyle css
    Sheet sheet -> FFI.addStyle (renderStyleSheet sheet)
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
startSub :: ToMisoString subKey => subKey -> Sub action -> Effect model action
startSub subKey sub = do
  compName <- ask
  io_
    (M.lookup compName <$> liftIO (readIORef components) >>= \case
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
              _ -> pure ())
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
stopSub :: ToMisoString subKey => subKey -> Effect model action
stopSub subKey = do
  compName <- ask
  io_
    (M.lookup compName <$> liftIO (readIORef components) >>= \case
      Nothing -> do
        pure ()
      Just ComponentState {..} -> do
        mtid <- liftIO (M.lookup (ms subKey) <$> readIORef componentSubThreads)
        forM_ mtid $ \tid ->
          liftIO $ do
            atomicModifyIORef' componentSubThreads $ \m -> (M.delete (ms subKey) m, ())
            killThread tid)
-----------------------------------------------------------------------------
