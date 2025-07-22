-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE BangPatterns               #-}
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
  , getComponentId
  , getParentComponentId
  , ComponentState
  -- * Mailbox
  , mail
  ) where
-----------------------------------------------------------------------------
import           Control.Exception (SomeException)
import           Control.Monad (forM, forM_, when, void, forever)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON, ToJSON, Result, fromJSON, toJSON)
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
import           Miso.Concurrent (Waiter(..), waiter, Mailbox, copyMailbox, readMail, sendMail, newMailbox)
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff (diff)
import qualified Miso.FFI.Internal as FFI
import           Miso.String hiding (reverse)
import           Miso.Types
import           Miso.Style (renderStyleSheet)
import           Miso.Event (Events)
import           Miso.Property (textProp)
import           Miso.Effect (Sub, Sink, Effect, runEffect, io_, withSink)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of @Component@ between top-level API functions.
initialize
  :: Eq model
  => Component model action
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
  (componentScripts, componentMount, componentVTree) <- getView componentSink
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
      newModel <- foldEffects update Async componentMount componentSink (toList as) oldModel
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
  componentMailbox <- liftIO newMailbox
  componentMailboxThreadId <- do
    FFI.forkJSM . forever $ do
      message <- liftIO (readMail =<< copyMailbox componentMailbox)
      mapM_ componentSink (mailbox message)
  let vcomp = ComponentState {..}
  registerComponent vcomp
  delegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  forM_ initialAction componentSink
  pure vcomp
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
  , componentMount           :: JSVal
  , componentVTree           :: IORef VTree
  , componentSink            :: action -> JSM ()
  , componentModel           :: IORef model
  , componentActions         :: IORef (Seq action)
  , componentMailbox         :: Mailbox
  , componentMailboxThreadId :: ThreadId
  , componentScripts         :: [DOMRef]
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
  -> (Result message -> action)
  -> Effect model action
subscribe topicName toAction = do
  domRef <- ask
  io_ $ do
    vcompId <- FFI.getComponentId domRef
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
          message <- liftIO (readMail clonedMailbox)
          componentSink $ toAction (fromJSON message)
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
unsubscribe :: Topic message -> Effect model action
unsubscribe topicName = do
  domRef <- ask
  io_ (unsubscribe_ topicName =<< FFI.getComponentId domRef)
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
publish :: ToJSON message => Topic message -> message -> Effect model action
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
  :: (action -> Effect model action)
  -> Synchronicity
  -> DOMRef
  -> Sink action
  -> [action]
  -> model
  -> JSM model
foldEffects _ _ _ _ [] m = pure m
foldEffects update synchronicity domRef snk (e:es) o =
  case runEffect (update e) domRef o of
    (n, subs) -> do
      forM_ subs $ \sub -> do
        syncWith synchronicity $
          sub snk `catch` (void . exception)
      foldEffects update synchronicity domRef snk es n
  where
    exception :: SomeException -> JSM ()
    exception ex = FFI.consoleError ("[EXCEPTION]: " <> ms ex)
--------------------------------------------------
-- | Internally used for runView and startComponent
-- Initial draw helper
-- If hydrateing, bypass diff and continue copying
drawComponent
  :: Hydrate
  -> DOMRef
  -> Component model action
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
  :: Component model action
  -> ComponentState model action
  -> JSM ()
drain app@Component{..} cs@ComponentState {..} = do
  actions <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
  if S.null actions then pure () else go actions
  unloadScripts cs
      where
        go as = do
          x <- liftIO (readIORef componentModel)
          y <- foldEffects update Sync componentMount componentSink (toList as) x
          liftIO (atomicWriteIORef componentModel y)
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
  -> Component model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback app@Component {..} cs@ComponentState {..} = do
  undelegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO (killThread componentMailboxThreadId)
  liftIO (mapM_ killThread =<< readIORef componentSubThreads)
  killSubscribers componentId
  drain app cs
  liftIO $ atomicModifyIORef' components $ \m -> (IM.delete componentId m, ())
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
  -> View action
  -> Sink action
  -> LogLevel
  -> Events
  -> JSM VTree
runView hydrate (VComp attrs (SomeComponent app)) snk _ _ = do
  mountCallback <- do
    FFI.syncCallback2 $ \domRef continuation -> do
      ComponentState {..} <- initialize app (drawComponent hydrate domRef app)
      vtree <- toJSVal =<< liftIO (readIORef componentVTree)
      vcompId <- toJSVal componentId
      void $ call continuation global [vcompId, vtree]
  unmountCallback <- toJSVal =<< do
    FFI.syncCallback1 $ \domRef -> do
      componentId <- liftJSM (FFI.getComponentId domRef)
      IM.lookup componentId <$> liftIO (readIORef components) >>= \case
        Nothing -> pure ()
        Just componentState ->
          unmount mountCallback app componentState
  vcomp <- createNode "vcomp" HTML "div"
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
    Event callback ->
      callback snk (VTree vnode) logLevel events
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
    Src src -> FFI.addSrc src
    Script script -> FFI.addScript script
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
startSub :: ToMisoString subKey => subKey -> Sub action -> Effect model action
startSub subKey sub = do
  domRef <- ask
  io_ $ do
    vcompId <- FFI.getComponentId domRef
    (IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
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
--
-- @since 1.9.0.0
stopSub :: ToMisoString subKey => subKey -> Effect model action
stopSub subKey = do
  domRef <- ask
  io_ $ do
    vcompId <- FFI.getComponentId domRef
    (IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
      Nothing -> do
        pure ()
      Just ComponentState {..} -> do
        mtid <- liftIO (M.lookup (ms subKey) <$> readIORef componentSubThreads)
        forM_ mtid $ \tid ->
          liftIO $ do
            atomicModifyIORef' componentSubThreads $ \m -> (M.delete (ms subKey) m, ())
            killThread tid)
-----------------------------------------------------------------------------
-- | Used to acquire the 'ComponentId' of the current 'Component'
getComponentId
  :: (ComponentId -> action)
  -> Effect model action 
getComponentId callback = do
  domRef <- ask
  withSink $ \sink -> do
    componentId <- FFI.getComponentId domRef
    sink (callback componentId)
-----------------------------------------------------------------------------
-- | Used to acquire the parent 'ComponentId' of the current 'Component'
-- This is commonly used in situations to send messages or receive messages
-- between components.
getParentComponentId
  :: (ComponentId -> action)
  -- ^ Successful callback (with parent ComponentId)
  -> action
  -- ^ Errorful callback (without ComponentId)
  -> Effect model action 
getParentComponentId successful errorful = do
  domRef <- ask
  withSink $ \sink -> do
    FFI.getParentComponentId domRef >>= \case
      Nothing ->
        sink errorful
      Just parentComponentId ->
        sink (successful parentComponentId)
-----------------------------------------------------------------------------
-- | Send any 'ToJSON message => message' to a 'Component' mailbox, by 'ComponentId'
--
-- @
-- mail componentId ("test message" :: MisoString) :: Effect model action
-- @
-- 
-- @since 1.9.0.0
mail
  :: ToJSON message
  => ComponentId
  -> message
  -> Effect model action
mail vcompId message = io_ $
  IM.lookup vcompId <$> liftIO (readIORef components) >>= \case
    Nothing ->
      -- dmj: TODO add DebugMail here
      pure ()
    Just ComponentState {..} ->
      liftIO $ sendMail componentMailbox (toJSON message)
-----------------------------------------------------------------------------
