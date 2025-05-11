-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
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
  , componentMap
  , notify
  , notify'
  , runView
  , sample
  , sample'
  , renderStyles
  , Prerender(..)
  -- * Subscription
  , startSub
  , stopSub
  ) where
-----------------------------------------------------------------------------
import           Control.Exception (throwIO)
import           Control.Monad (forM, forM_, when, void)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import           Data.Foldable (toList)
import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, atomicWriteIORef, modifyIORef')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import qualified JavaScript.Array as JSArray
import           Language.Javascript.JSaddle hiding (Sync)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           GHC.Conc (ThreadStatus(ThreadDied, ThreadFinished), ThreadId, killThread, threadStatus)
import           Data.Proxy (Proxy(..))
import           Prelude hiding (null)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))
-----------------------------------------------------------------------------
import           Miso.Concurrent (Waiter(..), waiter)
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff (diff)
import           Miso.Exception (MisoException(..), exception)
import qualified Miso.FFI.Internal as FFI
import           Miso.Html hiding (on)
import           Miso.String hiding (reverse)
import           Miso.Types
import           Miso.Event (Events)
import           Miso.Effect (Sub, SubName, Sink, Effect, runEffect, io_)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of @Component@ between top-level API functions.
initialize
  :: Eq model
  => Component name model action
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
  (componentName, componentMount, componentVTree) <- getView componentSink
  componentSubThreads <- liftIO (newIORef M.empty)
  forM_ subs $ \sub -> do
    threadId <- FFI.forkJSM (sub componentSink)
    subName <- liftIO freshSubId
    liftIO $ atomicModifyIORef' componentSubThreads $ \m ->
      (M.insert subName threadId m, ())
  componentModel <- liftIO (newIORef model)
  let
    eventLoop !oldModel = liftIO wait >> do
      as <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
      newModel <- foldEffects update Async componentName componentSink (toList as) oldModel
      oldName <- liftIO $ oldModel `seq` makeStableName oldModel
      newName <- liftIO $ newModel `seq` makeStableName newModel
      when (oldName /= newName && oldModel /= newModel) $ do
        newVTree <- runView DontPrerender (view newModel) componentSink logLevel events
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
-- | Prerender avoids calling @diff@
-- and instead calls @hydrate@
data Prerender
  = DontPrerender
  | Prerender
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | @Component@ state, data associated with the lifetime of a @Component@
data ComponentState model action
  = ComponentState
  { componentName       :: MisoString
  , componentSubThreads :: IORef (Map MisoString ThreadId)
  , componentMount      :: JSVal
  , componentVTree      :: IORef VTree
  , componentSink       :: action -> JSM ()
  , componentModel      :: IORef model
  , componentActions    :: IORef (Seq action)
  }
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
freshComponentId :: IO MisoString
freshComponentId = do
  x <- atomicModifyIORef' componentIds $ \y -> (y + 1, y)
  pure ("miso-component-id-" <> ms x)
-----------------------------------------------------------------------------
-- | componentMap
--
-- This is a global @Component@ @Map@ that holds the state of all currently
-- mounted @Component@s
componentMap :: IORef (Map MisoString (ComponentState model action))
{-# NOINLINE componentMap #-}
componentMap = unsafePerformIO (newIORef mempty)
-----------------------------------------------------------------------------
-- | Read-only access to another @Component@'s @model@.
-- This function is safe to use when a child @Component@ wishes access
-- a parent @Components@ @model@ state. Under this circumstance the parent
-- will always be mounted and available.
--
-- Otherwise, if a sibling or parent @Component@'s @model@ state is attempted
-- to be accessed. Then we throw a @NotMountedException@, in the case the
-- @Component@ being accessed is not available.
sample
  :: forall name model action . KnownSymbol name
  => Component name model action
  -> JSM model
sample _ = do
  componentStateMap <- liftIO (readIORef componentMap)
  liftIO (case M.lookup name componentStateMap of
    Nothing -> throwIO (NotMountedException name)
    Just ComponentState {..} -> readIORef componentModel)
  where
    name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
-- | Like @sample@ except used for dynamic @Component@ where the component-id
-- has been retrieved via @ask@.
sample'
  :: MisoString
  -> JSM model
sample' name = do
  componentStateMap <- liftIO (readIORef componentMap)
  liftIO (case M.lookup name componentStateMap of
    Nothing -> throwIO (NotMountedException name)
    Just ComponentState {..} -> readIORef componentModel)
-----------------------------------------------------------------------------
-- | Used for bidirectional communication between components.
-- Specify the mounted @Component@ you'd like to target.
--
-- This function is used to send messages to @Component@ that are mounted on
-- other parts of the DOM tree.
--
notify
  :: forall name model action . KnownSymbol name
  => Component name model action
  -> action
  -> JSM ()
notify _ action = do
  componentStateMap <- liftIO (readIORef componentMap)
  forM_ (M.lookup name componentStateMap) $ \ComponentState {..} ->
    componentSink action
  where
    name = ms $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
-- | Like @notify@ except used for dynamic @Component@ where the /component-id/
-- has been retrieved via @ask@.
--
notify'
  :: MisoString
  -> action
  -> JSM ()
notify' name action = do
  componentStateMap <- liftIO (readIORef componentMap)
  forM_ (M.lookup name componentStateMap) $ \ComponentState {..} ->
    componentSink action
-----------------------------------------------------------------------------
-- | Sychronicity
--
-- Data type to indicate if effects should be handled asynchronously
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
-- If prerendering, bypass diff and continue copying
drawComponent
  :: Prerender
  -> MisoString
  -> Component name model action
  -> Sink action
  -> JSM (MisoString, JSVal, IORef VTree)
drawComponent prerender name Component {..} snk = do
  vtree <- runView prerender (view model) snk logLevel events
  mountElement <- FFI.getComponent name
  when (prerender == DontPrerender) (diff Nothing (Just vtree) mountElement)
  ref <- liftIO (newIORef vtree)
  pure (name, mountElement, ref)
-----------------------------------------------------------------------------
-- | Drains the event queue before unmounting, executed synchronously
drain
  :: Component name model action
  -> ComponentState model action
  -> JSM ()
drain app@Component{..} cs@ComponentState {..} = do
  actions <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
  if S.null actions then pure () else go actions
    where
      go as = do
        x <- liftIO (readIORef componentModel)
        y <- foldEffects update Sync componentName componentSink (toList as) x
        liftIO (atomicWriteIORef componentModel y)
        drain app cs
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a @Component@
unmount
  :: Function
  -> Component name model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback app@Component {..} cs@ComponentState {..} = do
  undelegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO (mapM_ killThread =<< readIORef componentSubThreads)
  drain app cs
  liftIO $ atomicModifyIORef' componentMap $ \m -> (M.delete componentName m, ())
-----------------------------------------------------------------------------
-- | Internal function for construction of a Virtual DOM.
--
-- Component mounting should be synchronous.
-- Mounting causes a recursive diffing to occur
-- (creating sub components as detected), setting up
-- infrastructure for each sub-component. During this
-- process we go between the Haskell heap and the JS heap.
runView
  :: Prerender
  -> View action
  -> Sink action
  -> LogLevel
  -> Events
  -> JSM VTree
runView prerender (VComp name attributes key (SomeComponent app)) snk _ _ = do
  compName <-
    if null name
    then liftIO freshComponentId
    else pure name
  mountCallback <- do
    FFI.syncCallback1 $ \continuation -> do
      vtreeRef <- initialize app (drawComponent prerender compName app)
      VTree vtree <- liftIO (readIORef vtreeRef)
      void $ call continuation global [vtree]
  unmountCallback <- toJSVal =<< do
    FFI.syncCallback $ do
      M.lookup compName <$> liftIO (readIORef componentMap) >>= \case
        Nothing -> pure ()
        Just componentState ->
          unmount mountCallback app componentState
  vcomp <- createNode "vcomp" HTML key "div"
  setAttrs vcomp attributes snk (logLevel app) (events app)
  flip (FFI.set "children") vcomp =<< toJSVal ([] :: [MisoString])
  FFI.set "data-component-id" compName vcomp
  flip (FFI.set "mount") vcomp =<< toJSVal mountCallback
  FFI.set "unmount" unmountCallback vcomp
  pure (VTree vcomp)
runView prerender (VNode ns tag key attrs kids) snk logLevel events = do
  vnode <- createNode "vnode" ns key tag
  setAttrs vnode attrs snk logLevel events
  vchildren <- ghcjsPure . jsval =<< procreate
  FFI.set "children" vchildren vnode
  sync <- FFI.shouldSync =<< toJSVal vnode
  FFI.set "shouldSync" sync vnode
  pure $ VTree vnode
    where
      procreate = do
        kidsViews <- forM kids $ \kid -> do
          VTree (Object vtree) <- runView prerender kid snk logLevel events
          pure vtree
        ghcjsPure (JSArray.fromList kidsViews)
runView _ (VText t) _ _ _ = do
  vtree <- create
  FFI.set "type" ("vtext" :: JSString) vtree
  FFI.set "ns" ("text" :: JSString) vtree
  FFI.set "text" t vtree
  pure $ VTree vtree
runView prerender (VTextRaw str) snk logLevel events =
  case parseView str of
    [] ->
      runView prerender (VText (" " :: MisoString)) snk logLevel events
    [parent] ->
      runView prerender parent snk logLevel events
    kids -> do
      runView prerender (VNode HTML "div" Nothing mempty kids) snk logLevel events
-----------------------------------------------------------------------------
-- | @createNode@
-- A helper function for constructing a vtree (used for 'vcomp' and 'vnode')
-- Doesn't handle children
createNode :: MisoString -> NS -> Maybe Key -> MisoString -> JSM Object
createNode typ ns key tag = do
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
  FFI.set "key" key vnode
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
        attrs' = [ Property key $ A.String (fromMisoString value)
                 | (key, value) <- attrs
                 ]
        newNode =
          VNode HTML name Nothing attrs' (reverse (go kids []))
      in
        go next (newNode:views)
    go (TagLeaf _ : next) views =
      go next views
-----------------------------------------------------------------------------
-- | Registers components in the global state
registerComponent :: MonadIO m => ComponentState model action -> m ()
registerComponent componentState = liftIO
  $ modifyIORef' componentMap
  $ M.insert (componentName componentState) componentState
-----------------------------------------------------------------------------
-- | Registers components in the global state
renderStyles :: [CSS] -> JSM ()
renderStyles styles =
  forM_ styles $ \case
    Href url -> FFI.addStyleSheet url
    Style css -> FFI.addStyle css
-----------------------------------------------------------------------------
-- | Starts a named 'Sub' dynamically, during the life of a 'Component'.
-- The 'Sub' can be stopped by calling @stop subName@ from the 'update' function.
-- All 'Sub' started will be stopped if a 'Component' is unmounted.
--
startSub :: SubName -> Sub action -> Effect action model
startSub subName sub = do
  compName <- ask
  io_
    (M.lookup compName <$> liftIO (readIORef componentMap) >>= \case
      Nothing -> pure ()
      Just compState@ComponentState {..} -> do
        mtid <- liftIO (M.lookup subName <$> readIORef componentSubThreads)
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
        (M.insert subName tid m, ())
-----------------------------------------------------------------------------
-- | Stops a named 'Sub' dynamically, during the life of a 'Component'.
-- All 'Sub' started will be stopped automatically if a 'Component' is unmounted.
--
stopSub :: SubName -> Effect action model
stopSub subName = do
  compName <- ask
  io_
    (M.lookup compName <$> liftIO (readIORef componentMap) >>= \case
      Nothing -> do
        pure ()
      Just ComponentState {..} -> do
        mtid <- liftIO (M.lookup subName <$> readIORef componentSubThreads)
        forM_ mtid $ \tid -> do
          liftIO $ do
            atomicModifyIORef' componentSubThreads $ \m -> (M.delete subName m, ())
            killThread tid)
-----------------------------------------------------------------------------
