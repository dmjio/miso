-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
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
  ( initialize
  , componentMap
  , sink
  , mail
  , notify
  , runView
  , sample
  , Prerender(..)
  ) where
-----------------------------------------------------------------------------
import           Control.Exception (throwIO)
import           Control.Concurrent (ThreadId, killThread, threadDelay)
import           Control.Monad (forM, forM_, when, void)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import           Data.Foldable (toList)
import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, atomicWriteIORef, modifyIORef')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified JavaScript.Array as JSArray
import           Language.Javascript.JSaddle
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
import qualified Miso.FFI as FFI
import           Miso.Html hiding (on)
import           Miso.String hiding (reverse)
import           Miso.Types hiding (componentName)
import           Miso.Event (Events)
import           Miso.Effect (Sink, Effect(Effect), Transition, scheduleIO_)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of @App@ between top-level API functions.
initialize
  :: Eq model
  => App effect model action
  -> (Sink action -> JSM (MisoString, JSVal, IORef VTree))
  -- ^ Callback function is used to perform the creation of VTree
  -> JSM (IORef VTree)
initialize App {..} getView = do
  Waiter {..} <- liftIO waiter
  componentActions <- liftIO (newIORef S.empty)
  let
    componentSink = \action -> liftIO $ do
      atomicModifyIORef' componentActions $ \actions -> (actions S.|> action, ())
      serve
  componentSubThreads <- forM subs $ \sub -> FFI.forkJSM (sub componentSink)
  (componentName, componentMount, componentVTree) <- getView componentSink
  componentModel <- liftIO (newIORef model)
  let
    eventLoop !oldModel = liftIO wait >> do
      as <- liftIO $ atomicModifyIORef' componentActions $ \actions -> (S.empty, actions)
      -- translate :: effect action model -> (model -> Effect action model)
      -- update    :: action -> model -> effect action model
      newModel <- foldEffects translate update componentSink (toList as) oldModel
      oldName <- liftIO $ oldModel `seq` makeStableName oldModel
      newName <- liftIO $ newModel `seq` makeStableName newModel
      when (oldName /= newName && oldModel /= newModel) $ do
        newVTree <- runView DontPrerender (view newModel) componentSink logLevel events
        oldVTree <- liftIO (readIORef componentVTree)
        void waitForAnimationFrame
        diff componentMount (Just oldVTree) (Just newVTree)
        liftIO $ do
          atomicWriteIORef componentVTree newVTree
          atomicWriteIORef componentModel newModel
      syncPoint
      eventLoop newModel
  componentMainThread <- FFI.forkJSM (eventLoop model)
  registerComponent ComponentState {..}
  delegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  forM_ initialAction componentSink
  pure componentVTree
-----------------------------------------------------------------------------
-- | Prerender avoids calling @diff@
-- and instead calls @copyDOMIntoVTree@
data Prerender
  = DontPrerender
  | Prerender
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | @Component@ state, data associated with the lifetime of a @Component@
data ComponentState model action
  = ComponentState
  { componentName       :: MisoString
  , componentMainThread :: ThreadId
  , componentSubThreads :: [ThreadId]
  , componentMount      :: JSVal
  , componentVTree      :: IORef VTree
  , componentSink       :: action -> JSM ()
  , componentModel      :: IORef model
  }
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
  :: Component effect model app
  -> JSM model
sample (Component _ name _) = do
  componentStateMap <- liftIO (readIORef componentMap)
  liftIO $ case M.lookup name componentStateMap of
    Nothing -> throwIO (NotMountedException name)
    Just ComponentState {..} -> readIORef componentModel
-----------------------------------------------------------------------------
-- | Like @mail@ but lifted to work with the @Transition@ interface.
-- This function is used to send messages to @Component@s on other parts of the application
notify
  :: Component effect m a
  -> a
  -> Transition action model
notify (Component _ name _) action = scheduleIO_ (void io)
  where
    io = do
      componentStateMap <- liftIO (readIORef componentMap)
      forM (M.lookup name componentStateMap) $ \ComponentState {..} ->
        componentSink action
-----------------------------------------------------------------------------
-- | Helper for processing effects in the event loop.
foldEffects
  :: (effect action model -> (model -> Effect action model))
  -> (action -> model -> effect action model)
  -> Sink action
  -> [action]
  -> model
  -> JSM model
foldEffects _ _ _ [] m = pure m
foldEffects translate update snk (e:es) old = do
  case translate (update e old) old of
    Effect n effects -> do
      forM_ effects $ \effect -> effect snk `catch` (void . exception)
      foldEffects translate update snk es n
-----------------------------------------------------------------------------
-- | The sink function gives access to an @App@
-- @Sink@. This is use for third-party integration, or for
-- long-running @IO@ operations. Use at your own risk.
--
-- If the @Component@ or is not mounted, it does not exist
-- in the global component map, and will therefore be a no-op.
-- This is a backdoor function, caveat emptor.
--
-- It is recommended to use the @mail@ or @notify@ functions by default
-- when message passing with @App@ and @Component@
--
sink :: MisoString -> App effect action model -> Sink action
sink name _ = \a ->
  M.lookup name <$> liftIO (readIORef componentMap) >>= \case
    Just ComponentState {..} -> componentSink a
    Nothing -> pure ()
-----------------------------------------------------------------------------
-- | Used for bidirectional communication between components.
-- Specify the mounted Component's 'App' you'd like to target.
--
-- > update (MakeTodo entry) m -> do
-- >   m <# mail calendarComponent (NewCalendarEntry entry)
--
mail
  :: Component effect m a
  -> a
  -> JSM ()
mail (Component _ name _) action = do
  dispatch <- liftIO (readIORef componentMap)
  forM_ (M.lookup name dispatch) $ \ComponentState {..} ->
    componentSink action
-----------------------------------------------------------------------------
-- | Internally used for runView and startApp
-- Initial draw helper
-- If prerendering bypass diff and continue copying
drawComponent
  :: Prerender
  -> MisoString
  -> App effect model action
  -> Sink action
  -> JSM (MisoString, JSVal, IORef VTree)
drawComponent prerender name App {..} snk = do
  vtree <- runView prerender (view model) snk logLevel events
  mountElement <- FFI.getComponent name
  when (prerender == DontPrerender) $ diff mountElement Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  pure (name, mountElement, ref)
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a @Component@
unmount
  :: Function
  -> App effect model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback App{..} ComponentState {..} = do
  undelegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO (mapM_ killThread componentSubThreads)
  liftIO $ do
    killThread componentMainThread
    modifyIORef' componentMap (M.delete componentName)
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
runView prerender (Embed attributes (SomeComponent (Component key mount app))) snk _ _ = do
  mountCallback <- do
    FFI.syncCallback1 $ \continuation -> do
      vtreeRef <- initialize app (drawComponent prerender mount app) 
      VTree vtree <- liftIO (readIORef vtreeRef)
      void $ call continuation global [vtree]
  unmountCallback <- toJSVal =<< do
    FFI.syncCallback $ do
      M.lookup mount <$> liftIO (readIORef componentMap) >>= \case
        Nothing -> pure ()
        Just componentState -> do
          liftIO (threadDelay (millis 1))
          -- dmj ^ introduce 1ms delay to account for recursive component unmounting
          unmount mountCallback app componentState
  vcomp <- createNode "vcomp" HTML key "div"
  setAttrs vcomp attributes snk (logLevel app) (events app)
  flip (FFI.set "children") vcomp =<< toJSVal ([] :: [MisoString])
  FFI.set "data-component-id" mount vcomp
  flip (FFI.set "mount") vcomp =<< toJSVal mountCallback
  FFI.set "unmount" unmountCallback vcomp
  pure (VTree vcomp)
runView prerender (Node ns tag key attrs kids) snk logLevel events = do
  vnode <- createNode "vnode" ns key tag
  setAttrs vnode attrs snk logLevel events
  flip (FFI.set "children") vnode
    =<< ghcjsPure . jsval
    =<< setKids
  pure $ VTree vnode
    where
      setKids = do
        kidsViews <- forM kids $ \kid -> do
          VTree (Object vtree) <- runView prerender kid snk logLevel events
          pure vtree
        ghcjsPure (JSArray.fromList kidsViews)
runView _ (Text t) _ _ _ = do
  vtree <- create
  FFI.set "type" ("vtext" :: JSString) vtree
  FFI.set "text" t vtree
  pure $ VTree vtree
runView prerender (TextRaw str) snk logLevel events =
  case parseView str of
    [] ->
      runView prerender (Text (" " :: MisoString)) snk logLevel events
    [parent] ->
      runView prerender parent snk logLevel events
    kids -> do
      runView prerender (Node HTML "div" Nothing mempty kids) snk logLevel events
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
    Style styles -> do
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
      go next (Text s : views)
    go (TagLeaf (TagOpen name attrs) : next) views =
      go (TagBranch name attrs [] : next) views
    go (TagBranch name attrs kids : next) views =
      let
        attrs' = [ Property key $ A.String (fromMisoString value)
                 | (key, value) <- attrs
                 ]
        newNode =
          Node HTML name Nothing attrs' (reverse (go kids []))
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
-- | Millisecond helper, converts microseconds to milliseconds
millis :: Int -> Int
millis = (*1000)
-----------------------------------------------------------------------------
