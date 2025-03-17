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
import           Control.Exception (throwIO, SomeException, fromException)
import           Control.Concurrent (ThreadId, killThread)
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
import           Miso.Exception (MisoException(..))
import           Miso.FFI hiding (diff)
import           Miso.Html hiding (on)
import           Miso.String hiding (reverse)
import           Miso.Types hiding (componentName)
import           Miso.Event (Events)
import           Miso.Effect (Sink, Effect(Effect), Transition, scheduleIO_)
-----------------------------------------------------------------------------
-- | Helper function to abstract out initialization of @App@ between top-level API functions.
initialize
  :: Eq model
  => App model action
  -> (Sink action -> JSM (MisoString, JSVal, IORef VTree))
  -- ^ Callback function is used to perform the creation of VTree
  -> JSM (IORef VTree)
initialize App {..} getView = do
  Waiter {..} <- liftIO waiter
  actions <- liftIO (newIORef S.empty)
  let
    eventSink = \a -> liftIO $ do
      atomicModifyIORef' actions $ \as -> (as S.|> a, ())
      serve
  subThreads <- forM subs $ \sub -> forkJSM (sub eventSink)
  (mount, mountEl, viewRef) <- getView eventSink
  modelRef <- liftIO (newIORef model)
  let
    loop !oldModel = liftIO wait >> do
        as <- liftIO $ atomicModifyIORef' actions $ \as -> (S.empty, as)
        newModel <- foldEffects update eventSink (toList as) oldModel
        oldName <- liftIO $ oldModel `seq` makeStableName oldModel
        newName <- liftIO $ newModel `seq` makeStableName newModel
        when (oldName /= newName && oldModel /= newModel) $ do
          newVTree <- runView DontPrerender (view newModel) eventSink events
          oldVTree <- liftIO (readIORef viewRef)
          void waitForAnimationFrame
          diff mountEl (Just oldVTree) (Just newVTree)
          liftIO $ do
            atomicWriteIORef viewRef newVTree
            atomicWriteIORef modelRef newModel
        syncPoint
        loop newModel
  tid <- forkJSM (loop model)
  registerComponent (ComponentState mount tid subThreads mountEl viewRef eventSink modelRef)
  delegator mountEl viewRef events (logLevel `elem` [DebugEvents, DebugAll])
  eventSink initialAction
  pure viewRef
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
  :: Component name model app
  -> JSM model
sample (Component name _) = do
  componentStateMap <- liftIO (readIORef componentMap)
  liftIO $ case M.lookup name componentStateMap of
    Nothing -> throwIO (NotMountedException name)
    Just ComponentState {..} -> readIORef componentModel
-----------------------------------------------------------------------------
-- | Like @mail@ but lifted to work with the @Transition@ interface.
-- This function is used to send messages to @Component@s on other parts of the application
notify
  :: Component name m a
  -> a
  -> Transition action model ()
notify (Component name _) action = scheduleIO_ (void io)
  where
    io = do
      componentStateMap <- liftIO (readIORef componentMap)
      forM (M.lookup name componentStateMap) $ \ComponentState {..} ->
        componentSink action
-----------------------------------------------------------------------------
-- | Helper for processing effects in the event loop.
foldEffects
  :: (action -> model -> Effect action model)
  -> Sink action
  -> [action]
  -> model
  -> JSM model
foldEffects _ _ [] m = pure m
foldEffects update snk (e:es) old =
  case update e old of
    Effect n effects -> do
      forM_ effects $ \effect ->
        effect snk `catch` exception
      foldEffects update snk es n
  where
    exception :: SomeException -> JSM ()
    exception ex
      | Just (NotMountedException name) <- fromException ex =
          consoleError ("NotMountedException: Could not sample model state from the Component " <> name)
      | Just (AlreadyMountedException name) <- fromException ex =
          consoleError ("AlreadytMountedException: Component " <> name <> " is already")
      | otherwise = 
          consoleError ("UnknownException: " <> ms ex)
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
sink :: MisoString -> App action model -> Sink action
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
  :: Component name m a
  -> a
  -> JSM ()
mail (Component name _) action = do
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
  -> App model action
  -> Sink action
  -> JSM (MisoString, JSVal, IORef VTree)
drawComponent prerender name App {..} snk = do
  vtree <- runView prerender (view model) snk events
  el <- getComponent name
  when (prerender == DontPrerender) $
    diff el Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  pure (name, el, ref)
-----------------------------------------------------------------------------
-- | Helper function for cleanly destroying a @Component@
unmount
  :: Function
  -> App model action
  -> ComponentState model action
  -> JSM ()
unmount mountCallback App{..} ComponentState {..} = do
  undelegator componentMount componentVTree events (logLevel `elem` [DebugEvents, DebugAll])
  freeFunction mountCallback
  liftIO $ do
    killThread componentMainThread
    mapM_ killThread componentSubThreads
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
  -> Events
  -> JSM VTree
runView prerender (Embed (SomeComponent (Component name app)) (ComponentOptions {..})) snk _ = do
  let mount = name
  mountCb <- do
    syncCallback1 $ \continuation -> do
      forM_ onMounted snk
      vtreeRef <- initialize app (drawComponent prerender mount app)
      VTree vtree <- liftIO (readIORef vtreeRef)
      void $ call continuation global [vtree]
  unmountCb <- toJSVal =<< do
    syncCallback $ do
      forM_ onUnmounted snk
      M.lookup mount <$> liftIO (readIORef componentMap) >>= \case
        Nothing -> pure ()
        Just componentState ->
          unmount mountCb app componentState
  vcomp <- create
  cssObj <- create
  propsObj <- create
  eventsObj <- create
  set "css" cssObj vcomp
  set "props" propsObj vcomp
  set "events" eventsObj vcomp
  set "type" ("vcomp" :: JSString) vcomp
  set "ns" HTML vcomp
  set "tag" ("div" :: JSString) vcomp
  set "key" componentKey vcomp
  setAttrs vcomp attributes snk (events app)
  flip (set "children") vcomp =<< toJSVal ([] :: [MisoString])
  set "data-component-id" mount vcomp
  flip (set "mount") vcomp =<< toJSVal mountCb
  set "unmount" unmountCb vcomp
  pure (VTree vcomp)
runView prerender (Node ns tag key attrs kids) snk events = do
  vnode <- create
  cssObj <- toJSVal =<< create
  propsObj <- toJSVal =<< create
  eventObj <- toJSVal =<< create
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: JSString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode attrs snk events
  flip (set "children") vnode
    =<< ghcjsPure . jsval
    =<< setKids
  pure $ VTree vnode
    where
      setKids = do
        kidsViews <- forM kids $ \kid -> do
          VTree (Object vtree) <- runView prerender kid snk events
          pure vtree
        ghcjsPure (JSArray.fromList kidsViews)
runView _ (Text t) _ _ = do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree
runView prerender (TextRaw str) snk events =
  case parseView str of
    [] ->
      runView prerender (Text (" " :: MisoString)) snk events
    [parent] ->
      runView prerender parent snk events
    kids -> do
      runView prerender (Node HTML "div" Nothing mempty kids) snk events
-----------------------------------------------------------------------------
-- | Helper function for populating "props" and "css" fields on a virtual
-- DOM node
setAttrs
  :: Object
  -> [Attribute action]
  -> Sink action
  -> Events
  -> JSM ()
setAttrs vnode attrs snk events =
  forM_ attrs $ \case
    Property k v -> do
      value <- toJSVal v
      o <- getProp "props" vnode
      set k value (Object o)
    Event attr -> attr snk vnode events
    Style styles -> do
      cssObj <- getProp "css" vnode
      forM_ (M.toList styles) $ \(k,v) -> do
        set k v (Object cssObj)
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
