{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}

module Miso.Internal
  ( initialize
  , componentMap
  , drawComponent
  , sink
  , sinkRaw
  , mail
  , notify
  , runView
  , registerSink
  , Prerender(..)
  )
where

import           Control.Concurrent
import           Control.Monad (forM, forM_, (<=<), when, forever, void)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import           Data.Foldable (toList)
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified JavaScript.Array as JSArray
import           Language.Javascript.JSaddle
import           Prelude hiding (null)
import           System.IO.Unsafe
import           System.Mem.StableName
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))

import           Miso.Concurrent
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff
import           Miso.Effect
import           Miso.FFI
import           Miso.Html hiding (on)
import           Miso.String hiding (reverse)
import           Miso.Types hiding (componentName)

-- | Helper function to abstract out initialize functionality between `startApp` and `miso`
initialize
  :: Eq model
  => App model action
  -> (Sink action -> JSM (MisoString, JSVal, IORef VTree))
  -- ^ Callback function is used to perform the creation of VTree
  -> JSM (IORef VTree)
initialize App {..} getView = do
  -- init Waiter
  Waiter {..} <- liftIO waiter
  -- init empty actions
  actions <- liftIO (newIORef S.empty)
  let
    eventSink = \a -> liftIO $ do
      atomicModifyIORef' actions $ \as -> (as S.|> a, ())
      serve

  -- init Subs, save threads for destruction later
  subThreads <- forM subs $ \sub -> forkJSM (sub eventSink)
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . liftIO . forkIO . forever $ threadDelay (1000000 * 86400) >> serve
  -- Retrieves reference view
  (mount, mountEl, viewRef) <- getView eventSink
  -- Program loop, blocking on SkipChan
  let
    loop !oldModel = liftIO wait >> do
        -- Apply actions to model
        as <- liftIO $ atomicModifyIORef' actions $ \as -> (S.empty, as)
        newModel <- foldEffects update eventSink (toList as) oldModel
        oldName <- liftIO $ oldModel `seq` makeStableName oldModel
        newName <- liftIO $ newModel `seq` makeStableName newModel
        when (oldName /= newName && oldModel /= newModel) $ do
          swapCallbacks
          newVTree <- runView DontPrerender (view newModel) eventSink
          oldVTree <- liftIO (readIORef viewRef)
          void waitForAnimationFrame
          diff mountEl (Just oldVTree) (Just newVTree)
          releaseCallbacks
          liftIO (atomicWriteIORef viewRef newVTree)
        syncPoint
        loop newModel

  tid <- forkJSM (loop model)

  liftIO $ do
     registerSink (ComponentState mount tid subThreads mountEl viewRef eventSink)
     eventSink initialAction

  delegator mountEl viewRef events
  pure viewRef

-- | Prerender avoids calling @diff@
-- and instead calls @copyDOMIntoVTree@
data Prerender
  = DontPrerender
  | Prerender
  deriving (Show, Eq)

data ComponentState action
  = ComponentState
  { componentName :: MisoString
  , componentMainThread :: ThreadId
  , componentSubThreads :: [ThreadId]
  , componentMount :: JSVal
  , componentVTree :: IORef VTree
  , componentSink :: action -> IO ()
  }

componentMap :: IORef (Map MisoString (ComponentState action))
{-# NOINLINE componentMap #-}
componentMap = unsafePerformIO (newIORef mempty)

-- | Like @mail@ but lifted to work with the @Transition@ interface.
-- This function is used to send messages to @Component@s on other parts of the application
notify
  :: Component name m a
  -> a
  -> Transition action model ()
notify (Component name _) action = scheduleIO_ $ void (liftIO io)
  where
    io = do
      componentStateMap <- liftIO (readIORef componentMap)
      forM (M.lookup name componentStateMap) $ \ComponentState {..} ->
        componentSink action

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
      forM_ effects $ \effect -> forkJSM (effect snk)
      foldEffects update snk es n

-- | Component sink exposed as a backdoor
-- Meant for usage in long running IO actions, or custom callbacks
-- Good for integrating with third-party components.
-- 'sink' is now extended to take an reference to an App
-- `sink` can now write to any component that is mounted.
-- dmj: As of miso 2.0 sink is parameterized by App
-- and uses the global component map for dispatch to the
-- appropriate sub component sink.
-- Warning:
-- If the component is not mounted it does not exist
-- in the global component map and it will be a no-op
-- this is a backdoor function so it comes with warnings
sink :: MisoString -> App action model -> Sink action
sink name _ = \a ->
  M.lookup name <$> readIORef componentMap >>= \case
    Just ComponentState {..} -> componentSink a
    Nothing -> pure ()

-- | Link 'sink' but without `App` argument
sinkRaw :: MisoString -> Sink action
sinkRaw name = sink name undefined

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
mail (Component name _) action = liftIO $ do
  dispatch <- liftIO (readIORef componentMap)
  forM_ (M.lookup name dispatch) $ \ComponentState {..} ->
    componentSink action

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
  vtree <- runView prerender (view model) snk
  el <- getComponent name
  when (prerender == DontPrerender) $
    diff el Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  pure (name, el, ref)

-- | Helper function for cleanly destroying a @Component@
unmount
  :: Function
  -> App model action
  -> ComponentState action
  -> JSM ()
unmount mountCallback app ComponentState {..} = do
  undelegator componentMount componentVTree (events app)
  freeFunction mountCallback
  liftIO $ do
    killThread componentMainThread
    mapM_ killThread componentSubThreads
    modifyIORef' componentMap (M.delete componentName)

runView
  :: Prerender
  -> View action
  -> Sink action
  -> JSM VTree
runView prerender (Embed (SomeComponent (Component name app)) (ComponentOptions {..})) snk = do
  let mount = name
  -- By default components should be mounted sychronously.
  -- We also include async mounting for tools like jsaddle-warp.
  -- mounting causes a recursive diff to occur, creating subcomponents
  -- and setting up infrastructure for each sub-component. During this
  -- process we go between the haskell heap and the js heap.
  -- It's important that things remain synchronous during this process.
  mountCb <- do
    syncCallback1 $ \continuation -> do
      forM_ onMounted $ \m -> liftIO $ snk m
      vtreeRef <- initialize app (drawComponent prerender mount app)
      VTree vtree <- liftIO (readIORef vtreeRef)
      void $ call continuation global [vtree]

  unmountCb <- toJSVal =<< do
    syncCallback1 $ \_ -> do
      forM_ onUnmounted $ \m ->
        liftIO $ snk m

      M.lookup mount <$> liftIO (readIORef componentMap) >>= \case
        Nothing -> pure ()
        Just componentState -> unmount mountCb app componentState

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
  setAttrs vcomp attributes snk
  flip (set "children") vcomp =<< toJSVal ([] :: [MisoString])
  set "data-component-id" mount vcomp
  flip (set "mount") vcomp =<< toJSVal mountCb
  set "unmount" unmountCb vcomp
  pure (VTree vcomp)
runView prerender (Node ns tag key attrs kids) snk = do
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
  setAttrs vnode attrs snk
  flip (set "children") vnode
    =<< ghcjsPure . jsval
    =<< setKids
  pure $ VTree vnode
    where
      setKids = do
        kidsViews <- traverse (toJSVal . getTree <=< flip (runView prerender) snk) kids
        ghcjsPure (JSArray.fromList kidsViews)
runView _ (Text t) _ = do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree
runView prerender (TextRaw str) snk =
  case parseView str of
    [] ->
      runView prerender (Text (" " :: MisoString)) snk
    [parent] ->
      runView prerender parent snk
    kids -> do
      runView prerender (Node HTML "div" Nothing mempty kids) snk

setAttrs :: Object -> [Attribute action] -> Sink action -> JSM ()
setAttrs vnode attrs snk =
  forM_ attrs $ \case
    P k v -> do
      value <- toJSVal v
      o <- getProp "props" vnode
      set k value (Object o)
    E attr -> attr snk vnode
    S m -> do
      cssObj <- getProp "css" vnode
      forM_ (M.toList m) $ \(k,v) -> do
        set k v (Object cssObj)

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
        attrs' = [ P key $ A.String (fromMisoString value)
                 | (key, value) <- attrs
                 ]
        newNode =
          Node HTML name Nothing attrs' (reverse (go kids []))
      in
        go next (newNode:views)
    go (TagLeaf _ : next) views =
      go next views

registerSink :: MonadIO m => ComponentState action -> m ()
registerSink componentState = liftIO
  $ modifyIORef' componentMap
  $ M.insert (componentName componentState) componentState
