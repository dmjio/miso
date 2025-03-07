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
  ( common
  , componentMap
  , initComponent
  , sink
  , sinkRaw
  , mail
  , notify
  , runView
  , registerSink
  )
where

import           Control.Concurrent
import           Control.Monad (forM_, (<=<), when, forever, void)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import           Data.Foldable (toList)
import           Data.IORef
import           Data.JSString (JSString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           GHCJS.Marshal (toJSVal)
import           GHCJS.Types (jsval)
import qualified JavaScript.Array as JSArray
import           JavaScript.Object (create, getProp)
import           JavaScript.Object.Internal (Object(Object))
import           Prelude hiding (null)
import           System.IO.Unsafe
import           System.Mem.StableName
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))

#ifdef GHCJS_OLD
import           Language.Javascript.JSaddle hiding (obj, val)
import           GHCJS.Foreign.Callback (syncCallback', releaseCallback)
import qualified JavaScript.Object.Internal as OI
#endif

#ifdef GHCJS_NEW
import           GHC.JS.Foreign.Callback (syncCallback', releaseCallback)
#endif

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle (waitForAnimationFrame)
import           Language.Javascript.JSaddle hiding (Success, obj, val)
#endif

import           Miso.Concurrent
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff
import           Miso.Effect
import           Miso.FFI
import           Miso.Html
import           Miso.String hiding (reverse)
import           Miso.Types

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => App model action
  -> (Sink action -> JSM (MisoString, JSVal, IORef VTree))
  -> JSM (IORef VTree)
common App {..} getView = do
  -- init Waiter
  Waiter {..} <- liftIO waiter
  -- init empty actions
  actions <- liftIO (newIORef S.empty)
  let
    eventSink = \a -> void . liftIO . forkIO $ do
        atomicModifyIORef' actions $ \as -> (as S.|> a, ())
        serve

  -- init Subs
  forM_ subs $ \sub -> sub eventSink
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . liftIO . forkIO . forever $ threadDelay (1000000 * 86400) >> serve
  -- Retrieves reference view
  (mount, mountEl, viewRef) <- getView eventSink
  -- Process initial action of application
  liftIO (eventSink initialAction)
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
          newVTree <- runView (view newModel) eventSink
          oldVTree <- liftIO (readIORef viewRef)
          void $ waitForAnimationFrame
          diff mountEl (Just oldVTree) (Just newVTree)
          releaseCallbacks
          liftIO (atomicWriteIORef viewRef newVTree)
        syncPoint
        loop newModel

  tid <- forkJSM (loop model)
  addToComponentMap mount tid viewRef eventSink
  delegator mountEl viewRef events
  pure viewRef

addToComponentMap
  :: MonadIO m
  => MisoString
  -> ThreadId
  -> IORef VTree
  -> (action -> IO ())
  -> m ()
addToComponentMap mount thread view snk =
  liftIO $ modifyIORef' componentMap (M.insert mount (thread, view, snk))

componentMap
  :: forall action
   . IORef (Map MisoString (ThreadId, IORef VTree, action -> IO ()))
{-# NOINLINE componentMap #-}
componentMap = unsafePerformIO (newIORef mempty)

-- | Like @mail@ but lifted to work with the @Transition@ interface.
-- This function is used to send messages to @Component@s on other parts of the application
notify
  :: Component name m a
  -> a
  -> Transition action model ()
notify (Component name _) action = scheduleIO_ (liftIO io)
  where
    io = do
      dispatch <- liftIO (readIORef componentMap)
      forM_ (M.lookup name dispatch) $ \(_, _, f) ->
        f action

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
sink name _ = \a -> do
  M.lookup name <$> readIORef componentMap >>= \case
    Nothing -> pure ()
    Just (_, _, f) -> f a

-- | Link 'sink' but without `App` argument
sinkRaw :: MisoString -> Sink action
sinkRaw name = \x -> do
  M.lookup name <$> readIORef componentMap >>= \case
    Nothing -> pure ()
    Just (_, _, f) -> f x

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
  forM_ (M.lookup name dispatch) $ \(_, _, f) ->
    f action

-- | Internally used for runView and startApp
initComponent
  :: MisoString
  -> App model action
  -> Sink action
  -> JSM (MisoString, JSVal, IORef VTree)
initComponent name App {..} snk = do
  vtree <- runView (view model) snk
  el <- getComponent name
  diff el Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  pure (name, el, ref)

runView :: View action -> Sink action -> JSM VTree
runView (Embed (SomeComponent (Component name app)) (ComponentOptions {..})) snk = do
  vcomp <- create
  let mount = name

  -- By default components should be mounted sychronously.
  -- We also include async mounting for tools like jsaddle-warp.
  -- mounting causes a recursive diff to occur, creating subcomponents
  -- and setting up infrastructure for each sub-component. During this
  -- process we go between the haskell heap and the js heap.
  -- It's important that things remain synchronous during this process.
#ifdef GHCJS_BOTH
  mountCb <-
    syncCallback' $ do
      forM_ onMounted $ \m -> liftIO $ snk m
      vtreeRef <- common app (initComponent mount app)
      VTree (OI.Object jval) <- liftIO (readIORef vtreeRef)
      -- consoleLog "Sync component mounting enabled" (dmj: enable logging)
      pure jval
#else
  mountCb <- do
    syncCallback1 $ \continuation -> do
      forM_ onMounted $ \m -> liftIO $ snk m
      vtreeRef <- common app (initComponent mount app)
      VTree vtree <- liftIO (readIORef vtreeRef)
      void $ call continuation global [vtree]
#endif

  unmountCb <- toJSVal =<< do
    syncCallback1 $ \mountEl -> do
      forM_ onUnmounted $ \m -> liftIO $ snk m
      M.lookup mount <$> liftIO (readIORef componentMap) >>= \case
        Nothing ->
          pure ()
        Just (tid, ref, _) -> do
          undelegator mountEl ref (events app)
#ifdef GHCJS_BOTH
          releaseCallback mountCb
#else
          freeFunction mountCb
#endif
          liftIO $ do
            killThread tid
            modifyIORef' componentMap (M.delete mount)
  set "type" ("vcomp" :: JSString) vcomp
  set "tag" ("div" :: JSString) vcomp
  forM_ componentKey $ \(Key key) -> set "key" key vcomp
  propsObj <- create
  set "props" propsObj vcomp
  eventsObj <- create
  setAttrs vcomp attributes snk
  set "events" eventsObj vcomp
  set "ns" HTML vcomp
  set "data-component-id" mount vcomp
  flip (set "children") vcomp =<< toJSVal ([] :: [MisoString])
#ifdef GHCJS_BOTH
  set "mount" (jsval mountCb) vcomp
#else
  flip (set "mount") vcomp =<< toJSVal mountCb
#endif
  set "unmount" unmountCb vcomp
  pure (VTree vcomp)
runView (Node ns tag key attrs kids) snk = do
  vnode <- create
  cssObj <- objectToJSVal =<< create
  propsObj <- objectToJSVal =<< create
  eventObj <- objectToJSVal =<< create
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
        kidsViews <- traverse (objectToJSVal . getTree <=< flip runView snk) kids
        ghcjsPure (JSArray.fromList kidsViews)
runView (Text t) _ = do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree
runView (TextRaw str) snk =
  case parseView str of
    [] ->
      runView (Text (" " :: MisoString)) snk
    [parent] ->
      runView parent snk
    kids -> do
      runView (Node HTML "div" Nothing mempty kids) snk

setAttrs :: Object -> [Attribute action] -> Sink action -> JSM ()
setAttrs vnode attrs snk =
  forM_ attrs $ \case
    P k v -> do
      val <- toJSVal v
      o <- getProp "props" vnode
      set k val (Object o)
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
        attrs' = [ P key $ A.String (fromMisoString val)
                 | (key, val) <- attrs
                 ]
        newNode =
          Node HTML name Nothing attrs' (reverse (go kids []))
      in
        go next (newNode:views)
    go (TagLeaf _ : next) views =
      go next views

registerSink :: MonadIO m => MisoString -> IORef VTree -> Sink action -> m ()
registerSink mount vtree snk = liftIO $ do
  tid <- myThreadId
  liftIO $ modifyIORef' componentMap (M.insert mount (tid, vtree, snk))
