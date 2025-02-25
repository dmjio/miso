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
  , sink
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

#ifdef ghcjs_HOST_OS
import           Language.Javascript.JSaddle hiding (obj, val)
import           GHCJS.Foreign.Callback (syncCallback', releaseCallback)
import qualified JavaScript.Object.Internal as OI
#endif

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle (eval, waitForAnimationFrame)
import           Data.FileEmbed
import           Language.Javascript.JSaddle hiding (Success, obj, val)
#endif

import           Miso.Delegate (delegator, undelegator)
import           Miso.Concurrent
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
  -> (Sink action -> JSM (IORef VTree))
  -> JSM (IORef VTree)
common App {..} getView = do
#ifndef ghcjs_HOST_OS
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: JSString)
#endif
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
  viewRef <- getView eventSink
  -- know thy mountElement
  mountEl <- mountElement mountPoint
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
          diff mountPoint (Just oldVTree) (Just newVTree)
          releaseCallbacks
          liftIO (atomicWriteIORef viewRef newVTree)
        syncPoint
        loop newModel

  tid <- forkJSM (loop model)
  liftIO $ modifyIORef' componentMap (M.insert mountPoint (tid, viewRef, eventSink))
  delegator mountEl viewRef events
  pure viewRef

componentMap
  :: forall action
   . IORef (Map MisoString (ThreadId, IORef VTree, action -> IO ()))
{-# NOINLINE componentMap #-}
componentMap = unsafePerformIO (newIORef mempty)

-- | Helper
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
sink :: App action model -> Sink action
sink app = \a -> do
  M.lookup (mountPoint app) <$> readIORef componentMap >>= \case
    Nothing -> pure ()
    Just (_, _, f) -> f a

-- | Used for bidirectional communication between components.
-- Specify the mounted Component's 'App' you'd like to target.
--
-- > AddTodo n :: TodoAction -> do
-- >   notify (calendarApp :: App CalendarModel CalendarAction) (MakeCalendarEntry Now n :: CalendarAction)
-- >   pure (m :: TodoModel)
--
notify
  :: model
  -> App m a
  -> a
  -> Effect action model
notify m app action = Effect m [ \_ -> io ]
  where
    io = liftIO $ do
      dispatch <- liftIO (readIORef componentMap)
      forM_ (M.lookup (mountPoint app) dispatch) $ \(_, _, f) ->
        f action

-- | Internally used for runView and startApp
initApp :: App model action -> Sink action -> JSM (IORef VTree)
initApp App {..} snk = do
  vtree <- runView (view model) snk
  diff mountPoint Nothing (Just vtree)
  liftIO (newIORef vtree)

runView :: View action -> Sink action -> JSM VTree
runView (ComponentNode (Component maybeKey app) mountHooks) snk = do
  vcomp <- create
  let name = mountPoint app

  -- By default components should be mounted sychronously.
  -- We also include async mounting for tools like jsaddle-warp.
  -- mounting causes a recursive diff to occur, creating subcomponents
  -- and setting up infrastructure for each sub-component. During this
  -- process we go between the haskell heap and the js heap.
  -- It's important that things remain synchronous during this process.
#ifdef ghcjs_HOST_OS
  mountCb <-
        syncCallback' $ do
          forM_ mountHooks $ \(Mount m _) -> snk m
          vtreeRef <- common app (initApp app)
          VTree (OI.Object jval) <- liftIO (readIORef vtreeRef)
          consoleLog "Sync component mounting enabled"
          pure jval
#else
  mountCb <- do
    asyncCallback1 $ \continuation -> do
      forM_ mountHooks $ \(Mount m _) -> liftIO $ snk m
      vtreeRef <- common app (initApp app)
      VTree vtree <- liftIO (readIORef vtreeRef)
      consoleLog "Async component mounting enabled"
      void $ call continuation global [vtree]
#endif

  unmountCb <- toJSVal =<< do
    syncCallback $ do
      forM_ mountHooks $ \(Mount _ u) -> liftIO $ snk u
      M.lookup name <$> liftIO (readIORef componentMap) >>= \case
        Nothing ->
          pure ()
        Just (tid, ref, _) -> do
          mount <- mountElement (mountPoint app)
          undelegator mount ref (events app)
#ifdef ghcjs_HOST_OS
          releaseCallback mountCb
#else
          freeFunction mountCb
#endif

          liftIO $ do
            killThread tid
            modifyIORef' componentMap (M.delete name)
  set "type" ("vcomp" :: JSString) vcomp
  set "tag" ("div" :: JSString) vcomp
  forM_ maybeKey $ \(Key key) -> set "key" key vcomp
  propsObj <- create
  set "props" propsObj vcomp
  eventsObj <- create
  set "events" eventsObj vcomp
  set "id" name vcomp
  flip (set "children") vcomp =<< toJSVal ([] :: [MisoString])
#ifdef ghcjs_HOST_OS
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
  setAttrs vnode
  flip (set "children") vnode
    =<< ghcjsPure . jsval
    =<< setKids
  pure $ VTree vnode
    where
      setAttrs vnode =
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

registerSink :: MonadIO m => App model action -> IORef VTree -> Sink action -> m ()
registerSink App {..} vtree snk = liftIO $ do
  tid <- myThreadId
  liftIO $ modifyIORef' componentMap (M.insert mountPoint (tid, vtree, snk))
