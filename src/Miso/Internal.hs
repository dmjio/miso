{-# LANGUAGE TypeApplications    #-}
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
import           Control.Exception
import           Control.Monad (forM_, (<=<), when, forever, void, forM)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import           Data.Coerce
import           Data.FileEmbed
import           Data.Foldable (toList)
import           Data.Function
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import           Prelude hiding (null)
import           System.IO.Unsafe
import           System.Mem.StableName
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))

import           Miso.FFI
import           Miso.Concurrent
import           Miso.Delegate (delegator, undelegator)
import           Miso.Diff
import           Miso.Effect
import           Miso.Html
import           Miso.String hiding (reverse)
import           Miso.Types

default (MisoString)

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => App model action
  -> (Sink action -> IO (MisoString, JSVal, IORef VTree))
  -> IO (IORef VTree)
common App {..} getView = do
#ifdef WASM
  -- _ <- eval ($(embedStringFile "jsbits/delegate.js") :: MisoString)
  -- _ <- eval ($(embedStringFile "jsbits/diff.js") :: MisoString)
  -- _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: MisoString)
  -- _ <- eval ($(embedStringFile "jsbits/util.js") :: MisoString)
  -- _ <- eval ($(embedStringFile "jsbits/callbacks.js") :: MisoString)
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
          waitForAnimationFrame =<< do
            asyncCallback $ do
              diff mountEl (Just oldVTree) (Just newVTree)
              atomicWriteIORef viewRef newVTree
        loop newModel

  tid <- forkIO (loop model)
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
  -> IO model
foldEffects _ _ [] m = pure m
foldEffects update snk (e:es) old =
  case update e old of
    Effect n effects -> do
      forM_ effects $ \effect -> forkIO (effect snk)
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
  -> IO ()
mail (Component name _) action = liftIO $ do
  dispatch <- liftIO (readIORef componentMap)
  forM_ (M.lookup name dispatch) $ \(_, _, f) ->
    f action

-- | Internally used for runView and startApp
initComponent
  :: MisoString
  -> App model action
  -> Sink action
  -> IO (MisoString, JSVal, IORef VTree)
initComponent name App {..} snk = do
  consoleLog ("IN INIT COMPONENT" :: MisoString)
  vtree <- runView (view model) snk
  consoleLog ("IN RUNVIEW" :: MisoString)
  el <- getComponent name
  consoleLog ("GOT COMPONENT" :: MisoString)
  consoleLog el
  consoleLog ("EXECUTING DIFF" :: MisoString)
  diff el Nothing (Just vtree)
  consoleLog ("DONE DIFFING" :: MisoString)
  ref <- liftIO (newIORef vtree)
  pure (name, el, ref)

runView :: View action -> Sink action -> IO VTree
runView (Embed (SomeComponent (Component name app)) (ComponentOptions {..})) snk = do
  consoleLog ("IN RUN VIEW" :: MisoString)
  vcomp <- newJSObject
  consoleLog ("NEW OBJ" :: MisoString)
  let mount = name

  mountCb <- do
    syncCallback' $ do
      fix $ \loop -> do
        consoleLog ("IN MOUNT HASKELL SIDE" :: MisoString)
        forM_ onMounted $ \m -> snk m
        vtreeRef <-
          (common app (initComponent mount app)) `catch`
            (\(e :: SomeException) -> print e >> putStrLn "woohoo")
        consoleLog ("MOUNTED HASKELL SIDE (done with common)" :: MisoString)
        VTree (JSObject vtree) <- readIORef vtreeRef
        pure vtree

  consoleLog ("DEFINED mountCb" :: MisoString)
  consoleLog mountCb

  unmountCb <- toJSVal =<< do
    syncCallback' $ do
      forM_ onUnmounted $ \m -> liftIO $ snk m
      M.lookup mount <$> liftIO (readIORef componentMap) >>= \case
        Nothing ->
          pure jsNull
        Just (tid, ref, _) -> do
          mountEl <- getComponent mount
          undelegator mountEl ref (events app)
          releaseCallback mountCb
          liftIO $ do
            killThread tid
            modifyIORef' componentMap (M.delete mount)
            pure jsNull

  consoleLog ("DEFINED unmountCb" :: MisoString)
  consoleLog unmountCb

  set "type" ("vcomp" :: MisoString) vcomp
  set "tag" ("div" :: MisoString) vcomp
  forM_ componentKey $ \(Key key) -> set "key" key vcomp
  propsObj <- newJSObject
  set "props" propsObj vcomp
  eventsObj <- newJSObject
  setAttrs vcomp attributes snk
  set "events" eventsObj vcomp
  set "ns" HTML vcomp
  set "data-component-id" mount vcomp
  flip (set "children") vcomp =<< toJSVal ([] :: [MisoString])
  flip (set "mount") vcomp =<< toJSVal mountCb
  set "unmount" unmountCb vcomp

  consoleLog ("RETURNING VTREE" :: MisoString)
  pure (VTree vcomp)
runView (Node ns tag key attrs kids) snk = do
  vnode <- newJSObject
  JSObject cssObj   <- newJSObject
  JSObject propsObj <- newJSObject
  JSObject eventObj <- newJSObject
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: MisoString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode attrs snk
  flip (set "children") vnode =<< toJSVal =<< setKids
  pure $ VTree vnode
    where
      setKids = do
        kidsViews <- forM kids $ \kid -> do
          VTree jval <- runView kid snk
          pure jval
        fromList kidsViews
runView (Text t) _ = do
  vtree <- newJSObject
  set "type" ("vtext" :: MisoString) vtree
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

setAttrs :: JSObject -> [Attribute action] -> Sink action -> IO ()
setAttrs vnode attrs snk =
  forM_ attrs $ \case
    P k v -> do
      val <- toJSVal v
      o <- getProp "props" vnode
      set k val (JSObject o)
    E attr -> attr snk vnode
    S m -> do
      cssObj <- getProp "css" vnode
      forM_ (M.toList m) $ \(k,v) -> do
        set k v (JSObject cssObj)

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
