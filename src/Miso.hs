{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TemplateHaskell     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , startApp
  , sink
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
#ifndef ghcjs_HOST_OS
  , module Miso.TypeLevel
#endif
  , module Miso.Types
  , module Miso.Router
  , module Miso.Util
  , module Miso.FFI
  , module Miso.WebSocket
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import qualified JavaScript.Object.Internal    as OI
import           System.IO.Unsafe
import           System.Mem.StableName

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle   (eval, waitForAnimationFrame)
import           GHCJS.Types                   (JSString)
import           Data.FileEmbed
#else
import           JavaScript.Web.AnimationFrame
#endif

import           Miso.Concurrent
import           Miso.Delegate
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
#ifndef ghcjs_HOST_OS
import           Miso.TypeLevel
#endif
import           Miso.Types
import           Miso.Util
import           Miso.WebSocket

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => App model action
  -> (Sink action -> JSM (IORef VTree))
  -> JSM ()
common App {..} getView = do
#ifndef ghcjs_HOST_OS
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: JSString)
#endif
  -- init Notifier
  Notify {..} <- liftIO newNotify
  -- init empty actions
  actionsRef <- liftIO (newIORef S.empty)
  let writeEvent a = void . liftIO . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> a, ())
        notify
  -- init global sink
  liftIO (writeIORef sinkRef writeEvent)
  -- init Subs
  forM_ subs $ \sub ->
    sub writeEvent
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . liftIO . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- getView writeEvent
  -- know thy mountElement
  mountEl <- mountElement mountPoint
  -- Begin listening for events in the virtual dom
  delegator mountEl viewRef events
  -- Process initial action of application
  writeEvent initialAction
  -- Program loop, blocking on SkipChan

  let
    loop !oldModel = liftIO wait >> do
        -- Apply actions to model
        actions <- liftIO $ atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
        let (Acc newModel effects) = foldl' (foldEffects writeEvent update)
                                            (Acc oldModel (pure ())) actions
        effects
        oldName <- liftIO $ oldModel `seq` makeStableName oldModel
        newName <- liftIO $ newModel `seq` makeStableName newModel
        when (oldName /= newName && oldModel /= newModel) $ do
          swapCallbacks
          newVTree <- runView (view newModel) writeEvent
          oldVTree <- liftIO (readIORef viewRef)
          void $ waitForAnimationFrame
          (diff mountPoint) (Just oldVTree) (Just newVTree)
          releaseCallbacks
          liftIO (atomicWriteIORef viewRef newVTree)
        syncPoint
        loop newModel
  loop model

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = do
  app@App {..} <- f <$> getCurrentURI
  common app $ \writeEvent -> do
    let initialView = view model
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    mountEl <- mountElement mountPoint
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    liftIO (newIORef initialVTree)

sinkRef :: IORef (Sink action)
{-# NOINLINE sinkRef #-}
sinkRef = unsafePerformIO $ newIORef (\_ -> pure ())

-- | Global sink exposed as a backdoor
-- Meant for usage in long running IO actions, or custom callbacks
-- Good for integrating with third-party components.
sink :: Sink action
sink = unsafePerformIO (readIORef sinkRef)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} =
  common app $ \writeEvent -> do
    let initialView = view model
    initialVTree <- flip runView writeEvent initialView
    (diff mountPoint) Nothing (Just initialVTree)
    liftIO (newIORef initialVTree)

-- | Helper
foldEffects
  :: Sink action
  -> (action -> model -> Effect action model)
  -> Acc model -> action -> Acc model
foldEffects snk update = \(Acc model as) action ->
  case update action model of
    Effect newModel effs -> Acc newModel newAs
      where
        newAs = as >> do
          forM_ effs $ \eff -> forkJSM (eff snk)

data Acc model = Acc !model !(JSM ())
