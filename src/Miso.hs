{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}

#ifdef IOS
#else
{-# LANGUAGE TemplateHaskell     #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , startApp
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
#ifndef __GHCJS__
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
import           Data.Sequence ((|>))
import           System.Mem.StableName
import qualified Data.Sequence as S
import qualified JavaScript.Object.Internal as OI

#ifndef __GHCJS__
import           Language.Javascript.JSaddle (eval, waitForAnimationFrame)
#ifdef IOS
import           Miso.JSBits
#else
import           GHCJS.Types (JSString)
import           Data.FileEmbed
#endif
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
#ifndef __GHCJS__
import           Miso.TypeLevel
#endif
import           Miso.Types
import           Miso.Util
import           Miso.WebSocket

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => App model action
  -> model
  -> (Sink action -> JSM (IORef VTree))
  -> JSM ()
common App {..} m getView = do
#ifndef __GHCJS__
#ifdef IOS
  mapM_ eval [delegateJs,diffJs,isomorphicJs,utilJs]
#else
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: JSString)
#endif
#endif
  -- init Notifier
  Notify {..} <- liftIO newNotify
  -- init empty actions
  actionsRef <- liftIO (newIORef S.empty)
  let writeEvent a = void . liftIO . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> a, ())
        notify
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
  loop m

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = do
  app@App {..} <- f <$> getCurrentURI
  common app model $ \writeEvent -> do
    let initialView = view model
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    mountEl <- mountElement mountPoint
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    liftIO (newIORef initialVTree)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} =
  common app model $ \writeEvent -> do
    let initialView = view model
    initialVTree <- flip runView writeEvent initialView
    (diff mountPoint) Nothing (Just initialVTree)
    liftIO (newIORef initialVTree)

-- | Helper
foldEffects
  :: Sink action
  -> (action -> model -> Effect action model)
  -> Acc model -> action -> Acc model
foldEffects sink update = \(Acc model as) action ->
  case update action model of
    Effect newModel effs -> Acc newModel newAs
      where
        newAs = as >> do
          forM_ effs $ \eff -> forkJSM (eff sink)

data Acc model = Acc !model !(JSM ())
