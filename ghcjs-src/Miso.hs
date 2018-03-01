{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
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
  , module Miso.Types
  , module Miso.Router
  , module Miso.Util
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import qualified JavaScript.Object.Internal    as OI
import           JavaScript.Web.AnimationFrame

import           Miso.Concurrent
import           Miso.Delegate
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.Util
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
import           Miso.Types
import           Miso.FFI

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => App model action
  -> model
  -> (Sink action -> IO (IORef VTree))
  -> IO b
common App {..} m getView = do
  -- init Notifier
  Notify {..} <- newNotify
  -- init empty Model
  modelRef <- newIORef m
  -- init empty actions
  actionsRef <- newIORef S.empty
  let writeEvent a = void . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> a, ())
        notify
  -- init Subs
  forM_ subs $ \sub ->
    sub (readIORef modelRef) writeEvent
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- getView writeEvent
  -- know thy mountElement
  mountEl <- mountElement mountPoint
  -- Begin listening for events in the virtual dom
  delegator mountEl viewRef events
  -- Process initial action of application
  writeEvent initialAction
  -- Program loop, blocking on SkipChan
  forever $ wait >> do
    -- Apply actions to model
    actions <- atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
    (shouldDraw, effects) <- atomicModifyIORef' modelRef $! \oldModel ->
          let (newModel, effects) =
                foldl' (foldEffects writeEvent update)
                  (oldModel, pure ()) actions
          in (newModel, (oldModel /= newModel, effects))
    effects
    when shouldDraw $ do
      newVTree <-
        flip runView writeEvent
          =<< view <$> readIORef modelRef
      oldVTree <- readIORef viewRef
      void $ waitForAnimationFrame
      (diff mountPoint) (Just oldVTree) (Just newVTree)
      atomicWriteIORef viewRef newVTree

-- | Runs an isomorphic miso application
-- Assumes the pre-rendered DOM is already present
miso :: (HasURI model, Eq model) => App model action -> IO ()
miso app@App{..} = do
  uri <- getCurrentURI
  let modelWithUri = setURI uri model
  common app model $ \writeEvent -> do
    let initialView = view modelWithUri
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    newIORef initialVTree

-- | Runs a miso application
startApp :: Eq model => App model action -> IO ()
startApp app@App {..} =
  common app model $ \writeEvent -> do
    let initialView = view model
    initialVTree <- flip runView writeEvent initialView
    (diff mountPoint) Nothing (Just initialVTree)
    newIORef initialVTree

-- | Helper
foldEffects
  :: Sink action
  -> (action -> model -> Effect action model)
  -> (model, IO ()) -> action -> (model, IO ())
foldEffects sink update = \(!model, !as) action ->
  case update action model of
    Effect newModel effs -> (newModel, newAs)
      where
        newAs = as >> do
          forM_ effs $ \eff ->
            void $ forkIO (eff sink)
