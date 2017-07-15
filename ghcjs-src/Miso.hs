{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2017 David M. Johnson
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
  -> ((action -> IO ()) -> IO (IORef VTree))
  -> IO b
common App {..} m getView = do
  -- init Notifier
  Notify {..} <- newNotify
  -- init EventWriter
  EventWriter {..} <- newEventWriter notify
  -- init empty Model
  modelRef <- newIORef m
  -- init empty actions
  actionsMVar <- newMVar S.empty
  -- init Subs
  forM_ subs $ \sub ->
    sub (readIORef modelRef) writeEvent
  -- init event application thread
  void . forkIO . forever $ do
    action <- getEvent
    modifyMVar_ actionsMVar $! \actions ->
      pure (actions |> action)
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- getView writeEvent
  -- Begin listening for events in the virtual dom
  delegator viewRef events
  -- Program loop, blocking on SkipChan
  forever $ wait >> do
    -- Apply actions to model
    shouldDraw <-
      modifyMVar actionsMVar $! \actions -> do
        (shouldDraw, effects) <- atomicModifyIORef' modelRef $! \oldModel ->
          let (newModel, effects) =
                foldl' (foldEffects writeEvent update)
                  (oldModel, pure ()) actions
          in (newModel, (oldModel /= newModel, effects))
        effects
        pure (S.empty, shouldDraw)
    when shouldDraw $ do
      newVTree <-
        flip runView writeEvent
          =<< view <$> readIORef modelRef
      oldVTree <- readIORef viewRef
      void $ waitForAnimationFrame
      Just oldVTree `diff` Just newVTree
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
    Nothing `diff` (Just initialVTree)
    newIORef initialVTree

-- | Helper
foldEffects
  :: (action -> IO ())
  -> (action -> model -> Effect model action)
  -> (model, IO ()) -> action -> (model, IO ())
foldEffects sink update = \(model, as) action ->
  case update action model of
    NoEffect newModel -> (newModel, as)
    Effect newModel eff -> (newModel, newAs)
      where
        newAs = as >> do void . forkIO . sink =<< eff
