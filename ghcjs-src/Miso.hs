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
  ( startApp
  , App (..)
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
  , module Miso.Types
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import           JavaScript.Web.AnimationFrame

import           Miso.Concurrent
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html                     hiding (foldl')
import           Miso.Subscription
import           Miso.Types

-- | Runs a miso application
startApp :: Eq model => App model action -> IO ()
startApp App {..} = do
  let initialView = view model
  -- init empty Model
  modelRef <- newIORef model
  -- init empty actions
  actionsMVar <- newMVar S.empty
  -- init Notifier
  Notify {..} <- newNotify
  -- init EventWriter
  EventWriter {..} <- newEventWriter notify
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
  -- Create virtual dom, perform initial diff
  initialVTree <- flip runView writeEvent initialView
  Nothing `diff` (Just initialVTree)
  viewRef <- newIORef initialVTree
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
      
