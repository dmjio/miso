{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
module Miso
  ( startApp
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import qualified Data.Map as M
import           Data.Sequence ((|>))
import qualified Data.Sequence as S
import           JavaScript.Web.AnimationFrame

import           Miso.Concurrent
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Subscription

startApp
  :: Eq model
  => model
  -> (action -> model -> Effect model action)
  -> (model -> View action)
  -> [ Sub action model ]
  -> M.Map MisoString Bool
  -> IO ()
startApp initialModel update view subs events = do
  let initialView = view initialModel
  -- init empty Model
  modelMVar <- newMVar initialModel
  -- init empty actions
  actionsMVar <- newMVar S.empty
  -- init Notifier
  Notify {..} <- newNotify
  -- init EventWriter
  EventWriter {..} <- newEventWriter notify
  -- init Subs
  forM_ subs $ \sub ->
    sub (readMVar modelMVar) writeEvent
  -- init event application thread
  void . forkIO . forever $ do
    action <- getEvent
    modifyMVar_ actionsMVar $! \actions ->
      pure (actions |> action)
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
        shouldDraw <- modifyMVar modelMVar $! \oldModel -> do
          newModel <- foldM (foldEffects writeEvent update) oldModel actions
          pure (newModel, oldModel /= newModel)
        pure (S.empty, shouldDraw)
    when shouldDraw $ do
      newVTree <-
        flip runView writeEvent
          =<< view <$> readMVar modelMVar
      oldVTree <- readIORef viewRef
      void $ waitForAnimationFrame
      Just oldVTree `diff` Just newVTree
      atomicWriteIORef viewRef newVTree

foldEffects
  :: (action -> IO ())
  -> (action -> model -> Effect model action)
  -> model
  -> action
  -> IO model
foldEffects sink update model action =
  case update action model of
    NoEffect newModel -> pure newModel
    Effect newModel eff -> do
      void . forkIO . sink =<< eff
      pure newModel
