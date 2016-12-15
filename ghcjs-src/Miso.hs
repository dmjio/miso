{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
module Miso
  ( startApp
  , module Miso.Html
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Sequence as S
import           Data.Sequence ((|>))
import qualified Data.Foldable                 as F
import           Data.Function
import qualified Data.Map                      as M
import           JavaScript.Web.AnimationFrame

import           Miso.Concurrent
import           Miso.Event.Delegate
import           Miso.Html
import           Miso.Diff

startApp
  :: Eq model
  => model
  -> (model -> View model)
  -> [ Sub model ]
  -> M.Map MisoString Bool
  -> IO ()
startApp initialModel view subs events = do
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
  forM_ subs (writeEvent &)
  -- init event application thread
  void . forkIO . forever $ do
    action <- getEvent
    modifyMVar_ actionsMVar $ \actions ->
      pure (actions |> action)
  -- Create virtual dom, perform initial diff
  initialVTree <- flip runView writeEvent initialView
  Nothing `diff` (Just initialVTree)
  viewMVar <- newMVar initialVTree
  -- Begin listening for events in the virtual dom
  delegator viewMVar events
  -- Program loop, blocking on SkipChan
  forever $ wait >> do
    -- Apply actions to model
    shouldDraw <-
      modifyMVar actionsMVar $ \actions -> do
        shouldDraw <- modifyMVar modelMVar $ \oldModel ->
          let newModel = F.foldl' (&) oldModel actions
              shouldDraw = oldModel /= newModel
          in pure (newModel, shouldDraw)
        pure (S.empty, shouldDraw)
    when shouldDraw $ do
      newVTree <-
        flip runView writeEvent
          =<< view <$> readMVar modelMVar
      modifyMVar_ viewMVar $ \oldVTree -> do
        void $ waitForAnimationFrame
        Just oldVTree `diff` Just newVTree
        pure newVTree
