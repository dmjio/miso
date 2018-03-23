{-# LANGUAGE CPP                 #-}
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
import           Control.Monad.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Functor
import qualified JavaScript.Object.Internal    as OI
import           JavaScript.Web.AnimationFrame

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
  :: forall model action b
   . (Eq model)
  => App model action
  -> model
  -> (Sink action -> IO (IORef VTree))
  -> IO b
common App {..} m getView = do
  -- init empty actions
  actionsTQueue <- newTQueueIO
  let sink :: Sink action
      sink = atomically . writeTQueue actionsTQueue
  -- init Subs
  forM_ subs $ \sub ->
    sub sink
  -- Retrieves reference view
  viewRef <- getView sink
  -- know thy mountElement
  mountEl <- mountElement mountPoint
  -- Begin listening for events in the virtual dom
  delegator mountEl viewRef events
  -- Process initial action of application
  sink initialAction
  -- Program loop, blocks when there are no new actions

  let loop !oldModel = do
        -- Apply actions to model
        actions <- atomically $ do
                     actions <- flushTQueue actionsTQueue
                     when (null actions) retry $> actions
        let (Acc newModel effects) = foldl' (foldEffects sink update)
                                            (Acc oldModel (pure ())) actions
        effects
        when (oldModel /= newModel) $ do
          newVTree <- runView (view newModel) sink
          oldVTree <- readIORef viewRef
          void $ waitForAnimationFrame
          (diff mountPoint) (Just oldVTree) (Just newVTree)
          atomicWriteIORef viewRef newVTree
        loop newModel
  loop m

-- | Runs an isomorphic miso application
-- Assumes the pre-rendered DOM is already present
miso :: (HasURI model, Eq model) => App model action -> IO ()
miso app@App{..} = do
  uri <- getCurrentURI
  let modelWithUri = setURI uri model
  common app model $ \sink -> do
    let initialView = view modelWithUri
    VTree (OI.Object iv) <- flip runView sink initialView
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    newIORef initialVTree

-- | Runs a miso application
startApp :: Eq model => App model action -> IO ()
startApp app@App {..} =
  common app model $ \sink -> do
    let initialView = view model
    initialVTree <- flip runView sink initialView
    (diff mountPoint) Nothing (Just initialVTree)
    newIORef initialVTree

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
          forM_ effs $ \eff ->
            void $ forkIO (eff sink)

data Acc model = Acc !model !(IO ())

#if !MIN_VERSION_stm(2,4,5)
-- | Efficiently read the entire contents of a TQueue into a list.
-- This function never retries.
-- Note that the implementation from stm >= 2.4.5.0 is more efficient
-- than this fallback.
flushTQueue :: TQueue a -> STM [a]
flushTQueue tq = readAll []
  where
    readAll xs = do
      mbX <- tryReadTQueue tq
      case mbX of
        Nothing -> pure (reverse xs)
        Just x -> readAll (x:xs)
#endif
