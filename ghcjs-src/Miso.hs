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
  , module Miso.FFI
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import           GHCJS.Types (JSVal)
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
  -> (Sink action -> IO (URI, IORef VTree))
  -> IO b
common App {..} m getView = do
  -- init Notifier
  Notify {..} <- newNotify
  -- init empty actions
  actionsRef <- newIORef S.empty
  let writeEvent a = void . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> a, ())
        notify
  -- init Subs
  forM_ subs $ \sub ->
    sub writeEvent
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  (uri, viewRef) <- getView writeEvent
  -- know thy mountElement
  mountEl <- mountElement mountPoint
  -- Begin listening for events in the virtual dom
  delegator mountEl viewRef events
  -- Process initial action of application
  writeEvent initialAction
  -- Program loop, blocking on SkipChan
  let loop !oldModel oldUri = wait >> do
        -- Apply actions to model
        actions <- atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
        let (Acc newModel effects) = foldl' (foldEffects writeEvent update)
                                            (Acc oldModel (pure ())) actions
        effects
        if oldModel /= newModel
        then do
          let LocatedView mbNewUri newView = view newModel
          newUri <- handleUri oldUri mbNewUri
          swapCallbacks
          newVTree <- runView newView writeEvent
          oldVTree <- readIORef viewRef
          void $ waitForAnimationFrame
          (diff mountPoint) (Just oldVTree) (Just newVTree)
          releaseCallbacks
          atomicWriteIORef viewRef newVTree
          loop newModel newUri
        else
          loop newModel oldUri
  loop m uri

-- | Push a new URI on the history when it's different than the old URI.
handleUri :: URI -> Maybe URI -> IO URI
handleUri oldUri Nothing = pure oldUri
handleUri oldUri (Just newUri) = do
  when (oldUri /= newUri) $ pushURI newUri
  pure newUri

-- | Runs an isomorphic miso application
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> IO ()
miso f = do
  uri <- getCurrentURI
  let app@App {..} = f uri
  common app model $ \writeEvent -> do
    let LocatedView mbInitialUri initialView = view model
    newUri <- handleUri uri mbInitialUri
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    viewRef <- newIORef initialVTree
    pure (newUri, viewRef)

-- | Runs a miso application
startApp :: Eq model => App model action -> IO ()
startApp app@App {..} = do
  uri <- getCurrentURI
  common app model $ \writeEvent -> do
    let LocatedView mbInitialUri initialView = view model
    newUri <- handleUri uri mbInitialUri
    initialVTree <- flip runView writeEvent initialView
    (diff mountPoint) Nothing (Just initialVTree)
    viewRef <- newIORef initialVTree
    pure (newUri, viewRef)

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

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree :: JSVal -> IO ()

-- | Pins down the current callbacks for clearing later
foreign import javascript unsafe "swapCallbacks();"
  swapCallbacks :: IO ()

-- | Releases callbacks registered by the virtual DOM.
foreign import javascript unsafe "releaseCallbacks();"
  releaseCallbacks :: IO ()
