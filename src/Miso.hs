{-# LANGUAGE OverloadedStrings   #-}
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
  ( -- * Miso
    miso
  , startApp
  , startComponent
    -- * Third-party integration helpers
  , sink
  , sinkRaw
    -- * Component communication
  , notify
  , mail
    -- * Abstraction for hot reload
  , run
  , MT.Component
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
#ifdef NATIVE
  , module Miso.TypeLevel
#endif
  , module Miso.Types
  , module Miso.Router
  , module Miso.Util
  , module Miso.FFI
  , module Miso.WebSocket
  , module Miso.Storage
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef

import           Miso.Diff
import           Miso.Event
import           Miso.FFI hiding (close, go, back, send, forward, diff)
import           Miso.Html
import           Miso.Internal
import           Miso.Router
import           Miso.Runner (run)
import           Miso.Storage
import           Miso.Subscription
#ifdef NATIVE
import           Miso.TypeLevel
#endif
import           Miso.Types hiding (Component(..), SomeComponent(SomeComponent))
import qualified Miso.Types as MT
import           Miso.Util
import           Miso.WebSocket

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> IO ()
miso f = void $ do
  app@App {..} <- f <$> getCurrentURI
  common app $ \snk -> do
    VTree (JSObject iv) <- runView (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    -- Create virtual dom, perform initial diff
    ref <- liftIO $ newIORef $ VTree (JSObject iv)
    registerSink mount ref snk
    pure (mount, mountEl, ref)

-- | Runs a miso application
startApp :: Eq model => App model action -> IO ()
startApp app@App {..} = void $ do
  common app $ \snk -> do
    vtree <- runView (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    diff mountEl Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    registerSink mount ref snk
    pure (mount, mountEl, ref)

secs :: Int -> Int
secs = (*1000000)

-- | Runs a miso application (as a @Component@)
-- Note: uses the 'name' as the mount point.
startComponent :: Eq model => MT.Component name model action -> IO ()
startComponent (MT.Component mount app) =
  void $ common app (initComponent mount app)
