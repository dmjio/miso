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
  ( miso
  , startApp
  , sink
  , notify
  , run
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
  , module Miso.Storage
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified JavaScript.Object.Internal as OI

import           Miso.Diff
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Internal
import           Miso.Router
import           Miso.Runner (run)
import           Miso.Storage
import           Miso.Subscription
#ifndef ghcjs_HOST_OS
import           Miso.TypeLevel
#endif
import           Miso.Types hiding (Component(..))
import           Miso.Util
import           Miso.WebSocket

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = void $ do
  app@App {..} <- f <$> getCurrentURI
  common app $ \snk -> do
    let mount = getMountPoint mountPoint
    VTree (OI.Object iv) <- runView (view model) snk
    e <- mountElement mount
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) e iv
    -- Create virtual dom, perform initial diff
    ref <- liftIO $ newIORef $ VTree (OI.Object iv)
    registerSink mount ref snk
    pure (mount, ref)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = void $
  common app $ \snk -> do
    let mount = getMountPoint mountPoint
    vtree <- runView (view model) snk
    diff (getMountPoint mountPoint) Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    registerSink mount ref snk
    pure (mount, ref)
