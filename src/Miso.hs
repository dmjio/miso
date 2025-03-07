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
  , misoComponent
    -- * Third-party integration helpers
  , sink
  , sinkRaw
    -- * Component communication
  , notify
  , mail
    -- * Abstraction for jsaddle
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

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified JavaScript.Object.Internal as OI
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle (eval)
import           Data.FileEmbed
import           Language.Javascript.JSaddle hiding (Success, obj, val)
#endif

import           Miso.Diff
import           Miso.Event
import           Miso.FFI
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
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = withJS $ do
  app@App {..} <- f <$> getCurrentURI
  common app $ \snk -> do
    VTree (OI.Object iv) <- runView (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    ref <- liftIO $ newIORef $ VTree (OI.Object iv)
    registerSink mount ref snk
    pure (mount, mountEl, ref)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = withJS $
  common app $ \snk -> do
    vtree <- runView (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    diff mountEl Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    registerSink mount ref snk
    pure (mount, mountEl, ref)

-- | Runs a miso application (as a @Component@)
-- Note: uses the 'name' as the mount point.
startComponent :: Eq model => MT.Component name model action -> JSM ()
startComponent (MT.Component name app) = withJS $ common app $ \snk -> do
  setBodyComponent name
  initComponent name app snk

-- | Runs a miso application (as a @Component@)
-- Note: uses the 'name' as the mount point.
-- Copies page into the virtual dom.
misoComponent :: Eq model => (URI -> MT.Component name model action) -> JSM ()
misoComponent f = withJS $ do
  comp@(MT.Component name app) <- f <$> getCurrentURI
  common app $ \snk -> do
    vtree@(VTree (OI.Object vdom)) <- runView (Embed (MT.SomeComponent comp) componentOptions) snk
    mount <- getComponent name -- dmj: we assume we have received the HTML from the server
    copyDOMIntoVTree (logLevel app == DebugPrerender) mount vdom
    ref <- liftIO (newIORef vtree)
    registerSink name ref snk
    pure (name, mount, ref)

withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef ghcjs_HOST_OS
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: JSString)
#endif
  action

