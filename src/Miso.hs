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
  , Component
#ifdef NATIVE
  , module Miso.TypeLevel
#endif
  , module Export
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           JavaScript.Object.Internal (Object(Object))

import           Miso.Diff (diff, mountElement)
import           Miso.FFI
import           Miso.Types (Component(Component))
import           Miso.Internal

import           Miso.Event as Export
import           Miso.Html as Export
import           Miso.Router as Export
import           Miso.Runner as Runner
import           Miso.Storage as Export
import           Miso.Subscription as Export
import           Miso.Types as Export hiding (Component(Component))
import           Miso.Util as Export
import           Miso.FFI as Export

#ifdef NATIVE
import           Miso.TypeLevel
#endif

#ifndef GHCJS_BOTH
import           Language.Javascript.JSaddle (eval)
import           Data.FileEmbed
import           Language.Javascript.JSaddle hiding (Success, obj, val)
#endif

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = withJS $ do
  app@App {..} <- f <$> getCurrentURI
  common app $ \snk -> do
    VTree (Object iv) <- runView Prerender (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    ref <- liftIO $ newIORef $ VTree (Object iv)
    registerSink mount mountEl ref snk
    pure (mount, mountEl, ref)

-- | Runs a miso application (as a @Component@)
-- Note: uses the 'name' as the key in @componentMap@
-- Mounts to `body`. Copies page into the virtual dom.
misoComponent
  :: Eq model
  => (URI -> Component name model action)
  -> JSM ()
misoComponent f = withJS $ do
  Component name app@App {..} <- f <$> getCurrentURI
  common app $ \snk -> do
    vtree@(VTree (Object jval)) <- runView Prerender (view model) snk
    mount <- getBody
    setBodyComponent name
    copyDOMIntoVTree (logLevel == DebugPrerender) mount jval
    ref <- liftIO (newIORef vtree)
    registerSink name mount ref snk
    pure (name, mount, ref)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = withJS $
  common app $ \snk -> do
    vtree <- runView DontPrerender (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    diff mountEl Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    registerSink mount mountEl ref snk
    pure (mount, mountEl, ref)

-- | Runs a miso application (as a @Component@)
-- Note: uses the 'name' as the mount point.
startComponent :: Eq model => Component name model action -> JSM ()
startComponent (Component name app@App{..}) = withJS $ common app $ \snk -> do
  vtree <- runView DontPrerender (view model) snk
  mount <- getBody
  setBodyComponent name
  diff mount Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  registerSink name mount ref snk
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

