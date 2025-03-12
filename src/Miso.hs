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
  , module Export
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Language.Javascript.JSaddle

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

#ifndef GHCJS_BOTH
import           Data.FileEmbed
#endif

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present.
-- Note: Uses 'mountPoint' as the @Component@ name.
-- Always mounts to <body>. Copies page into the virtual dom.
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = withJS $ do
  app@App {..} <- f <$> getCurrentURI
  initialize app $ \snk -> do
    VTree (Object iv) <- runView Prerender (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- getBody
    copyDOMIntoVTree (logLevel == DebugPrerender) mountEl iv
    ref <- liftIO $ newIORef $ VTree (Object iv)
    pure (mount, mountEl, ref)

-- | Runs a miso application (as a @Component@)
-- Assumes the pre-rendered DOM is already present.
-- Note: Uses 'name' in @(Component name model action) as the @Component@ name.
-- Always mounts to <body>. Copies page into the virtual dom.
misoComponent
  :: Eq model
  => (URI -> Component name model action)
  -> JSM ()
misoComponent f = withJS $ do
  Component name app@App {..} <- f <$> getCurrentURI
  initialize app $ \snk -> do
    vtree@(VTree (Object jval)) <- runView Prerender (view model) snk
    mount <- getBody
    setBodyComponent name
    copyDOMIntoVTree (logLevel == DebugPrerender) mount jval
    ref <- liftIO (newIORef vtree)
    pure (name, mount, ref)

-- | Runs a miso application
-- Initializes application at `mountPoint` (defaults to <body> when @Nothing@)
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = withJS $
  initialize app $ \snk -> do
    vtree <- runView DontPrerender (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    diff mountEl Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    pure (mount, mountEl, ref)

-- | Runs a miso application (as a @Component@)
-- Initializes application at `name` (defaults to <body> when @Nothing@)
-- Ignores @mountPoint@ on the enclosing @App@, uses @name@ from @(Component name model action)
startComponent :: Eq model => Component name model action -> JSM ()
startComponent (Component name app@App{..}) = withJS $ initialize app $ \snk -> do
  vtree <- runView DontPrerender (view model) snk
  mount <- mountElement name
  diff mount Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  pure (name, mount, ref)

withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: JSString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: JSString)
#endif
  action
