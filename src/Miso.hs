-----------------------------------------------------------------------------
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
    -- ** Combinators
    miso
  , startApp
  , startComponent
  , misoComponent
    -- ** Sink
  , sink
    -- ** Sampling
  , sample
    -- ** Message Passing
  , mail
  , notify
  , module Miso.Types
    -- * Effect
  , module Miso.Effect
    -- * Event
  , module Miso.Event
    -- * Mathml
  , module Miso.Mathml
    -- * Html
  , module Miso.Html
  , module Miso.Render
    -- * Router
  , module Miso.Router
    -- * Runner
  , module Miso.Runner
    -- * Subs
  , module Miso.Subscription
    -- * Storage
  , module Miso.Storage
    -- * Util
  , module Miso.Util
    -- * FFI
  , set
  , now
  , consoleLog
  , consoleLog'
  , getElementById
  , focus
  , blur
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (newIORef)
import           Language.Javascript.JSaddle (Object(Object), JSM)
#ifndef GHCJS_BOTH
import           Data.FileEmbed (embedStringFile)
import           Language.Javascript.JSaddle (eval)
import           Miso.String (MisoString)
#endif
-----------------------------------------------------------------------------
import           Miso.Diff (diff, mountElement)
import           Miso.Effect
import           Miso.Event
import           Miso.Html
import           Miso.Render
import           Miso.Router
import           Miso.Runner
import           Miso.Mathml
import           Miso.Subscription
import           Miso.Types
import           Miso.Util
import           Miso.Storage
import           Miso.Internal
import           Miso.FFI hiding (diff)
-----------------------------------------------------------------------------
-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present.
-- Note: Uses @mountPoint@ as the @Component@ name.
-- Always mounts to *<body>*. Copies page into the virtual DOM.
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
-----------------------------------------------------------------------------
-- | Runs a miso application (as a @Component@)
-- Assumes the pre-rendered DOM is already present.
-- Note: Uses @name@ in @Component name model action@ as the @Component@ name.
-- Always mounts to *<body>*. Copies page into the virtual DOM.
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
-----------------------------------------------------------------------------
-- | Runs a miso application
-- Initializes application at @mountPoint@ (defaults to @<body>@ when @Nothing@)
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = withJS $
  initialize app $ \snk -> do
    vtree <- runView DontPrerender (view model) snk
    let mount = getMountPoint mountPoint
    mountEl <- mountElement mount
    diff mountEl Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    pure (mount, mountEl, ref)
-----------------------------------------------------------------------------
-- | Runs a miso application (as a @Component@)
-- Initializes application at @name@ (defaults to @<body>@)
-- Ignores @mountPoint@ on the enclosing @App@, uses @name@ from @(Component name model action)@
startComponent :: Eq model => Component name model action -> JSM ()
startComponent (Component name app@App{..}) = withJS $ initialize app $ \snk -> do
  vtree <- runView DontPrerender (view model) snk
  mount <- getBody
  setBodyComponent name
  diff mount Nothing (Just vtree)
  ref <- liftIO (newIORef vtree)
  pure (name, mount, ref)
-----------------------------------------------------------------------------
withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
  _ <- eval ($(embedStringFile "jsbits/delegate.js") :: MisoString)
  _ <- eval ($(embedStringFile "jsbits/diff.js") :: MisoString)
  _ <- eval ($(embedStringFile "jsbits/isomorphic.js") :: MisoString)
  _ <- eval ($(embedStringFile "jsbits/util.js") :: MisoString)
#endif
  action
-----------------------------------------------------------------------------
