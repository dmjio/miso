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
    -- ** Sink
  , sink
    -- ** Sampling
  , sample
    -- ** Message Passing
  , notify
  , module Miso.Types
    -- * Effect
  , module Miso.Effect
    -- * Event
  , module Miso.Event
    -- * Html
  , module Miso.Html
  , module Miso.Render
    -- * Mathml
  , module Miso.Mathml
    -- * Router
  , module Miso.Router
    -- * Run
  , module Miso.Run
    -- * Exception
  , module Miso.Exception
    -- * Subs
  , module Miso.Subscription
    -- * Storage
  , module Miso.Storage
    -- * Util
  , module Miso.Util
    -- * FFI
  , module Miso.FFI
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
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.Exception
import           Miso.FFI
import qualified Miso.FFI.Internal as FFI
import           Miso.Html
import           Miso.Internal
import           Miso.Mathml
import           Miso.Render
import           Miso.Router
import           Miso.Run
import           Miso.Storage
import           Miso.Subscription
import           Miso.Types
import           Miso.Util
-----------------------------------------------------------------------------
-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present.
-- Note: Uses @mountPoint@ as the @Component@ name.
-- Always mounts to /<body>/. Copies page into the virtual DOM.
miso :: Eq model => (URI -> App effect model action a) -> JSM ()
miso f = withJS $ do
  app@App {..} <- f <$> getCurrentURI
  initialize app $ \snk -> do
    renderStyles styles
    VTree (Object vtree) <- runView Prerender (view model) snk logLevel events
    let name = getMountPoint mountPoint
    FFI.setBodyComponent name
    mount <- FFI.getBody
    FFI.hydrate (logLevel `elem` [DebugPrerender, DebugAll]) mount vtree
    viewRef <- liftIO $ newIORef $ VTree (Object vtree)
    pure (name, mount, viewRef)
-----------------------------------------------------------------------------
-- | Runs a miso application
-- Initializes application at @mountPoint@ (defaults to /<body>/ when @Nothing@)
startApp :: Eq model => App effect model action a -> JSM ()
startApp app@App {..} = withJS $
  initialize app $ \snk -> do
    renderStyles styles
    vtree <- runView DontPrerender (view model) snk logLevel events
    let name = getMountPoint mountPoint
    FFI.setBodyComponent name
    mount <- mountElement name
    diff mount Nothing (Just vtree)
    viewRef <- liftIO (newIORef vtree)
    pure (name, mount, viewRef)
-----------------------------------------------------------------------------
-- | Used when compiling with jsaddle to make miso's JavaScript present in
-- the execution context.
withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
#ifdef PRODUCTION
  _ <- eval ($(embedStringFile "js/miso.prod.js") :: MisoString)
#else
  _ <- eval ($(embedStringFile "js/miso.js") :: MisoString)
#endif
#endif
  action
-----------------------------------------------------------------------------
