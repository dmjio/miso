-----------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( -- * API
    -- ** Entry
    miso
  , (🍜)
  , startComponent
  , renderComponent
    -- ** Sink
  , withSink
  , Sink
    -- ** Publishers / Subscribers
  , subscribe
  , unsubscribe
  , publish
  , Topic
  , topic
    -- ** Subscriptions
  , startSub
  , stopSub
  , Sub
  -- ** Effect
  , issue
  , batch
  , io
  , io_
  , for
  , module Miso.Types
    -- * Effect
  , module Miso.Effect
    -- * Event
  , module Miso.Event
    -- * Property
  , module Miso.Property
    -- * Html
  , module Miso.Html
  , module Miso.Render
    -- * Property
  , module Miso.Property
    -- * Router
  , module Miso.Router
    -- * Run
  , module Miso.Run
    -- * Subscriptions
  , module Miso.Subscription
    -- * Storage
  , module Miso.Storage
    -- * Fetch
  , module Miso.Fetch
    -- * Util
  , module Miso.Util
    -- * FFI
  , module Miso.FFI
    -- * State management
  , module Miso.State
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (newIORef, IORef)
import           Language.Javascript.JSaddle (Object(Object), JSM)
#ifndef GHCJS_BOTH
#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm.TH as JSaddle.Wasm.TH
#else
import           Data.FileEmbed (embedStringFile)
import           Language.Javascript.JSaddle (eval)
#endif
#endif
-----------------------------------------------------------------------------
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.Fetch
import           Miso.FFI
import qualified Miso.FFI.Internal as FFI
import           Miso.Html
import           Miso.Internal
import           Miso.Property
import           Miso.Render
import           Miso.Router
import           Miso.Run
import           Miso.State
import           Miso.Storage
import           Miso.String (MisoString)
import           Miso.Subscription
import           Miso.Types
import           Miso.Util
----------------------------------------------------------------------------
-- | Runs an isomorphic @miso@ application.
-- Assumes the pre-rendered DOM is already present.
-- Always mounts to \<body\>. Copies page into the virtual DOM.
miso :: Eq model => (URI -> Component model action) -> JSM ()
miso f = withJS $ do
  app@Component {..} <- f <$> getURI
  initialize app $ \snk -> do
    renderStyles styles
    VTree (Object vtree) <- runView Hydrate (view model) snk logLevel events
    mount <- FFI.getBody
    FFI.hydrate (logLevel `elem` [DebugHydrate, DebugAll]) mount vtree
    viewRef <- liftIO $ newIORef $ VTree (Object vtree)
    pure (mount, viewRef)
-----------------------------------------------------------------------------
-- | Alias for 'miso'.
(🍜) :: Eq model => (URI -> Component model action) -> JSM ()
(🍜) = miso
----------------------------------------------------------------------------
-- | Runs a miso application
-- Initializes application at 'mountPoint' (defaults to \<body\> when @Nothing@)
startComponent
  :: Eq model
  => Component model action
  -- ^ Component application
  -> JSM ()
startComponent vcomp@Component { styles } = withJS $ initComponent vcomp (renderStyles styles)
----------------------------------------------------------------------------
-- | Runs a miso application, but with a custom rendering engine.
-- The @MisoString@ specified here is the variable name of a globally-scoped
-- JS object that implements the context interface per 'ts/miso/context/dom.ts'
-- This is necessary for native support.
renderComponent
  :: Eq model
  => Maybe MisoString
  -- ^ Name of the JS object that contains the drawing context
  -> Component model action
  -- ^ Component application
  -> JSM ()
  -- ^ Custom hook to perform any JSM action (e.g. render styles) before initialization.
  -> JSM ()
renderComponent Nothing vcomp _ = startComponent vcomp
renderComponent (Just renderer) vcomp hooks = withJS $ do
  FFI.setDrawingContext renderer
  initComponent vcomp hooks
----------------------------------------------------------------------------
-- | Internal helper function to support both 'render' and 'startComponent'
initComponent
  :: Eq model
  => Component model action
  -- ^ Component application
  -> JSM ()
  -- ^ Custom hook to perform any JSM action (e.g. render styles) before initialization.
  -> JSM (IORef VTree)
initComponent vcomp@Component{..} hooks = do
  initialize vcomp $ \snk -> hooks >> do
    vtree <- runView Draw (view model) snk logLevel events
    mount <- mountElement (getMountPoint mountPoint)
    diff Nothing (Just vtree) mount
    viewRef <- liftIO (newIORef vtree)
    pure (mount, viewRef)
-----------------------------------------------------------------------------
#ifdef PRODUCTION
#define MISO_JS_PATH "js/miso.prod.js"
#else
#define MISO_JS_PATH "js/miso.js"
#endif
-- | Used when compiling with jsaddle to make miso's JavaScript present in
-- the execution context.
withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
#ifdef WASM
  $(JSaddle.Wasm.TH.evalFile MISO_JS_PATH)
#else
  _ <- eval ($(embedStringFile MISO_JS_PATH) :: MisoString)
#endif
#endif
  action
-----------------------------------------------------------------------------
