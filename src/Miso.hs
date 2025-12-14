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
    -- ** Miso
    miso
  , (🍜)
    -- ** App
  , App
  , startApp
  , renderApp
    -- ** Component
  , Component
  , startComponent
  , component
  , (+>)
    -- ** Sink
  , withSink
  , Sink
    -- ** Mail
  , mail
  , checkMail
  , parent
  , mailParent
  , broadcast
    -- ** Subscriptions
  , startSub
  , stopSub
  , Sub
    -- ** Effect
  , issue
  , batch
  , io
  , io_
  , sync
  , sync_
  , for
    -- * Reactivity
    -- | Primitives for synchronizing parent and child models.
  , module Miso.Binding
    -- * Types
    -- | Core types for Miso applications.
  , module Miso.Types
    -- * Effect
    -- | 'Effect', 'Sub', and 'Sink' types for defining update functions and subscriptions.
  , module Miso.Effect
    -- * Event
    -- | Functions for specifying component lifecycle events and event handlers.
  , module Miso.Event
    -- * Property
    -- | Construct custom properties on DOM elements.
  , module Miso.Property
    -- * PubSub
    -- | Publish / Subscribe primitives for communication between components.
  , module Miso.PubSub
    -- * Run
    -- | Support for running and live-reloading of miso applications.
  , module Miso.Run
    -- * Subscriptions
    -- | Subscriptions for external events (mouse, keyboard, window, history, etc.).
  , module Miso.Subscription
    -- * Storage
    -- | Web Storage API (Local and Session storage) interface.
  , module Miso.Storage
    -- * Fetch
    -- | Interface to the Fetch API for making HTTP requests.
  , module Miso.Fetch
    -- * Util
    -- | Utility functions for views, parsing, and general purpose combinators.
  , module Miso.Util
    -- * FFI
    -- | Foreign Function Interface (FFI) utilities for interacting with JavaScript.
  , module Miso.FFI
    -- * State management
    -- | State management for Miso applications.
  , module Miso.State
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
#ifndef GHCJS_BOTH
#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm.TH as JSaddle.Wasm.TH
#else
import           Data.FileEmbed (embedStringFile)
import           Language.Javascript.JSaddle (eval)
#endif
#endif
-----------------------------------------------------------------------------
import           Miso.Binding
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.Fetch
import           Miso.FFI
import qualified Miso.FFI.Internal as FFI
import           Miso.Runtime
import           Miso.Property
import           Miso.PubSub
import           Miso.Router
import           Miso.Run
import           Miso.State
import           Miso.Storage
import           Miso.Subscription
import           Miso.Types
import           Miso.Util
----------------------------------------------------------------------------
-- | Runs an isomorphic @miso@ application.
-- Assumes the pre-rendered DOM is already present.
-- Always mounts to \<body\>. Copies page into the virtual DOM.
--
-- To get an IO action that starts the application, use 'run' on the result of this function.
--
-- @
-- main :: IO ()
-- main = run (miso (\\uri -> ..))
-- @
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = withJS $ do
  vcomp <- f <$> getURI
  body <- FFI.getBody
  initialize Hydrate isRoot vcomp (pure body)
-----------------------------------------------------------------------------
-- | Synonym for 'startComponent'.
--
-- To get an IO action that starts the application, use 'run' on the result of this function.
--
-- @
-- main :: IO ()
-- main = run (startApp app)
-- @
startApp :: Eq model => App model action -> JSM ()
startApp = startComponent
-----------------------------------------------------------------------------
-- | Alias for 'Miso.miso'.
(🍜) :: Eq model => (URI -> App model action) -> JSM ()
(🍜) = miso
----------------------------------------------------------------------------
-- | Runs a miso application
startComponent :: Eq model => Component ROOT model action -> JSM ()
startComponent vcomp = withJS (initComponent vcomp)
----------------------------------------------------------------------------
-- | Runs a 'miso' application, but with a custom rendering engine.
--
-- The 'MisoString' specified here is the variable name of a globally-scoped
-- JS object that implements the context interface per @ts\/miso\/context\/dom.ts@
-- This is necessary for native support.
--
-- To get an IO action that starts the application, use 'run' on the result of this function.
--
-- @
-- main :: IO ()
-- main = run (renderApp "my-context" app)
-- @
renderApp
  :: Eq model
  => MisoString
  -- ^ Name of the JS object that contains the drawing context
  -> App model action
  -- ^ Component application
  -> JSM ()
renderApp renderer vcomp =
  withJS (FFI.setDrawingContext renderer >> initComponent vcomp)
----------------------------------------------------------------------------
-- | Top-level t'Miso.Types.Component' initialization helper for 'renderApp' and 'startComponent'.
initComponent
  :: (Eq parent, Eq model)
  => Component parent model action
  -> JSM (ComponentState parent model action)
initComponent vcomp@Component {..} = do
  mount <- mountElement (getMountPoint mountPoint)
  initialize Draw isRoot vcomp (pure mount)
----------------------------------------------------------------------------
isRoot :: Bool
isRoot = True
----------------------------------------------------------------------------
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
