-----------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE QuasiQuotes               #-}
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
  , prerender
  , (🍜)
    -- ** App
  , App
  , startApp
  , renderApp
    -- ** Component
  , Component
  , component
  , (+>)
  , mount_
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
#ifdef WASM
  -- ** JS file embedding
  , evalFile
#endif
    -- * Reactivity (Data bindings)
    -- | Primitives for synchronizing parent and child models.
  , module Miso.Binding
    -- * DSL
    -- | A JavaScript DSL for easy FFI interoperability
  , module Miso.DSL
    -- * Effect
    -- | 'Effect', 'Sub', and 'Sink' types for defining update functions and subscriptions.
  , module Miso.Effect
    -- * Event
    -- | Functions for specifying component lifecycle events and event handlers.
  , module Miso.Event
    -- * Fetch
    -- | Interface to the Fetch API for making HTTP requests.
  , module Miso.Fetch
    -- * PubSub
    -- | Publish / Subscribe primitives for communication between components.
  , module Miso.PubSub
    -- * Property
    -- | Construct custom properties on DOM elements.
  , module Miso.Property
    -- * Reload
    -- | Support for clearing the page during live-reloading w/ WASM browser mode.
  , module Miso.Reload
    -- * Subscriptions
    -- | Subscriptions for external events (mouse, keyboard, window, history, etc.).
  , module Miso.Subscription
    -- * Storage
    -- | Web Storage API (Local and Session storage) interface.
  , module Miso.Storage
    -- * Types
    -- | Core types for Miso applications.
  , module Miso.Types
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
-----------------------------------------------------------------------------
import           Miso.Binding
import           Miso.Diff
import           Miso.DSL
import           Miso.Effect
import           Miso.Event
import           Miso.Fetch
import           Miso.FFI
import qualified Miso.FFI.Internal as FFI
import           Miso.Property
import           Miso.PubSub
import           Miso.Reload
import           Miso.Router
import           Miso.Runtime
import           Miso.State
import           Miso.Storage
import           Miso.Subscription
import           Miso.Types
import           Miso.Util
----------------------------------------------------------------------------
-- | Runs an isomorphic @miso@ application.
--
-- Assumes the pre-rendered DOM is already present.
-- Always mounts to \<body\>. Copies page into the virtual DOM.
--
-- @
-- main :: IO ()
-- main = miso defaultEvents (\\uri -> app uri))
-- @
miso :: Eq model => Events -> (URI -> App model action) -> IO ()
miso events f = withJS $ do
  vcomp <- f <$> getURI
  initialize events rootComponentId Hydrate isRoot vcomp FFI.getBody
----------------------------------------------------------------------------
-- | Like 'miso', except discards the 'URI' argument.
--
-- Use this function if you'd like to prerender, but not use navigation.
--
-- @
-- main :: IO ()
-- main = prerendder defaultEvents app
-- @
prerender :: Eq model => Events -> App model action -> IO ()
prerender events vcomp = withJS $ do
  initialize events rootComponentId Hydrate isRoot vcomp FFI.getBody
-----------------------------------------------------------------------------
-- | Like 'miso', except it does not perform page hydration.
--
-- This function draws your application on an empty <body>
--
-- You will most likely want to use this function for your application
-- unless you are using prerendering.
--
-- @
-- main :: IO ()
-- main = startApp defaultEvents app
-- @
--
startApp :: Eq model => Events -> App model action -> IO ()
startApp events vcomp = withJS (initComponent events vcomp)
-----------------------------------------------------------------------------
-- | Alias for 'Miso.miso'.
(🍜) :: Eq model => Events -> (URI -> App model action) -> IO ()
(🍜) = miso
----------------------------------------------------------------------------
-- | Runs a 'miso' application, but with a custom rendering engine.
--
-- The 'MisoString' specified here is the variable name of a globally-scoped
-- JS object that implements the context interface per @ts\/miso\/context\/dom.ts@
-- This is necessary for native support.
--
-- It is expected to be run on an empty @<body>@
--
-- @
-- main :: IO ()
-- main = renderApp defaultEvents "my-context" app
-- @
renderApp
  :: Eq model
  => Events
  -> MisoString
  -- ^ Name of the JS object that contains the drawing context
  -> App model action
  -- ^ Component application
  -> IO ()
renderApp events renderer vcomp =
  withJS (FFI.setDrawingContext renderer >> initComponent events vcomp isRoot)
----------------------------------------------------------------------------
-- | Top-level t'Miso.Types.Component' initialization helper for 'renderApp'.
initComponent
  :: (Eq parent, Eq model)
  => Events
  -> Component parent model action
  -> Bool
  -- ^ isRoot
  -> IO (ComponentState parent model action)
initComponent events vcomp@Component {..} root = do
  m <- mountElement (getMountPoint mountPoint)
  initialize events rootComponentId Draw root vcomp (pure m)
----------------------------------------------------------------------------
isRoot :: Bool
isRoot = True
----------------------------------------------------------------------------
#ifdef PRODUCTION
#define MISO_JS_PATH "js/miso.prod.js"
#else
#define MISO_JS_PATH "js/miso.js"
#endif
withJS :: IO a -> IO ()
withJS action = void $ do
#ifdef WASM
  $(evalFile MISO_JS_PATH)
#endif
  action
-----------------------------------------------------------------------------
