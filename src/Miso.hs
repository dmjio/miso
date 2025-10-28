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
    -- ** Sink
  , withSink
  , Sink
    -- ** Component
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
  , for
    -- * Reactivity
  , module Miso.Binding
    -- * Types
  , module Miso.Types
    -- * Effect
  , module Miso.Effect
    -- * Event
  , module Miso.Event
    -- * Property
  , module Miso.Property
    -- * PubSub
  , module Miso.PubSub
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
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = withJS $ do
  vcomp <- f <$> getURI
  initialize Hydrate vcomp FFI.getBody
-----------------------------------------------------------------------------
-- | Synonym 'startApp' to 'startComponent'.
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
-- | top-level t'Component' initialization helper for `renderApp` and `startComponent`
initComponent
  :: Eq model
  => Component parent model action
  -> JSM (ComponentState model action)
initComponent vcomp@Component {..} =
  initialize Draw vcomp (mountElement (getMountPoint mountPoint))
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
