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
  , mount
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
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)
import           Data.IORef (atomicWriteIORef)
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
  liftIO (atomicWriteIORef globalMountPoint body)
  initialize Hydrate isRoot vcomp (pure body)
-----------------------------------------------------------------------------
-- | Synonym 'startApp' to 'startComponent'.
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
renderApp renderer vcomp = do
  withJS (FFI.setDrawingContext renderer >> initComponent vcomp)
----------------------------------------------------------------------------
-- | top-level t'Miso.Types.Component' initialization helper for `renderApp` and `startComponent`
initComponent
  :: (Eq parent, Eq model)
  => Component parent model action
  -> JSM (ComponentState model action)
initComponent vcomp@Component {..} = do
  globalMount <- mountElement (getMountPoint mountPoint)
  liftIO (atomicWriteIORef globalMountPoint globalMount)
  initialize Draw isRoot vcomp (pure globalMount)
----------------------------------------------------------------------------
-- | Is the component being rendered the root component
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
