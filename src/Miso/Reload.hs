-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Reload
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Reload" supports hot-reloading of miso applications during
-- interactive development with GHC WASM browser mode (@ghciwatch@ +
-- WASM GHCi). It provides two entry points that replace @startApp@ in
-- your @main@:
--
-- ['reload'] clears @\<head\>@ and @\<body\>@ — full reset on every @:r@; model is lost
-- ['live'] clears @\<body\>@ only — model state survives @:r@
--
-- If your top-level t'Component' uses a non-trivial app-global @context@ (see
-- 'Miso.startAppWithContext'), use 'reloadWithContext' \/ 'liveWithContext',
-- which seed the @context@ just as 'Miso.startAppWithContext' does.
--
-- = reload
--
-- Clears both @\<head\>@ and @\<body\>@, kills any running scheduler thread,
-- and re-mounts the component from scratch. All application state is lost.
-- Use this when you are actively changing the @model@ type.
--
-- @
-- main :: IO ()
-- main = 'reload' 'Miso.Event.Types.defaultEvents' app
-- @
--
-- = live
--
-- Clears only @\<body\>@, then re-mounts the component using the __old
-- model__ value recovered from the previous GHCi session via a C-heap
-- stable pointer. @\<head\>@ injections (stylesheets, scripts) from the
-- previous session are preserved.
--
-- @
-- main :: IO ()
-- main = 'live' 'Miso.Event.Types.defaultEvents' app
-- @
--
-- __Warning__: 'live' is unsafe if you change the @model@ type between
-- reloads (adding, removing, or changing a field's type). Such a change
-- will produce a segfault because the old in-memory model is coerced
-- directly into the new type. Use 'reload' whenever you alter the model
-- schema.
--
-- = See also
--
-- * <https://github.com/haskell-miso/miso-sampler miso-sampler> — reference project demonstrating 'live'
-- * "Miso.Runtime" — 'Miso.Runtime.initComponent' and component lifecycle
-- * "Miso.Event.Types" — 'Miso.Event.Types.defaultEvents' used as first argument
----------------------------------------------------------------------------
module Miso.Reload
  ( -- ** Functions
    reload
  , reloadWithContext
  , live
  , liveWithContext
  ) where
-----------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
-----------------------------------------------------------------------------
import           Miso.DSL ((!), jsg, setField)
import qualified Miso.FFI.Internal as FFI
import           Miso.Types (Component(..), Events, App)
import           Miso.String (MisoString)
import           Miso.Runtime (componentModel, initComponent, topLevelComponentId, globalContext, Hydrate(..))
import           Miso.Runtime.Internal (components, schedulerThread)
-----------------------------------------------------------------------------
import           Miso.Lens
-----------------------------------------------------------------------------
import qualified Data.IntMap.Strict as IM
import           Data.IORef
import           Foreign hiding (void)
import           Foreign.C.Types
-----------------------------------------------------------------------------
foreign import ccall unsafe "miso_x_store"
  x_store :: StablePtr a -> IO ()
-----------------------------------------------------------------------------
foreign import ccall unsafe "miso_x_get"
  x_get :: IO (StablePtr a)
-----------------------------------------------------------------------------
foreign import ccall unsafe "miso_x_exists"
  x_exists :: IO CInt
-----------------------------------------------------------------------------
foreign import ccall unsafe "miso_x_clear"
  x_clear :: IO ()
-----------------------------------------------------------------------------
#define MISO_JS_PATH "js/miso.js"
-----------------------------------------------------------------------------
-- | Clears the \<body\> and \<head\> on each 'reload'.
--
-- Meant to be used with WASM browser mode.
--
-- @
-- main :: IO ()
-- main = 'reload' 'defaultEvents' app
-- @
--
-- N.B. This also resets the internal 'component' state. This means all currently
-- mounted components become unmounted and t'ComponentId' are reset to their
-- original form factory.
--
-- If you'd like to preserve application state between calls to GHCi `:r`, see 'live'.
--
-- @since 1.9.0.0
reload
  :: Eq model
  => Events
  -- ^ Event delegation map (typically 'Miso.Event.Types.defaultEvents')
  -> App model action
  -- ^ Top-level application component to (re-)mount
  -> IO ()
reload events = reloadWithContext events ()
-----------------------------------------------------------------------------
-- | Like 'reload', but seeds the app-global React-style @context@ with an
-- initial value (see 'Miso.startAppWithContext').
--
-- Use this instead of 'reload' when your top-level t'Component' uses a
-- non-trivial @context@, since 'reload' fixes the @context@ to @()@.
--
-- @
-- main :: IO ()
-- main = 'reloadWithContext' 'defaultEvents' Light app
-- @
--
-- @since 1.12.0.0
reloadWithContext
  :: forall context model action . (Eq context, Eq model)
  => Events
  -- ^ Event delegation map (typically 'Miso.Event.Types.defaultEvents')
  -> context
  -- ^ Initial app-global @context@
  -> Component context () model action
  -- ^ Top-level application component to (re-)mount
  -> IO ()
reloadWithContext events initialContext vcomp = do
  exists <- x_exists
  when (exists == 1) $ do
    (_, oldSchedulerRef, _) <- deRefStablePtr =<< x_get
    killThread =<< readIORef oldSchedulerRef
    x_clear
  clearPage
  -- 'reload' is a full reset: seed the freshly-supplied context.
  -- ('initComponent' writes 'globalContext' with the value we pass it.)
  void (initComponent events Draw False initialContext vcomp)
  x_store =<< newStablePtr (components, schedulerThread, globalContext :: IORef context)
-----------------------------------------------------------------------------
-- | Live reloading. Persists all t'Component' `model` between successive GHCi reloads.
--
-- This means application state should persist between GHCi reloads
--
-- Schema changes to 'model' are currently unsupported. If you're
-- changing fields in 'model' (adding, removing, changing a field's type), this
-- will more than likely segfault. If you change the 'view' or 'update' functions
-- it will be fine.
--
-- Use 'reload' if you're changing the 'model' frequently and 'live'
-- if you're adjusting the 'view' / 'update' function logic.
--
-- @
-- main :: IO ()
-- main = 'live' 'defaultEvents' app
-- @
--
-- @since 1.9.0.0
live
  :: Eq model
  => Events
  -- ^ Event delegation map (typically 'Miso.Event.Types.defaultEvents')
  -> App model action
  -- ^ Top-level application component to (re-)mount with preserved model state
  -> IO ()
live events = liveWithContext events ()
-----------------------------------------------------------------------------
-- | Like 'live', but seeds the app-global React-style @context@ with an
-- initial value (see 'Miso.startAppWithContext').
--
-- Use this instead of 'live' when your top-level t'Component' uses a
-- non-trivial @context@, since 'live' fixes the @context@ to @()@.
--
-- The seeded @context@ is only used on the initial load; on subsequent reloads
-- the preserved 'model' is recovered exactly as with 'live'.
--
-- @
-- main :: IO ()
-- main = 'liveWithContext' 'defaultEvents' Light app
-- @
--
-- @since 1.12.0.0
liveWithContext
  :: forall context model action . (Eq context, Eq model)
  => Events
  -- ^ Event delegation map (typically 'Miso.Event.Types.defaultEvents')
  -> context
  -- ^ Initial app-global @context@
  -> Component context () model action
  -- ^ Top-level application component to (re-)mount with preserved model state
  -> IO ()
liveWithContext events initialContext vcomp = do
  exists <- x_exists
  if exists == 1
    then do
      -- clearBody (only clear the body)
      clearBody

      -- Deref old state, update new state, set pointer in C heap.
      (oldComponentsRef, oldSchedulerRef, oldContextRef) <- deRefStablePtr =<< x_get
      oldContext <- readIORef oldContextRef
      killThread =<< readIORef oldSchedulerRef

      _oldState <- readIORef oldComponentsRef
      let oldModel = (_oldState IM.! topLevelComponentId) ^. componentModel
          initialVComp = vcomp { model = oldModel }

      -- Overwrite new components state with old components state.
      atomicWriteIORef components _oldState

      -- Perform initial draw, recovering the old model and the old context.
      -- ('initComponent' seeds 'globalContext' with the context we pass it.)
      initComponent events Draw True oldContext initialVComp

      -- Don't forget to flush (native mobile needs this too)
      FFI.flush

      -- Clear and set static ptr to use new state (new CAF state)
      x_clear
      x_store =<< newStablePtr (components, schedulerThread, globalContext :: IORef context)
    else do
      -- This means it is initial load, just store the pointer.
      void (initComponent events Draw False initialContext vcomp)
      x_store =<< newStablePtr (components, schedulerThread, globalContext :: IORef context)
-----------------------------------------------------------------------------
clearPage, clearBody, clearHead :: IO ()
clearPage = clearBody >> clearHead
clearBody = do
  body_ <- jsg "document" ! ("body" :: MisoString)
  setField body_ "innerHTML" ("" :: MisoString)
clearHead = do
  head_ <- jsg "document" ! ("head" :: MisoString)
  setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
