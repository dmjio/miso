-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Reload
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Support for live reloading of miso applications.
--
-- = Live Reload
--
-- This module exposes two functions meant to be used during interactive
-- development with GHC WASM browser mode, 'live' and 'reload'.
--
-- == Reload
--
-- Use 'reload' if you'd like to redraw the page on each file change, resetting
-- the working application state.
--
-- @
-- main :: IO ()
-- main = 'reload' 'defaultEvents' app
-- @
--
-- == Live
--
-- Use 'live' if you'd like to persist the working application state (all 'Component' 'model')
-- between GHCi reloads. This only works if you do not alter the 'model' schema (e.g. add, remove, change a field's type).
--
-- @
-- main :: IO ()
-- main = 'live' 'defaultEvents' app
-- @
--
-- See the [miso-sampler](https://github.com/haskell-miso/miso-sampler) for example usage.
--
----------------------------------------------------------------------------
module Miso.Reload
  ( -- ** Functions
    reload
  , live
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
#if __GLASGOW_HASKELL__ > 865
import           GHC.Conc.Sync (threadLabel)
import           GHC.Conc (listThreads, killThread)
#endif
-----------------------------------------------------------------------------
#ifdef WASM
import           Miso.DSL.TH.File (evalFile)
#endif
import           Miso.DSL ((!), jsg, setField)
import qualified Miso.FFI.Internal as FFI
import           Miso.Types (Component(..), Events)
import           Miso.String (MisoString)
import           Miso.Runtime (componentModel, initComponent, topLevelComponentId, resetComponentState, Hydrate(..))
import           Miso.Runtime.Internal (components)
-----------------------------------------------------------------------------
import           Miso.Lens
-----------------------------------------------------------------------------
import qualified Data.IntMap.Strict as IM
import           Data.IORef
import           Foreign hiding (void)
import           Foreign.C.Types
-----------------------------------------------------------------------------
foreign import ccall unsafe "x_store"
  x_store :: StablePtr a -> IO ()
-----------------------------------------------------------------------------
foreign import ccall unsafe "x_get"
  x_get :: IO (StablePtr a)
-----------------------------------------------------------------------------
foreign import ccall unsafe "x_exists"
  x_exists :: IO CInt
-----------------------------------------------------------------------------
foreign import ccall unsafe "x_clear"
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
  :: (Eq parent, Eq model)
  => Events
  -> Component parent model action
  -> IO ()
reload events vcomp = do
#ifdef WASM
    $(evalFile MISO_JS_PATH)
#endif
    resetComponentState clearPage
#if __GLASGOW_HASKELL__ > 865
    threads <- listThreads
    forM_ threads $ \threadId -> do
      threadLabel threadId >>= \case
        Just "scheduler" ->
          killThread threadId
        _ -> pure ()
#endif
    initComponent events Draw vcomp
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
  :: (Eq parent, Eq model)
  => Events
  -> Component parent model action
  -> IO ()
live events vcomp = do
  exists <- x_exists
  if exists == 1
    then do
#if __GLASGOW_HASKELL__ > 865
      threads <- listThreads
      forM_ threads $ \threadId -> do
        threadLabel threadId >>= \case
          Just "scheduler" ->
            killThread threadId
          _ -> pure ()
#endif
      -- clearPage (perform this with the context)
      clearPage

      -- Deref old state, update new state, set pointer in C heap.
      _oldState <- readIORef =<< deRefStablePtr =<< x_get

      let oldModel = (_oldState IM.! topLevelComponentId) ^. componentModel
          initialVComp = vcomp { model = oldModel }

      -- Overwrite new components state with old components state
      atomicWriteIORef components _oldState

      -- Perform initial draw, this will fetch the model from the old component state
      -- and overwrite the old state with the new state for everything else.
      initComponent events Draw initialVComp
      
      -- Don't forget to flush (native mobile needs this too)
      FFI.flush

      -- Clear and set static ptr to use new state
      x_clear
      x_store =<< newStablePtr components
    else do
      -- This means it is initial load, just store the pointer.
#ifdef WASM
      $(evalFile MISO_JS_PATH)
#endif
      x_store =<< newStablePtr components
      void (initComponent events Draw vcomp)
-----------------------------------------------------------------------------
clearPage :: IO ()
clearPage = do
  body_ <- jsg "document" ! ("body" :: MisoString)
  setField body_ "innerHTML" ("" :: MisoString)
  head_ <- jsg "document" ! ("head" :: MisoString)
  setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
