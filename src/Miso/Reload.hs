-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
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
-- Support for live-reload of miso applications.
----------------------------------------------------------------------------
module Miso.Reload
  ( -- ** Live reload
    reload
#ifdef WASM
  , live
#endif
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Runtime (resetComponentState)
import           Miso.DSL (jsg, (!), setField)
-----------------------------------------------------------------------------
#ifdef WASM
import           Miso.Runtime (components, initialize, topLevelComponentId, Hydrate(Draw))
import           Miso.Types (Component, Events)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
import           Data.IORef
import           Foreign
import           Foreign.C.Types
-----------------------------------------------------------------------------
-- | Foreign imports using t'StablePtr'
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
#endif
-- | Clears the <body> and <head> on each reload.
--
-- Meant to be used with WASM browser mode.
--
-- This function executes an t'IO' action, clearing both the body and head
-- of the current page in the process.
--
-- @
-- main :: IO ()
-- main = reload (startApp defaultEvents app)
-- @
--
-- N.B. This also resets the internal 'component' state. This means all currently
-- mounted components become unmounted and t'ComponentId' are reset to their
-- original form factory.
--
-- @since 1.9.0.0
reload
  :: IO ()
  -- ^ An t'IO' action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
reload action = clear >> action
  where
    clear :: IO ()
    clear = resetComponentState $ do
      body_ <- jsg "document" ! ("body" :: MisoString)
      setField body_ "innerHTML" ("" :: MisoString)
      head_ <- jsg "document" ! ("head" :: MisoString)
      setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
#ifdef WASM
-- | Live reloading.
live
  :: (Eq parent, Eq model)
  => Component parent model action
  -> Events
  -> IO ()
live vcomp events = do
  exists <- x_exists
  if exists == 1
    then do
      -- dmj: Read the original ComponentState
      -- Initialize the new ComponentState
      -- Overwrite new models w/ old models (t'Dynamic' diff them eventually ...)

      -- Deref old state, update new state, set pointer in C heap.
      _oldState <- readIORef =<< deRefStablePtr =<< x_get
      _ <- initialize events topLevelComponentId Draw False vcomp FFI.getBody
      _newState <- readIORef components
      -- Update current global 'components' with the results of oldstate

      -- TODO: Do the "swap" here.
      diffModels _oldState _newState
      -- update old state with new state...

      -- Set static ptr to use new state
      x_store =<< newStablePtr components <* x_clear

      -- Set all model to dirty and call `renderComponents`.
    else
      -- dmj: This means its initial load, just store the pointer.
      x_store =<< newStablePtr components
  where
    -- dmj: TODO implement, put old Component model into new Component model
    diffModels _ _ = pure ()
-----------------------------------------------------------------------------
#endif
