-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
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
import           Control.Monad
#if __GLASGOW_HASKELL__ > 865
import           GHC.Conc.Sync (threadLabel)
import           GHC.Conc (listThreads, killThread)
#endif
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Runtime (resetComponentState)
import           Miso.DSL (jsg, (!), setField)
-----------------------------------------------------------------------------
#ifdef WASM
import           Miso.Runtime
import           Miso.Types (Component(..), Events)
import qualified Miso.FFI.Internal as FFI
import           Miso.Lens
-----------------------------------------------------------------------------
import qualified Data.IntMap.Strict as IM
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
reload action = resetComponentState clearPage >> action
reload action = do
    clearPage
#if __GLASGOW_HASKELL__ > 865
    threads <- listThreads
    forM_ threads $ \threadId -> do
      threadLabel threadId >>= \case
        Just "scheduler" ->
          killThread threadId
        _ -> pure ()
#endif
    action
-----------------------------------------------------------------------------
#ifdef WASM
-- | Live reloading. Attempts to persist the working t'Component' state.
--
-- Some caveats, if you're changing fields in 'model' (add / remove / modifying a field), this
-- will more than likely segfault. If you change the 'view' or 'update' functions
-- it should be fine.
--
-- dmj: I recommend using 'reload' if you're changing the 'model' often and 'live'
-- if you're adjusting the 'view' / 'update' function logic.
--
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

      -- Drop old stuff
      clearPage

      -- Deref old state, update new state, set pointer in C heap.
      _oldState <- readIORef =<< deRefStablePtr =<< x_get
      let oldModel = (_oldState IM.! topLevelComponentId) ^. componentModel
      let initialVComp = vcomp { model = oldModel }
      atomicWriteIORef components _oldState
      _ <- initialize events rootComponentId Draw False initialVComp FFI.getBody

      -- don't forget to flush (native mobile needs this too)
      FFI.flush

      -- Set static ptr to use new state
      x_store =<< newStablePtr components <* x_clear

      -- Set all model to dirty and call `renderComponents`.
    else
      -- dmj: This means its initial load, just store the pointer.
      x_store =<< newStablePtr components
-----------------------------------------------------------------------------
#endif
-----------------------------------------------------------------------------
clearPage :: IO ()
clearPage = do
  body_ <- jsg "document" ! ("body" :: MisoString)
  setField body_ "innerHTML" ("" :: MisoString)
  head_ <- jsg "document" ! ("head" :: MisoString)
  setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------