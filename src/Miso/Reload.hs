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
reload action = do
    clear
#if __GLASGOW_HASKELL__ > 865
    threads <- listThreads
    forM_ threads $ \threadId -> do
      threadLabel threadId >>= \case
        Just "scheduler" ->
          killThread threadId
        _ -> pure ()
#endif
    action
  where
    clear :: IO ()
    clear = resetComponentState $ do
      body_ <- jsg "document" ! ("body" :: MisoString)
      setField body_ "innerHTML" ("" :: MisoString)
      head_ <- jsg "document" ! ("head" :: MisoString)
      setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
