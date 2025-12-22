-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Run
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Support for running and live-reloading of miso applications.
----------------------------------------------------------------------------
module Miso.Run
  ( -- ** Live reload
    run
  , reload
  ) where
-----------------------------------------------------------------------------
import           Miso.String
import           Miso.DSL
-----------------------------------------------------------------------------
-- | Entry point for a miso application.
--
run
  :: IO ()
  -- ^ An t'IO' action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
run = id
-----------------------------------------------------------------------------
-- | Like 'run', but clears the <body> and <head> on each reload.
--
-- Meant to be used with WASM browser mode
--
-- @since 1.9.0.0
reload
  :: IO ()
  -- ^ A JSM action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
reload action = run (clear >> action)
  where
    clear :: IO ()
    clear = do
      body_ <- jsg "document" ! ("body" :: MisoString)
      setField body_ "innerHTML" ("" :: MisoString)
      head_ <- jsg "document" ! ("head" :: MisoString)
      setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
