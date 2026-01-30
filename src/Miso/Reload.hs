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
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
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
-- @since 1.9.0.0
reload
  :: IO ()
  -- ^ An t'IO' action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
reload = (clear >>)
  where
    clear :: IO ()
    clear = do
      body_ <- jsg "document" ! ("body" :: MisoString)
      setField body_ "innerHTML" ("" :: MisoString)
      head_ <- jsg "document" ! ("head" :: MisoString)
      setField head_ "innerHTML" ("" :: MisoString)
-----------------------------------------------------------------------------
