{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Delegate
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Delegate
  ( delegator
  , undelegator
  ) where

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Map.Strict as M
import           Miso.FFI
import           Miso.Html.Types
import           Miso.String

-- | Entry point for event delegation
delegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> IO ()
delegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  delegateEvent mountPointElement evts $ do
    VTree (JSObject val) <- readIORef vtreeRef
    pure val

-- | Entry point for deinitalizing event delegation
undelegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> IO ()
undelegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  undelegateEvent mountPointElement evts $ do
    VTree (JSObject val) <- readIORef vtreeRef
    pure val
