-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Refresh.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Refresh.Method
  ( -- *** Methods
    autoStartRefresh
  , finishRefresh
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#autostartrefresh
--
-- When @enable-refresh@ is true, exposes the entire \<refresh-header\>,
-- triggering the @startrefresh@ event.
--
-- > autoStartRefresh "#myRefresh" Started StartFailed
--
autoStartRefresh
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
autoStartRefresh selector action =
  invokeExec "autoStartRefresh" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#finishrefresh
--
-- Called after the @startrefresh@ event to end the refresh state, making the
-- \<refresh-header\> rebound.
--
-- > finishRefresh "#myRefresh" Finished FinishFailed
--
finishRefresh
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
finishRefresh selector action =
  invokeExec "finishRefresh" selector () (\() -> action)
-----------------------------------------------------------------------------
