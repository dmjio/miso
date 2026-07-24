-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Refresh.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Refresh.Property
  ( -- *** Property
    enableRefresh_
  ) where
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#enable-refresh
--
-- Determines if dragging down or calling @autoStartRefresh@ can trigger the
-- @startrefresh@ event.
--
-- > enableRefresh_ False
--
-- Default Value: 'True'
--
enableRefresh_ :: Bool -> Attribute action
enableRefresh_ = boolProp "enable-refresh"
-----------------------------------------------------------------------------
