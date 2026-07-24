-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.ScrollCoordinator.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.ScrollCoordinator.Method
  ( -- *** Methods
    setFoldExpanded
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | Params object for 'setFoldExpanded'.
data SetFoldExpanded = SetFoldExpanded MisoString Bool
-----------------------------------------------------------------------------
instance ToJSVal SetFoldExpanded where
  toJSVal (SetFoldExpanded offset smooth) = do
    o <- create
    set "offset" offset o
    set "smooth" smooth o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#setfoldexpanded
--
-- Adjusts the fold expansion state, optionally with animation. The @offset@ is
-- a @px@ / @rpx@ value, e.g. @\"100px\"@.
--
-- > setFoldExpanded "#coordinator" "100px" True Expanded ExpandFailed
--
setFoldExpanded
  :: MisoString
  -> MisoString
  -- ^ Offset (px \/ rpx)
  -> Bool
  -- ^ Whether to animate
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
setFoldExpanded selector offset smooth action =
  invokeExec "setFoldExpanded" selector (SetFoldExpanded offset smooth) (\() -> action)
-----------------------------------------------------------------------------
