-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Viewpager.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Viewpager.Method
  ( -- *** Methods
    selectTab
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | Params object for 'selectTab'.
data SelectTab = SelectTab Int Bool
-----------------------------------------------------------------------------
instance ToJSVal SelectTab where
  toJSVal (SelectTab i smooth) = do
    o <- create
    set "index" i o
    set "smooth" smooth o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#selecttab
--
-- Programmatically navigates to the specified page, optionally with animation.
--
-- > selectTab "#myPager" 2 True TabSelected TabSelectFailed
--
selectTab
  :: MisoString
  -> Int
  -- ^ Page index
  -> Bool
  -- ^ Whether to animate the transition
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
selectTab selector i smooth action =
  invokeExec "selectTab" selector (SelectTab i smooth) (\() -> action)
-----------------------------------------------------------------------------
