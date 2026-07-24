-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.TitleBarView.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.TitleBarView.Property
  ( -- *** Property
    moveable_
  ) where
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/title-bar-view.html#moveable
--
-- When set to true, dragging the title bar view moves the window.
--
-- > moveable_ True
--
-- Default Value: 'False'
--
moveable_ :: Bool -> Attribute action
moveable_ = boolProp "moveable"
-----------------------------------------------------------------------------
