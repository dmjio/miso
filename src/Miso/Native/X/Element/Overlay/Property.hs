-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Overlay.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Overlay.Property
  ( -- *** Property
    iosEnableSwipeBack_
  , level_
  , mode_
  , visible_
    -- *** Types
  , OverlayLevel (..)
  , OverlayMode (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..))
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | Layer level of an \<overlay\>, used by 'level_'.
data OverlayLevel
  = Level1
  | Level2
  | Level3
  | Level4
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON OverlayLevel where
  toJSON Level1 = toJSON (1 :: Int)
  toJSON Level2 = toJSON (2 :: Int)
  toJSON Level3 = toJSON (3 :: Int)
  toJSON Level4 = toJSON (4 :: Int)
-----------------------------------------------------------------------------
-- | The level at which the overlay content resides (iOS), used by 'mode_'.
-- 'ModeOther' carries a custom client class name.
data OverlayMode
  = ModeWindow
  | ModeTop
  | ModePage
  | ModeOther MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON OverlayMode where
  toJSON ModeWindow      = "window"
  toJSON ModeTop         = "top"
  toJSON ModePage        = "page"
  toJSON (ModeOther cls) = toJSON cls
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#ios-enable-swipe-back
--
-- *iOS* only. When the overlay is displayed, whether swiping right closes the
-- current page.
--
-- Default Value: 'False'
--
iosEnableSwipeBack_ :: Bool -> Attribute action
iosEnableSwipeBack_ = boolProp "ios-enable-swipe-back"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#level
--
-- Layer level, from 'Level1' to 'Level4'. The larger the value, the closer it
-- is to the bottom.
--
-- > level_ Level2
--
-- Default Value: 'Level1'
--
level_ :: OverlayLevel -> Attribute action
level_ = prop "level"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#mode
--
-- *iOS* only. The level at which the overlay content resides. Use 'ModeOther'
-- for a custom client class name.
--
-- > mode_ ModeTop
--
-- Default Value: 'ModeWindow'
--
mode_ :: OverlayMode -> Attribute action
mode_ = prop "mode"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#visible
--
-- Controls whether the overlay is displayed.
--
-- Default Value: 'False'
--
visible_ :: Bool -> Attribute action
visible_ = boolProp "visible"
-----------------------------------------------------------------------------
