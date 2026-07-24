-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.ScrollCoordinator.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.ScrollCoordinator.Property
  ( -- *** Property
    androidNestedScrollAsChild_
  , bounces_
  , enableScroll_
  , enableScrollBar_
  , granularity_
  , headerOverSlot_
  , iosForceScrollDetach_
  , iosScrollsToTop_
  , refreshMode_
    -- *** Types
  , RefreshMode (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..))
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | The pull-to-refresh mode of the foldview (iOS), used by 'refreshMode_'.
data RefreshMode
  = RefreshNone
  | RefreshPage
  | RefreshFold
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON RefreshMode where
  toJSON RefreshNone = "none"
  toJSON RefreshPage = "page"
  toJSON RefreshFold = "fold"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#android-nested-scroll-as-child
--
-- *Android* only. Enables nested scroll behavior as a child element in other
-- scrolling widgets.
--
-- Default Value: 'False'
--
androidNestedScrollAsChild_ :: Bool -> Attribute action
androidNestedScrollAsChild_ = boolProp "android-nested-scroll-as-child"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#bounces
--
-- *iOS \/ Harmony* only. Enables the bounce effect when scrolling past the
-- boundary.
--
-- Default Value: 'True'
--
bounces_ :: Bool -> Attribute action
bounces_ = boolProp "bounces"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#enable-scroll
--
-- Controls whether vertical scrolling is permitted.
--
-- Default Value: 'True'
--
enableScroll_ :: Bool -> Attribute action
enableScroll_ = boolProp "enable-scroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#enable-scroll-bar
--
-- *iOS \/ Harmony* only. Determines scrollbar visibility during coordinator
-- scrolling.
--
-- Default Value: 'False'
--
enableScrollBar_ :: Bool -> Attribute action
enableScrollBar_ = boolProp "enable-scroll-bar"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#granularity
--
-- The event response granularity of @bindoffset@.
--
-- Default Value: 0.01
--
granularity_ :: Double -> Attribute action
granularity_ = doubleProp "granularity"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#header-over-slot
--
-- Controls header layering hierarchy relative to slot content.
--
-- Default Value: 'False'
--
headerOverSlot_ :: Bool -> Attribute action
headerOverSlot_ = boolProp "header-over-slot"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#ios-force-scroll-detach
--
-- *iOS* only. Forces @nested-vertical-scroll-behavior@ invalidation.
--
-- Default Value: 'False'
--
iosForceScrollDetach_ :: Bool -> Attribute action
iosForceScrollDetach_ = boolProp "ios-force-scroll-detach"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#ios-scrolls-to-top
--
-- *iOS* only. Enables status-bar tap-to-scroll-top functionality.
--
-- Default Value: 'False'
--
iosScrollsToTop_ :: Bool -> Attribute action
iosScrollsToTop_ = boolProp "ios-scrolls-to-top"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#refresh-mode
--
-- *iOS* only. The pull-to-refresh mode of the foldview.
--
-- > refreshMode_ RefreshFold
--
-- Default Value: 'RefreshNone'
--
refreshMode_ :: RefreshMode -> Attribute action
refreshMode_ = prop "refresh-mode"
-----------------------------------------------------------------------------
