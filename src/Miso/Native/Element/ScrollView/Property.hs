-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.ScrollView.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.ScrollView.Property
  ( -- *** Property
    scrollOrientation_
  , enableScroll_
  , initialScrollOffset_
  , initialScrollToIndex_
  , bounces_
  , upperThreshold_
  , lowerThreshold_
  , scrollBarEnable_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scroll-orientation
--
-- Set scroll orientation for the scrollable container.
--
-- Default Value: "vertical"
--
scrollOrientation_ :: MisoString -> Attribute action
scrollOrientation_ = textProp "scroll-orientation"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#enable-scroll
--
-- Sets whether to allow gesture dragging to scroll. Supports dynamic switching
-- and takes effect on the next gesture. When scrolling is disabled, the
-- user cannot scroll manually.
--
-- Default Value: True
--
enableScroll_ :: Bool -> Attribute action
enableScroll_ = boolProp "enable-scroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#initial-scroll-offset
--
-- Sets the absolute content offset distance during initial rendering
-- (different from the offset concept in the scrollTo method). The horizontal
-- or vertical direction is determined by `scroll-orientation`, and it only takes
-- effect during the first render execution, not responding to subsequent changes.
--
initialScrollOffset_ :: MisoString -> Attribute action
initialScrollOffset_ = textProp "initial-scroll-offset"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#initial-scroll-to-index
--
-- Sets the child node to be positioned during initial rendering, only taking
-- effect during the first render execution and not responding to subsequent changes.
--
initialScrollToIndex_ :: MisoString -> Attribute action
initialScrollToIndex_ = textProp "initial-scroll-to-index"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#bounces
--
-- *iOS* only
--
-- Declared on the \<list-item\> child node to control whether the node
-- will be sticky at the bottom.
--
-- Default value: 'True'
--
bounces_ :: Bool -> Attribute action
bounces_ = boolProp "bounces"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#upper-threshold
--
-- Sets a scroll threshold (unit: `px`), indicating how far from the top
-- or left before triggering the `scrolltoupper` event.
--
upperThreshold_ :: MisoString -> Attribute action
upperThreshold_ = textProp "upper-threshold"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#lower-threshold
--
-- Sets a scroll threshold (unit: px), indicating how far from the top
-- or left before triggering the scrolltolower event.
--
lowerThreshold_ :: MisoString -> Attribute action
lowerThreshold_ = textProp "lower-threshold"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-view.html#scroll-bar-enable>
--
-- Enables the scrollbar, supporting dynamic switching.
--
-- Default Value: False
--
scrollBarEnable_ :: Bool -> Attribute action
scrollBarEnable_ = boolProp "scroll-bar-enable"
-----------------------------------------------------------------------------
