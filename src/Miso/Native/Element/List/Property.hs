-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.List.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.List.Property
  ( -- *** Property
    -- *** Types
    ListOptions (..)
  , ScrollOrientation (..)
  , ListType (..)
  , ListItemSnapAlignment (..)
    -- *** Defaults
  , defaultListOptions
    -- *** Attributes
  , itemKey_
  , key_
  , enableScroll_
  , enableNestedScroll_
  , listMainAxisGap_
  , listCrossAxisGap_
  , sticky_
  , stickyOffset_
  , stickyTop_
  , stickyBottom_
  , bounces_
  , initialScrollIndex_
  , needVisibleItemInfo_
  , upperThresholdItemCount_
  , lowerThresholdItemCount_
  , scrollEventThrottle_
  , itemSnap_
  , needLayoutCompleteInfo_
  , layoutId_
  , preloadBufferCount_
  , scrollBarEnable_
  , reuseIdentifier_
  , fullSpan_
  , estimatedMainAxisSizePx_
  , recyclable_
  , updateAnimation_
  , harmonyScrollEdgeEffect_
  , experimentalRecycleStickyItem_
  ) where
-----------------------------------------------------------------------------
import Miso.JSON
import Miso.Property
import Miso.String (MisoString)
import Miso.Types (Attribute)
----------------------------------------------------------------------------
-- | ListOptions
data ListOptions
  = ListOptions
  { listType_ :: ListType
    -- ^ list-type: 'single' | 'flow' | 'waterfall'
  , spanCount_ :: Int
    -- ^ Sets the number of columns or rows for the \<list\> component layout
  , scrollOrientation_ :: ScrollOrientation
    -- ^ 'vertical' ｜ 'horizontal'
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | ScrollOrientation
data ScrollOrientation
  = Vertical
  | Horizontal
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON ScrollOrientation where
  toJSON Vertical   = "vertical"
  toJSON Horizontal = "horizontal"
-----------------------------------------------------------------------------
-- | ListType
data ListType
  = Single
  | Flow
  | Waterfall
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON ListType where
  toJSON Single    = "single"
  toJSON Flow      = "flow"
  toJSON Waterfall = "waterfall"
-----------------------------------------------------------------------------
-- | defaultListOptions
defaultListOptions :: ListOptions
defaultListOptions
  = ListOptions
  { listType_ = Single
  , spanCount_ = 0
  , scrollOrientation_ = Vertical
  }
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#required-item-key
--
-- The item-key attribute is a required attribute on \<list-item\>.
-- 
--
itemKey_ :: MisoString -> Attribute action
itemKey_ = textProp "item-key"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#enable-scroll
--
-- Indicates whether the \<list\> component is allowed to scroll.
--
enableScroll_ :: Bool -> Attribute action
enableScroll_ = boolProp "enable-scroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#enable-nested-scroll
--
-- Indicates whether \<list\> can achieve nested scrolling with other scrollable containers. When enabled, the inner container scrolls first, followed by the outer container.
--
enableNestedScroll_ :: Bool -> Attribute action
enableNestedScroll_ = boolProp "enable-nested-scroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#list-main-axis-gap
--
-- Specifies the spacing of \<list\> child nodes in the main axis direction,
-- which needs to be written in the style.
--
listMainAxisGap_ :: MisoString -> Attribute action
listMainAxisGap_ = textProp "list-main-axis-gap"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#list-cross-axis-gap
--
-- Specifies the spacing of <list> child nodes in the cross axis direction,
-- which needs to be written in the style.
--
listCrossAxisGap_ :: MisoString -> Attribute action
listCrossAxisGap_ = textProp "list-cross-axis-gap"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#sticky
--
-- Declared on the \<list\> component to control whether the \<list\> component
-- as a whole is allowed to be sticky at the top or bottom.
--
sticky_ :: Bool -> Attribute action
sticky_ = boolProp "sticky"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#sticky-offset
--
-- The offset distance from the top or bottom of \<list\> for sticky positioning, in 'px'.
--
stickyOffset_ :: Int -> Attribute action
stickyOffset_ = intProp "sticky-offset"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#sticky-top
--
-- Declared on the \<list-item\> child node to control whether the node will
-- be sticky at the top.
--
stickyTop_ :: Bool -> Attribute action
stickyTop_ = boolProp "sticky-top"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#sticky-bottom
--
-- Declared on the \<list-item\> child node to control whether the node
-- will be sticky at the bottom.
--
stickyBottom_ :: Bool -> Attribute action
stickyBottom_ = boolProp "sticky-bottom"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#bounces
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
-- | https://lynxjs.org/api/elements/built-in/list.html#initial-scroll-index
--
-- Specifies the node position to which \<list\> automatically scrolls after
-- rendering effective only once.
--
initialScrollIndex_ :: Int -> Attribute action
initialScrollIndex_ = intProp "initial-scroll-index"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#need-visible-item-info
--
-- Controls whether the scroll event callback parameters include the position
-- information of the currently rendering node.
--
-- The scroll events include:
--  * `scroll`
--  * `scrolltoupper`
--  * `scrolltolower`
--
-- Default value: 'False'
--
needVisibleItemInfo_ :: Bool -> Attribute action
needVisibleItemInfo_ = boolProp "need-visible-item-info"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#upper-threshold-item-count
-- 
-- Triggers a `scrolltoupper` event once when the number of remaining displayable
-- child nodes at the top of \<list\> is less than `upper-threshold-item-count`
-- for the first time.
--
upperThresholdItemCount_ :: Int -> Attribute action
upperThresholdItemCount_ = intProp "upper-threshold-item-count"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#lower-threshold-item-count
-- 
-- Triggers a `scrolltolower` event once when the number of remaining
-- displayable child nodes at the bottom of \<list\> is less than
-- `lower-threshold-item-count` for the first time.
--
lowerThresholdItemCount_ :: Int -> Attribute action
lowerThresholdItemCount_ = intProp "lower-threshold-item-count"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scroll-event-throttle
-- 
-- Sets the time interval for the \<list\> callback `scroll` event, in milliseconds (ms).
-- By default, the scroll event is called back every 200 ms.
--
-- Default Value: 200
--
scrollEventThrottle_ :: Int -> Attribute action
scrollEventThrottle_ = intProp "scroll-event-throttle"
-----------------------------------------------------------------------------
data ListItemSnapAlignment
  = ListItemSnapAlignment
  { factor :: Int
  , offset :: Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON ListItemSnapAlignment where
  toJSON ListItemSnapAlignment {..}
    = object
    [ "factor" .= factor
    , "offset" .= offset
    ]
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#item-snap
-- 
-- Used to mark the unique identifier for this data source update, which
-- will be returned in the [`layoutcomplete`](#layoutcomplete) event callback.
--
-- - `factor`: The parameter for paginated positioning, with a range of `[0, 1]`.
-- - `offset`: Additional `offset` parameter added on top of `factor`.
--
itemSnap_ :: ListItemSnapAlignment -> Attribute action
itemSnap_ = prop "item-snap"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#need-layout-complete-info
-- 
-- Controls whether the layoutcomplete event includes the node layout information
-- before and after this layout, the \<list\> Diff information that triggered this
-- layout, and the current \<list\> scroll state information.
--
needLayoutCompleteInfo_ :: Bool -> Attribute action
needLayoutCompleteInfo_ = prop "need-layout-complete-info"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#layout-id
-- 
-- Used to mark the unique identifier for this data source update,
-- which will be returned in the layoutcomplete event callback.
--
-- Default Value: -1
--
layoutId_ :: Int -> Attribute action
layoutId_ = prop "layout-id"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#preload-buffer-count
-- 
-- This attribute controls the number of nodes outside \<list\> that are preloaded.
--
-- Default Value: 0
--
preloadBufferCount_ :: Int -> Attribute action
preloadBufferCount_ = intProp "preload-buffer-count"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scroll-bar-enable
--
-- *iOS* only
--
-- Indicates whether the \<list\> component scroll bar is displayed.
--
-- Default value: 'True'
--
scrollBarEnable_ :: Bool -> Attribute action
scrollBarEnable_ = boolProp "scroll-bar-enable"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#reuse-identifier
--
-- Sets the reuse id for \<list-item\>. When rendering child nodes, the \<list\>
-- component reuses \<list-item\> based on the reuse-identifier attribute value.
-- Only \<list-item\> with the same reuse-identifier attribute value will be reused.
--
reuseIdentifier_ :: MisoString -> Attribute action
reuseIdentifier_ = textProp "reuse-identifier"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#full-span
--
-- The full-span attribute is used to indicate that a \<list-item\>
-- occupies a full row or column.
--
fullSpan_ :: Bool -> Attribute action
fullSpan_ = boolProp "full-span"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#estimated-main-axis-size-px
--
-- Specifies the placeholder size in the main axis direction for \<list-item\>
-- before it is fully rendered, in px. If not set, the default value is the size
-- of \<list\> in the main axis direction.
--
estimatedMainAxisSizePx_ :: Int -> Attribute action
estimatedMainAxisSizePx_ = intProp "estimated-main-axis-size-px"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#recyclable
--
-- Declared on the \<list-item\> node to control whether the node can be recycled.
--
-- > recyclable_ False
--
-- Default Value: 'True'
--
recyclable_ :: Bool -> Attribute action
recyclable_ = boolProp "recyclable"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#update-animation
--
-- Controls the animation behavior during data source updates.
-- One of @\"default\"@ | @\"none\"@.
--
-- > updateAnimation_ "none"
--
updateAnimation_ :: MisoString -> Attribute action
updateAnimation_ = textProp "update-animation"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#harmony-scroll-edge-effect
--
-- *Harmony* only
--
-- When the contentSize is smaller than its own container size, sets whether
-- to enable the scroll (edge) effect.
--
-- > harmonyScrollEdgeEffect_ False
--
-- Default Value: 'True'
--
harmonyScrollEdgeEffect_ :: Bool -> Attribute action
harmonyScrollEdgeEffect_ = boolProp "harmony-scroll-edge-effect"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#experimental-recycle-sticky-item
--
-- Enables recycling of sticky nodes when they are pushed out of the view area.
--
-- > experimentalRecycleStickyItem_ False
--
-- Default Value: 'True'
--
experimentalRecycleStickyItem_ :: Bool -> Attribute action
experimentalRecycleStickyItem_ = boolProp "experimental-recycle-sticky-item"
-----------------------------------------------------------------------------
