-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.View.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.View.Property
  ( -- *** Property
    id_
  , name_
  , className_
  , flatten_
  , exposureId_
  , exposureScene_
  , exposeUIMarginTop_
  , exposeUIMarginBottom_
  , exposeUIMarginLeft_
  , exposeUIMarginRight_
  , exposeScreenMarginTop_
  , exposeScreenMarginBottom_
  , exposeScreenMarginLeft_
  , exposeScreenMarginRight_
  , exposureArea_
  , enableExposureUIMargin_
  , accessibilityElement_
  , accessibilityLabel_
  , accessibilityTrait_
  , accessibilityElements_
  , accessibilityElementsA11y_
  , accessibilityElementsHidden_
  , accessibilityExclusiveFocus_
  , a11yId_
  , iosPlatformAccessibilityId_
  , userInteractionEnabled_
  , nativeInteractionEnabled_
  , blockNativeEvent_
  , blockNativeEventAreas_
  , consumeSlideEvent_
  , enableTouchPseudoPropagation_
  , hitSlop_
  , ignoreFocus_
  , eventThrough_
  , iosEnableSimultaneousTouch_
  , lynxTimingFlag_
  ) where
-----------------------------------------------------------------------------
import           Miso.Property
import           Miso.Types
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#id
--
-- Used to specify the name of the element, generally for native to operate the corresponding node from the native side through `findViewByName`.
--
-- > id_ "test"
--
id_ :: MisoString -> Attribute action
id_ = textProp "id"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#name
--
-- Used to specify the name of the element, generally for native to operate the corresponding node from the native side through `findViewByName`.
--
-- > name_ "test"
--
name_ :: MisoString -> Attribute action
name_ = textProp "name"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#classname
--
-- Use `className` to set CSS class names, equivalent to 'class_'
--
-- > className_ "foo"
--
className_ :: MisoString -> Attribute action
className_ = textProp "className_"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#flatten
--
-- *Android* only
--
-- Only available on Android platform, used to force specific nodes to create
-- corresponding Android Views.
--
-- > flatten_ True
--
flatten_ :: Bool -> Attribute action
flatten_ = boolProp "flatten"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-id
--
-- Specify whether the target node needs to listen to [exposure/anti-exposure](https://lynxjs.org/guide/interaction/visibility-detection/exposure-ability.html#monitor-exposure-of-the-entire-page) events.
--
-- > exposureId_ "id-goes-here"
--
exposureId_ :: MisoString -> Attribute action
exposureId_ = textProp "exposure-id"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-scene
--
-- Specify the exposure scene of the target node, and use it together with
-- 'exposureId_' to uniquely identify the node that needs to monitor exposure.
--
-- > exposureScene_ "example-scene"
--
exposureScene_ :: MisoString -> Attribute action
exposureScene_ = textProp "exposure-scene"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-ui-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeUIMarginTop_ "10px"
--
-- Default Value: "0px"
--
exposeUIMarginTop_ :: MisoString -> Attribute action
exposeUIMarginTop_ = textProp "exposure-ui-margin-top"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-ui-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeUIMarginBottom_ "10px"
--
-- Default Value: "0px"
--
exposeUIMarginBottom_ :: MisoString -> Attribute action
exposeUIMarginBottom_ = textProp "exposure-ui-margin-bottom"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-ui-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeUIMarginLeft_ "10px"
--
-- Default Value: "0px"
--
exposeUIMarginLeft_ :: MisoString -> Attribute action
exposeUIMarginLeft_ = textProp "exposure-ui-margin-left"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-ui-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeUIMarginRight_ "10px"
--
-- Default Value: "0px"
--
-----------------------------------------------------------------------------
exposeUIMarginRight_ :: MisoString -> Attribute action
exposeUIMarginRight_ = textProp "exposure-screen-margin-right"
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-screen-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeUIMarginTop_ "10px"
--
-- Default Value: "0px"
--
exposeScreenMarginTop_ :: MisoString -> Attribute action
exposeScreenMarginTop_ = textProp "exposure-screen-margin-top"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-screen-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeScreenMarginBottom_ "10px"
--
-- Default Value: "0px"
--
exposeScreenMarginBottom_ :: MisoString -> Attribute action
exposeScreenMarginBottom_ = textProp "exposure-screen-margin-bottom"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-screen-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeScreenMarginLeft_ "10px"
--
-- Default Value: "0px"
--
exposeScreenMarginLeft_ :: MisoString -> Attribute action
exposeScreenMarginLeft_ = textProp "exposure-screen-margin-left"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-screen-margin-
--
-- Specify the boundary scaling value of the target node itself in the exposure
-- detection, which affects the viewport intersection judgment of the target node.
-- Each node can have its own boundary scaling value.
--
-- > exposeScreenMarginRight_ "10px"
--
-- Default Value: "0px"
--
exposeScreenMarginRight_ :: MisoString -> Attribute action
exposeScreenMarginRight_ = textProp "exposure-screen-margin-right"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#exposure-area
--
-- Specify the viewport intersection ratio of the target node that can trigger
-- the exposure event. When it is greater than this ratio, the exposure event
-- is triggered. When it is less than this ratio, the reverse exposure event
-- is triggered. By default, the exposure event is triggered when the target
-- node is exposed.
--
-- > exposureArea_ (pct 10)
--
-- Default Value: "0%"
--
exposureArea_ :: MisoString -> Attribute action
exposureArea_ = textProp "exposure-area"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#enable-exposure-ui-margin
--
-- Specify whether the target node supports the [exposure-ui-margin-*](https://lynxjs.org/api/elements/built-in/view.html#exposure-ui-margin-) properties.
--
-- Setting it to true will change the behavior of [exposure-screen-margin-*](https://lynxjs.org/api/elements/built-in/view.html#exposure-screen-margin-) and may cause the lazy loading of the scrollable container to fail.
--
-- > enableExposureUIMargin_ True
--
-- Default Value: 'False'
--
enableExposureUIMargin_ :: Bool -> Attribute action
enableExposureUIMargin_ = boolProp "enable-exposure-ui-margin"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-element
--
-- Set whether the node supports accessibility.
--
-- > accessibilityElement_ True
--
accessibilityElement_ :: Bool -> Attribute action
accessibilityElement_ = boolProp "accessibility-element"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-label
--
-- Set the content of the node voice broadcast.
--
-- If the \<text\> node does not set this attribute, the \<text\> node defaults to the \<text\> content.
--
-- > accessibilityLabel_ "some-label"
--
accessibilityLabel_ :: MisoString -> Attribute action
accessibilityLabel_ = textProp "accessibility-label"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-trait
--
-- Set the type characteristics of the node. The system will have specific
-- supplements to the playback content for different types of nodes.
--
-- > accessibilityTrait_ "button"
--
-- Default Value: "button"
--
accessibilityTrait_ :: MisoString -> Attribute action
accessibilityTrait_ = textProp "accessibility-trait"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-elements
--
-- Customize the focus order of child nodes. This property is set on the parent node,
-- and the focus order of its child nodes will be focused according to the
-- order of the child node `id` specified by the `accessibility-elements` property.
--
-- > accessibilityElements_ "view-3,view-2,view-5,view-1,view-4"
--
accessibilityElements_ :: MisoString -> Attribute action
accessibilityElements_ = textProp "accessibility-elements"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-elements-a11y
--
-- The same as @accessibilityElements_@, but the corresponding @id_@ is @a11yId_@.
--
-- > accessibilityElementsA11y_ "id"
--
accessibilityElementsA11y_ :: MisoString -> Attribute action
accessibilityElementsA11y_ = textProp "accessibility-elements-a11y"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-exclusive-hidden
--
-- Marks the current node and all its child nodes as non-accessible nodes.
--
-- > accessibilityExclusiveHidden_ True
--
-- Default Value: 'True'
--
accessibilityElementsHidden_ :: Bool -> Attribute action
accessibilityElementsHidden_ = boolProp "accessibilityElementsHidden_"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#accessibility-exclusive-focus
--
-- This property can be set for any node. In accessibility mode, sequential navigation will only focus on the child nodes under these nodes.
--
-- > accessibilityExclusiveFocus_ True
--
-- Default Value: 'True'
--
accessibilityExclusiveFocus_ :: Bool -> Attribute action
accessibilityExclusiveFocus_ = boolProp "accessibility-exclusive-focus"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#a11y-id
--
-- Different from `id`, it is used to identify barrier-free nodes separately.
--
-- > a11yId_ "test"
--
a11yId_ :: MisoString -> Attribute action
a11yId_ = textProp "a11y-id"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#ios-platform-accessibility-id
--
-- Used to specify the accessibility identifier of a `UIView` in *iOS*. It is
-- only used when the platform-level accessibility framework is accessed.
--
-- > iosPlatformAccessibilityId_ "view-3"
--
iosPlatformAccessibilityId_ :: MisoString -> Attribute action
iosPlatformAccessibilityId_ = textProp "ios-platform-accessibility-id"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#user-interaction-enabled
--
-- Specifies whether the target node and its child nodes can respond to Lynx touch events.
-- This property does not affect platform-level gestures (such as scrolling of scroll-view).
--
-- > userInteractionEnabled_ False
--
-- Default Value: 'True'
--
userInteractionEnabled_ :: Bool -> Attribute action
userInteractionEnabled_ = boolProp "user-interaction-enabled"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#native-interaction-enabled
--
--
-- Specify whether the target node consumes platform-layer touch events, affects
-- platform-layer gestures (such as scrolling of scroll-view), does not affect
-- Lynx touch events, and can achieve similar platform-layer gesture
-- penetration/interception effects.
--
-- > nativeInteractionEnabled_ True
--
-- Default Value: 'True' for *iOS*, 'False' for *Android*
--
nativeInteractionEnabled_ :: Bool -> Attribute action
nativeInteractionEnabled_ = boolProp "native-interaction-enabled"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#block-native-event
--
-- Specify whether to block platform layer gestures outside Lynx when the
-- target node is on the event response chain, which can achieve an effect
-- similar to blocking the platform layer side sliding back.
--
-- > blockNativeEvent True
--
-- Default Value: 'False'
--
blockNativeEvent_ :: Bool -> Attribute action
blockNativeEvent_ = boolProp "block-native-event"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#block-native-event-areas
--
-- Specify whether to block platform layer gestures outside Lynx when the target node is on the [eventAreas response chain](../../../guide/interaction/eventAreas-handling/eventAreas-propagation.mdx#eventAreas-response-chain), which can achieve an effect similar to blocking the platform layer side sliding back.
--
-- > blockNativeEventAreas_ []
--
-- Default Value: []
--
blockNativeEventAreas_ :: [Int] -> Attribute action
blockNativeEventAreas_ = prop "block-native-event-areas"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#consume-slide-event
--
-- Specify the target node to slide a specific angle on the
-- [event response chain](https://lynxjs.org/api/elements/built-in/view.html#consume-slide-event),
-- whether the platform layer gesture responds, does not affect the touch event
-- of Lynx, and can realize a front-end scrolling container similar to consuming
-- the specified direction of sliding.
--
-- > consumeSlideEvent_ []
--
-- Default Value: []
--
consumeSlideEvent_ :: [Double] -> Attribute action
consumeSlideEvent_ = prop "consume-slide-event"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#event-through
--
-- Specifies whether the touch event of the platform layer is distributed to Lynx
-- when the touch is on the target node, which can achieve a similar effect of
-- only displaying without interaction. This property supports inheritance.
--
-- > eventThrough_ True
--
-- Default Value: 'False'
--
eventThrough_ :: Bool -> Attribute action
eventThrough_ = boolProp "event-through"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#enable-touch-pseudo-propagation
--
-- Specify whether the target node supports the :active pseudo-class to continue bubbling
-- up on the [event response chain](https://lynxjs.org/guide/interaction/event-handling/event-propagation.html#event-response-chain,platform=ios).
--
-- > enableTouchPseudoPropagation_ True
--
-- Default Value: 'False'
--
enableTouchPseudoPropagation_ :: Bool -> Attribute action
enableTouchPseudoPropagation_ = boolProp "enable-touch-pseudo-propagation"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#hit-slop
--
-- Specify the touch event response hotspot of the target node, without
-- affecting the platform layer gesture.
--
-- > hitSlop_ "0px"
--
-- Default Value: "0px"
--
hitSlop_ :: MisoString -> Attribute action
hitSlop_ = textProp "hit-slop"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#ignore-focus
--
-- Specify whether to not grab focus when touching the target node. By default,
-- the node grabs focus when clicking on it, which can achieve a similar effect
-- of not closing the keyboard when clicking other areas.
--
-- In addition, it also supports inheritance, that is, the default value of the
-- child node is the ignore-focus value of the parent node, and the child node
-- can override this value.
--
-- > ignoreFocus_ True
--
-- Default Value: 'False
--
ignoreFocus_ :: MisoString -> Attribute action
ignoreFocus_ = textProp "ignore-focus"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#ios-enable-simultaneous-touch
--
-- *iOS* only
--
-- > iosEnableSimultaneousTouch_ True
--
-- Default Value: 'False'
--
iosEnableSimultaneousTouch_ :: Bool -> Attribute action
iosEnableSimultaneousTouch_ = boolProp "ios-enable-simultaneous-touch"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#__lynx_timing_flag
--
-- Add this flag to the current element to monitor the performance of the
-- lynx pipeline it participates in. When flagged, the lynx engine generates
-- a PipelineEntry event once the element completes its final painting phase.
-- This event can be observed and analyzed by registering a PerformanceObserver().
-- For more detailed usage, see the Marking Lynx Pipeline.
--
-- > lynxTimingFlag_ "test"
--
lynxTimingFlag_ :: MisoString -> Attribute action
lynxTimingFlag_ = textProp "__lynx_timing_flag"
-----------------------------------------------------------------------------
