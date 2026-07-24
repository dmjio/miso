-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Viewpager.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Viewpager.Property
  ( -- *** Property
    androidAlwaysOverscroll_
  , androidForceCanScroll_
  , bounces_
  , enableScroll_
  , initialSelectIndex_
  , iosGestureDirection_
  , iosGestureOffset_
  , iosRecognizedGestureClass_
  , iosRecognizedViewTag_
  , keepItemView_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#android-always-overscroll
--
-- *Android* only. Controls the bounce effect at the edges.
--
-- Default Value: 'False'
--
androidAlwaysOverscroll_ :: Bool -> Attribute action
androidAlwaysOverscroll_ = boolProp "android-always-overscroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#android-force-can-scroll
--
-- *Android* only. Prevents gesture pass-through to the parent.
--
-- Default Value: 'False'
--
androidForceCanScroll_ :: Bool -> Attribute action
androidForceCanScroll_ = boolProp "android-force-can-scroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#bounces
--
-- *iOS \/ Desktop \/ Harmony* only. Enables the spring effect.
--
-- Default Value: 'True'
--
bounces_ :: Bool -> Attribute action
bounces_ = boolProp "bounces"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#enable-scroll
--
-- Activates horizontal scroll gestures.
--
-- Default Value: 'True'
--
enableScroll_ :: Bool -> Attribute action
enableScroll_ = boolProp "enable-scroll"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#initial-select-index
--
-- Selects the specified page on initialization.
--
-- Default Value: 0
--
initialSelectIndex_ :: Int -> Attribute action
initialSelectIndex_ = intProp "initial-select-index"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#ios-gesture-direction
--
-- *iOS* only. Allows the outer container to respond when at the edges.
--
-- Default Value: 'False'
--
iosGestureDirection_ :: Bool -> Attribute action
iosGestureDirection_ = boolProp "ios-gesture-direction"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#ios-gesture-offset
--
-- *iOS* only. Edge zone distance where the back gesture takes priority.
--
-- Default Value: 0
--
iosGestureOffset_ :: Int -> Attribute action
iosGestureOffset_ = intProp "ios-gesture-offset"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#ios-recognized-gesture-class
--
-- *iOS* only. @UIGestureRecognizer@ class name for simultaneous recognition.
--
iosRecognizedGestureClass_ :: MisoString -> Attribute action
iosRecognizedGestureClass_ = textProp "ios-recognized-gesture-class"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#ios-recognized-view-tag
--
-- *iOS* only. @UIView@ tag identifying the gesture recognizer source.
--
-- Default Value: 0
--
iosRecognizedViewTag_ :: Int -> Attribute action
iosRecognizedViewTag_ = intProp "ios-recognized-view-tag"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#keep-item-view
--
-- Enables lazy-load mode with early exposure.
--
-- Default Value: 'False'
--
keepItemView_ :: Bool -> Attribute action
keepItemView_ = boolProp "keep-item-view"
-----------------------------------------------------------------------------
