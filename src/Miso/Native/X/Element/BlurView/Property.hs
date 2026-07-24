-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.BlurView.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.BlurView.Property
  ( -- *** Property
    androidCaptureTarget_
  , blurEffect_
  , blurRadius_
  , blurSampling_
  , enableAutoBlur_
  , experimentalUpdateBlurRadius_
  , glassInteractive_
  , glassStyle_
  , glassTintColor_
  , spacing_
    -- *** Types
  , BlurEffect (..)
  , GlassStyle (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..))
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | The blur/material effect of a \<blur-view\> (iOS), used by 'blurEffect_'.
data BlurEffect
  = BlurLight
  | BlurExtraLight
  | BlurDark
  | BlurGlass
  | BlurGlassContainer
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON BlurEffect where
  toJSON BlurLight          = "light"
  toJSON BlurExtraLight     = "extra-light"
  toJSON BlurDark           = "dark"
  toJSON BlurGlass          = "glass"
  toJSON BlurGlassContainer = "glass-container"
-----------------------------------------------------------------------------
-- | The glass effect appearance of a \<blur-view\> (iOS), used by 'glassStyle_'.
data GlassStyle
  = GlassClear
  | GlassRegular
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON GlassStyle where
  toJSON GlassClear   = "clear"
  toJSON GlassRegular = "regular"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#android-capture-target
--
-- *Android 3.9+*. The raw id of the Android Lynx view to capture and blur.
--
androidCaptureTarget_ :: MisoString -> Attribute action
androidCaptureTarget_ = textProp "android-capture-target"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#blur-effect
--
-- *iOS* only. Controls brightness of the blurred area; glass variants apply
-- material effects.
--
-- > blurEffect_ BlurDark
--
-- Default Value: 'BlurLight'
--
blurEffect_ :: BlurEffect -> Attribute action
blurEffect_ = prop "blur-effect"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#blur-radius
--
-- Gaussian blur radius specification.
--
-- Default Value: @\"0px\"@
--
blurRadius_ :: MisoString -> Attribute action
blurRadius_ = textProp "blur-radius"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#blur-sampling
--
-- *Android* only. Downsampling ratio for performance optimization.
--
-- Default Value: 6
--
blurSampling_ :: Int -> Attribute action
blurSampling_ = intProp "blur-sampling"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#enable-auto-blur
--
-- *Android* only. Automatic blur update toggle.
--
-- Default Value: 'True'
--
enableAutoBlur_ :: Bool -> Attribute action
enableAutoBlur_ = boolProp "enable-auto-blur"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#experimental-update-blur-radius
--
-- *Android 3.4+*. Switches the internal blur-buffer refresh mechanism.
--
experimentalUpdateBlurRadius_ :: Bool -> Attribute action
experimentalUpdateBlurRadius_ = boolProp "experimental-update-blur-radius"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#glass-interactive
--
-- *iOS 3.8+*. Enables interactive glass effect behavior.
--
-- Default Value: 'False'
--
glassInteractive_ :: Bool -> Attribute action
glassInteractive_ = boolProp "glass-interactive"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#glass-style
--
-- *iOS 3.8+*. Glass effect visual appearance.
--
-- > glassStyle_ GlassClear
--
-- Default Value: 'GlassRegular'
--
glassStyle_ :: GlassStyle -> Attribute action
glassStyle_ = prop "glass-style"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#glass-tint-color
--
-- *iOS 3.8+*. Tint color applied to the glass effect.
--
-- Default Value: @\"transparent\"@
--
glassTintColor_ :: MisoString -> Attribute action
glassTintColor_ = textProp "glass-tint-color"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/blur-view.html#spacing
--
-- *iOS* only. Distance threshold for element merging.
--
-- Default Value: 0
--
spacing_ :: Int -> Attribute action
spacing_ = intProp "spacing"
-----------------------------------------------------------------------------
