-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Image.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Image.Property
  ( -- *** Property
    mode_
  , placeholder_
  , blurRadius_
  , prefetchWidth_
  , prefetchHeight_
  , capInsets_
  , capInsetsScale_
  , loopCount_
  , imageConfig_
  , autoSize_
  , deferSrcInvalidation_
  , autoPlay_
  , tintColor_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#mode
--
-- Specifies the image cropping/scaling mode
--
-- > mode_ "aspectFit"
--
-- Default Value: "scaleToFill"
--
mode_ :: MisoString -> Attribute action
mode_ = textProp "mode"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#placeholder
--
-- Specifies the path to the placeholder image. The usage and limitations
-- are the same as for the 'src' attribute.
--
-- > placeholder_ "value"
--
placeholder_ :: MisoString -> Attribute action
placeholder_ = textProp "placeholder"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#blur-radius
--
-- Specifies the Gaussian blur radius for the image.
--
-- > image_ [ blurRadius_ "10px" ]
--
-- Default Value: "0px"
--
blurRadius_ :: MisoString -> Attribute action
blurRadius_ = textProp "blur-radius"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#prefetch-widthprefetch-height
--
-- Allows initiating a request when the image has a `width` / `height` of 0. This is
-- typically used when preloading images. It's recommended to set the sizes
-- to match the actual layout `width` / `height`.
--
-- > prefetchWidth_ "10px"
--
-- Default Value: "0px"
--
prefetchWidth_ :: MisoString -> Attribute action
prefetchWidth_ = textProp "prefetch-width"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#prefetch-widthprefetch-height
--
-- Allows initiating a request when the image has a `width` / `height` of 0. This is
-- typically used when preloading images. It's recommended to set the sizes
-- to match the actual layout `width` / `height`.
--
-- > prefetchHeight_ "10px"
--
-- Default Value: "0px"
--
prefetchHeight_ :: MisoString -> Attribute action
prefetchHeight_ = textProp "prefetch-height"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#cap-insets
--
-- Specifies the [9patch](https://developer.android.com/studio/write/draw9patch) image
-- scaling area with four values representing the top, right, bottom, and left edges.
-- Values must be specific numbers and do not support percentages or decimals.
--
-- > capInsets_ "0px 14px 0 14px"
--
-- Default Value: "0px 0px 0px 0px"
--
capInsets_ :: MisoString -> Attribute action
capInsets_ = textProp "cap-insets"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#cap-insets-scale
--
-- Works with `cap-insets` to adjust the pixel positions when stretching the image.
--
-- > capInsetsScale_ 10
--
-- Default Value: 1
--
capInsetsScale_ :: Int -> Attribute action
capInsetsScale_ = intProp "cap-insets-scale"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#loop-count
--
-- Specifies the number of times to play an animated image. The default is to loop indefinitely.
--
-- > loopCount_ 10
--
-- Default Value: 0
--
loopCount_ :: Int -> Attribute action
loopCount_ = intProp "loop-count"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#image-config
--
-- *Android* only.
--
-- Specifies the image data format. There are two options: `RGB_565` | `ARGB_8888`;
--
-- > imageConfig_ "RGB_565"
--
-- Default Value: 'ARGB_8888'
--
imageConfig_ :: MisoString -> Attribute action
imageConfig_ = textProp "image-config"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#auto-size
--
-- When set to true and the \<image\> element has no width or height,
-- the size of the \<image\> will be automatically adjusted to match the image's
-- original dimensions after the image is successfully loaded, ensuring that
-- the aspect ratio is maintained.
--
-- > autoSize_ True
--
-- Default Value: 'False'
--
autoSize_ :: Bool -> Attribute action
autoSize_ = boolProp "auto-size"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#defer-src-invalidation
--
-- When set to true, the \<image\> will only clear the previously displayed
-- image resource after a new image has successfully loaded.
--
-- > deferSrcInvalidation_ True
--
-- Default Value: 'False'
--
deferSrcInvalidation_ :: Bool -> Attribute action
deferSrcInvalidation_ = boolProp "defer-src-invalidation"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#autoplay
--
-- Specifies whether the animated image should start playing automatically once
-- it is loaded.
--
-- > autoPlay_ False
--
-- Default Value: 'True'
--
autoPlay_ :: Bool -> Attribute action
autoPlay_ = boolProp "autoplay"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#tint-color
--
-- Changes the color of all non-transparent pixels to the tint-color specified.
-- The value is a color.
--
-- > tintColor_ 10
--
-- Default Value: 0
--
tintColor_ :: Int -> Attribute action
tintColor_ = intProp "tint-color"
-----------------------------------------------------------------------------
