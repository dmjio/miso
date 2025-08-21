-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Element
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Svg.Element
  ( -- *** SVG
    svg_
    -- *** Graphics
  , circle_
  , ellipse_
  , image_
  , line_
  , path_
  , polygon_
  , polyline_
  , rect_
  , use_
  -- *** Animation
  , animate_
  , animateMotion_
  , animateTransform_
  , mpath_
  , set_
  -- *** Descriptive
  , desc_
  , metadata_
  , title_
  -- *** Containers
  , defs_
  , g_
  , marker_
  , mask_
  , pattern_
  , switch_
  , symbol_
  -- *** Text
  , textPath_
  , text_
  , tspan_
  -- *** Gradients
  , linearGradient_
  , radialGradient_
  , stop_
  -- *** Filters
  , feBlend_
  , feColorMatrix_
  , feComponentTransfer_
  , feComposite_
  , feConvolveMatrix_
  , feDiffuseLighting_
  , feDisplacementMap_
  , feDropShadow_
  , feFlood_
  , feFuncA_
  , feFuncB_
  , feFuncG_
  , feFuncR_
  , feGaussianBlur_
  , feImage_
  , feMerge_
  , feMergeNode_
  , feMorphology_
  , feOffset_
  , feSpecularLighting_
  , feTile_
  , feTurbulence_
  -- *** Light source
  , feDistantLight_
  , fePointLight_
  , feSpotLight_
  -- *** Misc.
  , foreignObject_
  , clipPath_
  , filter_
  , script_
  , style_
  , view_
  ) where
-----------------------------------------------------------------------------
import           Miso.Types  hiding (text_)
-----------------------------------------------------------------------------
-- | Used to construct a @VNode@ with namespace *"svg"*
--
-- > document.createElementNS('http://www.w3.org/2000/svg', 'circle');
--
nodeSvg :: MisoString -> [Attribute action] -> [View model action] -> View model action
nodeSvg nodeName = node SVG nodeName
-----------------------------------------------------------------------------
-- | [\<svg\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/svg)
svg_ :: [Attribute action] -> [View model action] -> View model action
svg_ = nodeSvg "svg"
-----------------------------------------------------------------------------
-- | [\<foreignObject\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/foreignObject)
foreignObject_ :: [Attribute action] -> [View model action] -> View model action
foreignObject_ = nodeSvg "foreignObject"
-----------------------------------------------------------------------------
-- | [\<circle\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/circle)
circle_ :: [Attribute action] -> View model action
circle_ = flip (nodeSvg "circle") []
-----------------------------------------------------------------------------
-- | [\<ellipse\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/ellipse)
ellipse_ :: [Attribute action] -> [View model action] -> View model action
ellipse_ = nodeSvg "ellipse"
-----------------------------------------------------------------------------
-- | [\<image\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/image)
image_ :: [Attribute action] -> [View model action] -> View model action
image_ = nodeSvg "image"
-----------------------------------------------------------------------------
-- | [\<line\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/line)
line_ :: [Attribute action] -> [View model action] -> View model action
line_ = nodeSvg "line"
-----------------------------------------------------------------------------
-- | [\<path\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/path)
path_ :: [Attribute action] -> View model action
path_ = flip (nodeSvg "path") []
-----------------------------------------------------------------------------
-- | [\<polygon\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/polygon)
polygon_ :: [Attribute action] -> [View model action] -> View model action
polygon_ = nodeSvg "polygon"
-----------------------------------------------------------------------------
-- | [\<polyline\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/polyline)
polyline_ :: [Attribute action] -> [View model action] -> View model action
polyline_ = nodeSvg "polyline"
-----------------------------------------------------------------------------
-- | [\<rect\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/rect)
rect_ :: [Attribute action] -> [View model action] -> View model action
rect_ = nodeSvg "rect"
-----------------------------------------------------------------------------
-- | [\<use\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/use)
use_ :: [Attribute action] -> [View model action] -> View model action
use_ = nodeSvg "use"
-----------------------------------------------------------------------------
-- | [\<animate\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animate)
animate_ :: [Attribute action] -> [View model action] -> View model action
animate_ = nodeSvg "animate"
-----------------------------------------------------------------------------
-- | [\<animateMotion\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animateMotion)
animateMotion_ :: [Attribute action] -> [View model action] -> View model action
animateMotion_ = nodeSvg "animateMotion"
-----------------------------------------------------------------------------
-- | [\<animateTransform\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animateTransform)
animateTransform_ :: [Attribute action] -> [View model action] -> View model action
animateTransform_ = nodeSvg "animateTransform"
-----------------------------------------------------------------------------
-- | [\<mpath\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/mpath)
mpath_ :: [Attribute action] -> [View model action] -> View model action
mpath_ = nodeSvg "mpath"
-----------------------------------------------------------------------------
-- | [\<set\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/set)
set_ :: [Attribute action] -> [View model action] -> View model action
set_ = nodeSvg "set"
-----------------------------------------------------------------------------
-- | [\<desc\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/desc)
desc_ :: [Attribute action] -> [View model action] -> View model action
desc_ = nodeSvg "desc"
-----------------------------------------------------------------------------
-- | [\<metadata\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/metadata)
metadata_ :: [Attribute action] -> [View model action] -> View model action
metadata_ = nodeSvg "metadata"
-----------------------------------------------------------------------------
-- | [\<title\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/title)
title_ :: [Attribute action] -> [View model action] -> View model action
title_ = nodeSvg "title"
-----------------------------------------------------------------------------
-- | [\<defs\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/defs)
defs_ :: [Attribute action] -> [View model action] -> View model action
defs_ = nodeSvg "defs"
-----------------------------------------------------------------------------
-- | [\<g\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/g)
g_ :: [Attribute action] -> [View model action] -> View model action
g_ = nodeSvg "g"
-----------------------------------------------------------------------------
-- | [\<marker\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker)
marker_ :: [Attribute action] -> [View model action] -> View model action
marker_ = nodeSvg "marker"
-----------------------------------------------------------------------------
-- | [\<mask\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/mask)
mask_ :: [Attribute action] -> [View model action] -> View model action
mask_ = nodeSvg "mask"
-----------------------------------------------------------------------------
-- | [\<pattern\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/pattern)
pattern_ :: [Attribute action] -> [View model action] -> View model action
pattern_ = nodeSvg "pattern"
-----------------------------------------------------------------------------
-- | [\<switch\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/switch)
switch_ :: [Attribute action] -> [View model action] -> View model action
switch_ = nodeSvg "switch"
-----------------------------------------------------------------------------
-- | [\<symbol\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/symbol)
symbol_ :: [Attribute action] -> [View model action] -> View model action
symbol_ = nodeSvg "symbol"
-----------------------------------------------------------------------------
-- | [\<textPath\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/textPath)
textPath_ :: [Attribute action] -> [View model action] -> View model action
textPath_ = nodeSvg "textPath"
-----------------------------------------------------------------------------
-- | [\<text\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/text)
text_ :: [Attribute action] -> [View model action] -> View model action
text_ = nodeSvg "text"
-----------------------------------------------------------------------------
-- | [\<tspan\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/tspan)
tspan_ :: [Attribute action] -> [View model action] -> View model action
tspan_ = nodeSvg "tspan"
-----------------------------------------------------------------------------
-- | [\<linearGradient\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/linearGradient)
linearGradient_ :: [Attribute action] -> [View model action] -> View model action
linearGradient_ = nodeSvg "linearGradient"
-----------------------------------------------------------------------------
-- | [\<radialGradient\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/radialGradient)
radialGradient_ :: [Attribute action] -> [View model action] -> View model action
radialGradient_ = nodeSvg "radialGradient"
-----------------------------------------------------------------------------
-- | [\<stop\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/stop)
stop_ :: [Attribute action] -> [View model action] -> View model action
stop_ = nodeSvg "stop"
-----------------------------------------------------------------------------
-- | [\<feBlend\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feBlend)
feBlend_ :: [Attribute action] -> [View model action] -> View model action
feBlend_ = nodeSvg "feBlend"
-----------------------------------------------------------------------------
-- | [\<feColorMatrix\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feColorMatrix)
feColorMatrix_ :: [Attribute action] -> [View model action] -> View model action
feColorMatrix_ = nodeSvg "feColorMatrix"
-----------------------------------------------------------------------------
-- | [\<feComponentTransfer\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feComponentTransfer)
feComponentTransfer_ :: [Attribute action] -> [View model action] -> View model action
feComponentTransfer_ = nodeSvg "feComponentTransfer"
-----------------------------------------------------------------------------
-- | [\<feComposite\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feComposite)
feComposite_ :: [Attribute action] -> [View model action] -> View model action
feComposite_ = nodeSvg "feComposite"
-----------------------------------------------------------------------------
-- | [\<feConvolveMatrix\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feConvolveMatrix)
feConvolveMatrix_ :: [Attribute action] -> [View model action] -> View model action
feConvolveMatrix_ = nodeSvg "feConvolveMatrix"
-----------------------------------------------------------------------------
-- | [\<feDiffuseLighting\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDiffuseLighting)
feDiffuseLighting_ :: [Attribute action] -> [View model action] -> View model action
feDiffuseLighting_ = nodeSvg "feDiffuseLighting"
-----------------------------------------------------------------------------
-- | [\<feDisplacementMap\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDisplacementMap)
feDisplacementMap_ :: [Attribute action] -> [View model action] -> View model action
feDisplacementMap_ = nodeSvg "feDisplacementMap"
-----------------------------------------------------------------------------
-- | [\<feDropShadow\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDropShadow)
--
-- @since 1.9.0.0
feDropShadow_ :: [Attribute action] -> [View model action] -> View model action
feDropShadow_ = nodeSvg "feDropShadow"
-----------------------------------------------------------------------------
-- | [\<feFlood\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFlood)
feFlood_ :: [Attribute action] -> [View model action] -> View model action
feFlood_ = nodeSvg "feFlood"
-----------------------------------------------------------------------------
-- | [\<feFuncA\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncA)
feFuncA_ :: [Attribute action] -> [View model action] -> View model action
feFuncA_ = nodeSvg "feFuncA"
-----------------------------------------------------------------------------
-- | [\<feFuncB\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncB)
feFuncB_ :: [Attribute action] -> [View model action] -> View model action
feFuncB_ = nodeSvg "feFuncB"
-----------------------------------------------------------------------------
-- | [\<feFuncG\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncG)
feFuncG_ :: [Attribute action] -> [View model action] -> View model action
feFuncG_ = nodeSvg "feFuncG"
-----------------------------------------------------------------------------
-- | [\<feFuncR\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncR)
feFuncR_ :: [Attribute action] -> [View model action] -> View model action
feFuncR_ = nodeSvg "feFuncR"
-----------------------------------------------------------------------------
-- | [\<feGaussianBlur\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feGaussianBlur)
feGaussianBlur_ :: [Attribute action] -> [View model action] -> View model action
feGaussianBlur_ = nodeSvg "feGaussianBlur"
-----------------------------------------------------------------------------
-- | [\<feImage\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feImage)
feImage_ :: [Attribute action] -> [View model action] -> View model action
feImage_ = nodeSvg "feImage"
-----------------------------------------------------------------------------
-- | [\<feMerge\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMerge)
feMerge_ :: [Attribute action] -> [View model action] -> View model action
feMerge_ = nodeSvg "feMerge"
-----------------------------------------------------------------------------
-- | [\<feMergeNode\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMergeNode)
feMergeNode_ :: [Attribute action] -> [View model action] -> View model action
feMergeNode_ = nodeSvg "feMergeNode"
-----------------------------------------------------------------------------
-- | [\<feMorphology\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMorphology)
--
-- @since 1.9.0.0
feMorphology_ :: [Attribute action] -> [View model action] -> View model action
feMorphology_ = nodeSvg "feMorphology"
-----------------------------------------------------------------------------
-- | [\<feOffset\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feOffset)
feOffset_ :: [Attribute action] -> [View model action] -> View model action
feOffset_ = nodeSvg "feOffset"
-----------------------------------------------------------------------------
-- | [\<feSpecularLighting\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feSpecularLighting)
feSpecularLighting_ :: [Attribute action] -> [View model action] -> View model action
feSpecularLighting_ = nodeSvg "feSpecularLighting"
-----------------------------------------------------------------------------
-- | [\<feTile\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feTile)
feTile_ :: [Attribute action] -> [View model action] -> View model action
feTile_ = nodeSvg "feTile"
-----------------------------------------------------------------------------
-- | [\<feTurbulence\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feTurbulence)
feTurbulence_ :: [Attribute action] -> [View model action] -> View model action
feTurbulence_ = nodeSvg "feTurbulence"
-----------------------------------------------------------------------------
-- | [\<feDistantLight\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDistantLight)
feDistantLight_ :: [Attribute action] -> [View model action] -> View model action
feDistantLight_ = nodeSvg "feDistantLight"
-----------------------------------------------------------------------------
-- | [\<fePointLight\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/fePointLight)
fePointLight_ :: [Attribute action] -> [View model action] -> View model action
fePointLight_ = nodeSvg "fePointLight"
-----------------------------------------------------------------------------
-- | [\<feSpotLight\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feSpotLight)
feSpotLight_ :: [Attribute action] -> [View model action] -> View model action
feSpotLight_ = nodeSvg "feSpotLight"
-----------------------------------------------------------------------------
-- | [\<clipPath\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/clipPath)
clipPath_ :: [Attribute action] -> [View model action] -> View model action
clipPath_ = nodeSvg "clipPath"
-----------------------------------------------------------------------------
-- | [\<filter\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/filter)
filter_ :: [Attribute action] -> [View model action] -> View model action
filter_ = nodeSvg "filter"
-----------------------------------------------------------------------------
-- | [\<script\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/script)
script_ :: [Attribute action] -> [View model action] -> View model action
script_ = nodeSvg "script"
-----------------------------------------------------------------------------
-- | [\<style\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/style)
style_ :: [Attribute action] -> [View model action] -> View model action
style_ = nodeSvg "style"
-----------------------------------------------------------------------------
-- | [\<view\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/view)
view_ :: [Attribute action] -> [View model action] -> View model action
view_ = nodeSvg "view"
-----------------------------------------------------------------------------
