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
  , a_
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
import           Miso.Html.Types hiding (style_)
import           Miso.String        (MisoString)
-----------------------------------------------------------------------------
-- | Used to construct a @VNode@ with namespace *"svg"*
--
-- > document.createElementNS('http://www.w3.org/2000/svg', 'circle');
--
nodeSvg :: MisoString -> [Attribute action] -> [View action] -> View action
nodeSvg nodeName = node SVG nodeName Nothing
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/svg>
svg_ :: [Attribute action] -> [View action] -> View action
svg_ = nodeSvg "svg"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/foreignObject>
foreignObject_ :: [Attribute action] -> [View action] -> View action
foreignObject_ = nodeSvg "foreignObject"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/circle>
circle_ :: [Attribute action] -> [View action] -> View action
circle_ = nodeSvg "circle"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/ellipse>
ellipse_ :: [Attribute action] -> [View action] -> View action
ellipse_ = nodeSvg "ellipse"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/image>
image_ :: [Attribute action] -> [View action] -> View action
image_ = nodeSvg "image"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/line>
line_ :: [Attribute action] -> [View action] -> View action
line_ = nodeSvg "line"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/path>
path_ :: [Attribute action] -> [View action] -> View action
path_ = nodeSvg "path"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/polygon>
polygon_ :: [Attribute action] -> [View action] -> View action
polygon_ = nodeSvg "polygon"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/polyline>
polyline_ :: [Attribute action] -> [View action] -> View action
polyline_ = nodeSvg "polyline"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/rect>
rect_ :: [Attribute action] -> [View action] -> View action
rect_ = nodeSvg "rect"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/use>
use_ :: [Attribute action] -> [View action] -> View action
use_ = nodeSvg "use"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animate>
animate_ :: [Attribute action] -> [View action] -> View action
animate_ = nodeSvg "animate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animateMotion>
animateMotion_ :: [Attribute action] -> [View action] -> View action
animateMotion_ = nodeSvg "animateMotion"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animateTransform>
animateTransform_ :: [Attribute action] -> [View action] -> View action
animateTransform_ = nodeSvg "animateTransform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/mpath>
mpath_ :: [Attribute action] -> [View action] -> View action
mpath_ = nodeSvg "mpath"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/set>
set_ :: [Attribute action] -> [View action] -> View action
set_ = nodeSvg "set"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/desc>
desc_ :: [Attribute action] -> [View action] -> View action
desc_ = nodeSvg "desc"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/metadata>
metadata_ :: [Attribute action] -> [View action] -> View action
metadata_ = nodeSvg "metadata"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/title>
title_ :: [Attribute action] -> [View action] -> View action
title_ = nodeSvg "title"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/a>
a_ :: [Attribute action] -> [View action] -> View action
a_ = nodeSvg "a"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/defs>
defs_ :: [Attribute action] -> [View action] -> View action
defs_ = nodeSvg "defs"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/g>
g_ :: [Attribute action] -> [View action] -> View action
g_ = nodeSvg "g"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker>
marker_ :: [Attribute action] -> [View action] -> View action
marker_ = nodeSvg "marker"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/mask>
mask_ :: [Attribute action] -> [View action] -> View action
mask_ = nodeSvg "mask"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/pattern>
pattern_ :: [Attribute action] -> [View action] -> View action
pattern_ = nodeSvg "pattern"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/switch>
switch_ :: [Attribute action] -> [View action] -> View action
switch_ = nodeSvg "switch"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/symbol>
symbol_ :: [Attribute action] -> [View action] -> View action
symbol_ = nodeSvg "symbol"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/textPath>
textPath_ :: [Attribute action] -> [View action] -> View action
textPath_ = nodeSvg "textPath"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/text>
text_ :: [Attribute action] -> [View action] -> View action
text_ = nodeSvg "text"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/tspan>
tspan_ :: [Attribute action] -> [View action] -> View action
tspan_ = nodeSvg "tspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/linearGradient>
linearGradient_ :: [Attribute action] -> [View action] -> View action
linearGradient_ = nodeSvg "linearGradient"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/radialGradient>
radialGradient_ :: [Attribute action] -> [View action] -> View action
radialGradient_ = nodeSvg "radialGradient"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/stop>
stop_ :: [Attribute action] -> [View action] -> View action
stop_ = nodeSvg "stop"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feBlend>
feBlend_ :: [Attribute action] -> [View action] -> View action
feBlend_ = nodeSvg "feBlend"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feColorMatrix>
feColorMatrix_ :: [Attribute action] -> [View action] -> View action
feColorMatrix_ = nodeSvg "feColorMatrix"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feComponentTransfer>
feComponentTransfer_ :: [Attribute action] -> [View action] -> View action
feComponentTransfer_ = nodeSvg "feComponentTransfer"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feComposite>
feComposite_ :: [Attribute action] -> [View action] -> View action
feComposite_ = nodeSvg "feComposite"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feConvolveMatrix>
feConvolveMatrix_ :: [Attribute action] -> [View action] -> View action
feConvolveMatrix_ = nodeSvg "feConvolveMatrix"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDiffuseLighting>
feDiffuseLighting_ :: [Attribute action] -> [View action] -> View action
feDiffuseLighting_ = nodeSvg "feDiffuseLighting"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDisplacementMap>
feDisplacementMap_ :: [Attribute action] -> [View action] -> View action
feDisplacementMap_ = nodeSvg "feDisplacementMap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDropShadow>
feDropShadow_ :: [Attribute action] -> [View action] -> View action
feDropShadow_ = nodeSvg "feDropShadow"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFlood>
feFlood_ :: [Attribute action] -> [View action] -> View action
feFlood_ = nodeSvg "feFlood"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncA>
feFuncA_ :: [Attribute action] -> [View action] -> View action
feFuncA_ = nodeSvg "feFuncA"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncB>
feFuncB_ :: [Attribute action] -> [View action] -> View action
feFuncB_ = nodeSvg "feFuncB"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncG>
feFuncG_ :: [Attribute action] -> [View action] -> View action
feFuncG_ = nodeSvg "feFuncG"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncR>
feFuncR_ :: [Attribute action] -> [View action] -> View action
feFuncR_ = nodeSvg "feFuncR"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feGaussianBlur>
feGaussianBlur_ :: [Attribute action] -> [View action] -> View action
feGaussianBlur_ = nodeSvg "feGaussianBlur"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feImage>
feImage_ :: [Attribute action] -> [View action] -> View action
feImage_ = nodeSvg "feImage"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMerge>
feMerge_ :: [Attribute action] -> [View action] -> View action
feMerge_ = nodeSvg "feMerge"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMergeNode>
feMergeNode_ :: [Attribute action] -> [View action] -> View action
feMergeNode_ = nodeSvg "feMergeNode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMorphology>
feMorphology_ :: [Attribute action] -> [View action] -> View action
feMorphology_ = nodeSvg "feMorphology"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feOffset>
feOffset_ :: [Attribute action] -> [View action] -> View action
feOffset_ = nodeSvg "feOffset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feSpecularLighting>
feSpecularLighting_ :: [Attribute action] -> [View action] -> View action
feSpecularLighting_ = nodeSvg "feSpecularLighting"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feTile>
feTile_ :: [Attribute action] -> [View action] -> View action
feTile_ = nodeSvg "feTile"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feTurbulence>
feTurbulence_ :: [Attribute action] -> [View action] -> View action
feTurbulence_ = nodeSvg "feTurbulence"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDistantLight>
feDistantLight_ :: [Attribute action] -> [View action] -> View action
feDistantLight_ = nodeSvg "feDistantLight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/fePointLight>
fePointLight_ :: [Attribute action] -> [View action] -> View action
fePointLight_ = nodeSvg "fePointLight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feSpotLight>
feSpotLight_ :: [Attribute action] -> [View action] -> View action
feSpotLight_ = nodeSvg "feSpotLight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/clipPath>
clipPath_ :: [Attribute action] -> [View action] -> View action
clipPath_ = nodeSvg "clipPath"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/filter>
filter_ :: [Attribute action] -> [View action] -> View action
filter_ = nodeSvg "filter"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/script>
script_ :: [Attribute action] -> [View action] -> View action
script_ = nodeSvg "script"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/style>
style_ :: [Attribute action] -> [View action] -> View action
style_ = nodeSvg "style"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/view>
view_ :: [Attribute action] -> [View action] -> View action
view_ = nodeSvg "view"
-----------------------------------------------------------------------------
