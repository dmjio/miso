-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Element
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Svg.Element" provides smart constructors for every element in the
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element SVG>
-- vocabulary. All nodes are created in the @SVG@ namespace via
-- @document.createElementNS(\"http:\/\/www.w3.org\/2000\/svg\", …)@.
-- This module is re-exported in its entirety by "Miso.Svg".
--
-- __Leaf elements__ (shapes, images, stops, …) omit the children argument:
--
-- @
-- circle_ :: ['Miso.Types.Attribute' action] -> 'Miso.Types.View' model action
-- @
--
-- __Container elements__ accept both attributes and children:
--
-- @
-- g_ :: ['Miso.Types.Attribute' action] -> ['Miso.Types.View' model action] -> 'Miso.Types.View' model action
-- @
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Svg"
-- import qualified "Miso.Svg.Property" as SP
--
-- logo :: 'Miso.Types.View' model action
-- logo =
--   'svg_' [ SP.'Miso.Svg.Property.viewBox_' \"0 0 200 200\", SP.'Miso.Svg.Property.width_' \"200\" ]
--     [ 'g_' [ SP.'Miso.Svg.Property.fill_' \"none\", SP.'Miso.Svg.Property.stroke_' \"black\" ]
--         [ 'circle_' [ SP.'Miso.Svg.Property.cx_' \"100\", SP.'Miso.Svg.Property.cy_' \"100\", SP.'Miso.Svg.Property.r_' \"80\" ]
--         , 'line_'   [ SP.'Miso.Svg.Property.x1_' \"20\", SP.'Miso.Svg.Property.y1_' \"100\"
--                     , SP.'Miso.Svg.Property.x2_' \"180\", SP.'Miso.Svg.Property.y2_' \"100\" ]
--         ]
--     , 'text_' [ SP.'Miso.Svg.Property.x_' \"100\", SP.'Miso.Svg.Property.y_' \"110\"
--               , SP.'Miso.Svg.Property.textAnchor_' \"middle\" ]
--               [ 'Miso.text' \"miso\" ]
--     ]
-- @
--
-- = Element groups
--
-- * __Root__: 'svg_'
-- * __Graphics__ (leaf): 'circle_', 'ellipse_', 'image_', 'line_',
--   'path_', 'polygon_', 'polyline_', 'rect_', 'use_'
-- * __Animation__: 'animate_', 'animateMotion_', 'animateTransform_',
--   'mpath_', 'set_'
-- * __Descriptive__: 'desc_', 'metadata_', 'title_'
-- * __Containers__: 'defs_', 'g_', 'marker_', 'mask_', 'pattern_',
--   'switch_', 'symbol_'
-- * __Text__: 'text_', 'textPath_', 'tspan_'
-- * __Gradients__: 'linearGradient_', 'radialGradient_', 'stop_'
-- * __Filters__: 'feBlend_', 'feColorMatrix_', 'feComponentTransfer_',
--   'feComposite_', 'feConvolveMatrix_', 'feDiffuseLighting_',
--   'feDisplacementMap_', 'feDropShadow_', 'feFlood_',
--   'feFuncA_', 'feFuncB_', 'feFuncG_', 'feFuncR_',
--   'feGaussianBlur_', 'feImage_', 'feMerge_', 'feMergeNode_',
--   'feMorphology_', 'feOffset_', 'feSpecularLighting_',
--   'feTile_', 'feTurbulence_'
-- * __Light sources__: 'feDistantLight_', 'fePointLight_', 'feSpotLight_'
-- * __Misc__: 'foreignObject_', 'clipPath_', 'filter_', 'script_',
--   'style_', 'view_'
--
-- = See also
--
-- * "Miso.Svg.Property" — SVG attribute combinators (@cx_@, @r_@, @fill_@, …)
-- * "Miso.Svg.Event" — SVG event handlers
-- * "Miso.Html.Element" — HTML element constructors
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
nodeSvg :: MisoString -> [Attribute action] -> [View context action] -> View context action
nodeSvg nodeName = node SVG nodeName
-----------------------------------------------------------------------------
-- | [\<svg\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/svg)
svg_ :: [Attribute action] -> [View context action] -> View context action
svg_ = nodeSvg "svg"
-----------------------------------------------------------------------------
-- | [\<foreignObject\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/foreignObject)
foreignObject_ :: [Attribute action] -> [View context action] -> View context action
foreignObject_ = nodeSvg "foreignObject"
-----------------------------------------------------------------------------
-- | [\<circle\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/circle)
circle_ :: [Attribute action] -> View context action
circle_ = flip (nodeSvg "circle") []
-----------------------------------------------------------------------------
-- | [\<ellipse\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/ellipse)
ellipse_ :: [Attribute action] -> View context action
ellipse_ = flip (nodeSvg "ellipse") []
-----------------------------------------------------------------------------
-- | [\<image\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/image)
image_ :: [Attribute action] -> View context action
image_ = flip (nodeSvg "image") []
-----------------------------------------------------------------------------
-- | [\<line\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/line)
line_ :: [Attribute action] -> View context action
line_ = flip (nodeSvg "line") []
-----------------------------------------------------------------------------
-- | [\<path\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/path)
path_ :: [Attribute action] -> View context action
path_ = flip (nodeSvg "path") []
-----------------------------------------------------------------------------
-- | [\<polygon\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/polygon)
polygon_ :: [Attribute action] -> View context action
polygon_ = flip (nodeSvg "polygon") []
-----------------------------------------------------------------------------
-- | [\<polyline\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/polyline)
polyline_ :: [Attribute action] -> View context action
polyline_ = flip (nodeSvg "polyline") []
-----------------------------------------------------------------------------
-- | [\<rect\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/rect)
rect_ :: [Attribute action] -> View context action
rect_ = flip (nodeSvg "rect") []
-----------------------------------------------------------------------------
-- | [\<use\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/use)
use_ :: [Attribute action] -> View context action
use_ = flip (nodeSvg "use") []
-----------------------------------------------------------------------------
-- | [\<animate\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animate)
animate_ :: [Attribute action] -> View context action
animate_ = flip (nodeSvg "animate") []
-----------------------------------------------------------------------------
-- | [\<animateMotion\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animateMotion)
animateMotion_ :: [Attribute action] -> View context action
animateMotion_ = flip (nodeSvg "animateMotion") []
-----------------------------------------------------------------------------
-- | [\<animateTransform\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/animateTransform)
animateTransform_ :: [Attribute action] -> View context action
animateTransform_ = flip (nodeSvg "animateTransform") []
-----------------------------------------------------------------------------
-- | [\<mpath\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/mpath)
mpath_ :: [Attribute action] -> View context action
mpath_ = flip (nodeSvg "mpath") []
-----------------------------------------------------------------------------
-- | [\<set\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/set)
set_ :: [Attribute action] -> View context action
set_ = flip (nodeSvg "set") []
-----------------------------------------------------------------------------
-- | [\<desc\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/desc)
desc_ :: [Attribute action] -> [View context action] -> View context action
desc_ = nodeSvg "desc"
-----------------------------------------------------------------------------
-- | [\<metadata\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/metadata)
metadata_ :: [Attribute action] -> [View context action] -> View context action
metadata_ = nodeSvg "metadata"
-----------------------------------------------------------------------------
-- | [\<title\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/title)
title_ :: [Attribute action] -> [View context action] -> View context action
title_ = nodeSvg "title"
-----------------------------------------------------------------------------
-- | [\<defs\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/defs)
defs_ :: [Attribute action] -> [View context action] -> View context action
defs_ = nodeSvg "defs"
-----------------------------------------------------------------------------
-- | [\<g\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/g)
g_ :: [Attribute action] -> [View context action] -> View context action
g_ = nodeSvg "g"
-----------------------------------------------------------------------------
-- | [\<marker\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker)
marker_ :: [Attribute action] -> [View context action] -> View context action
marker_ = nodeSvg "marker"
-----------------------------------------------------------------------------
-- | [\<mask\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/mask)
mask_ :: [Attribute action] -> [View context action] -> View context action
mask_ = nodeSvg "mask"
-----------------------------------------------------------------------------
-- | [\<pattern\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/pattern)
pattern_ :: [Attribute action] -> [View context action] -> View context action
pattern_ = nodeSvg "pattern"
-----------------------------------------------------------------------------
-- | [\<switch\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/switch)
switch_ :: [Attribute action] -> [View context action] -> View context action
switch_ = nodeSvg "switch"
-----------------------------------------------------------------------------
-- | [\<symbol\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/symbol)
symbol_ :: [Attribute action] -> [View context action] -> View context action
symbol_ = nodeSvg "symbol"
-----------------------------------------------------------------------------
-- | [\<textPath\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/textPath)
textPath_ :: [Attribute action] -> [View context action] -> View context action
textPath_ = nodeSvg "textPath"
-----------------------------------------------------------------------------
-- | [\<text\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/text)
text_ :: [Attribute action] -> [View context action] -> View context action
text_ = nodeSvg "text"
-----------------------------------------------------------------------------
-- | [\<tspan\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/tspan)
tspan_ :: [Attribute action] -> [View context action] -> View context action
tspan_ = nodeSvg "tspan"
-----------------------------------------------------------------------------
-- | [\<linearGradient\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/linearGradient)
linearGradient_ :: [Attribute action] -> [View context action] -> View context action
linearGradient_ = nodeSvg "linearGradient"
-----------------------------------------------------------------------------
-- | [\<radialGradient\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/radialGradient)
radialGradient_ :: [Attribute action] -> [View context action] -> View context action
radialGradient_ = nodeSvg "radialGradient"
-----------------------------------------------------------------------------
-- | [\<stop\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/stop)
stop_ :: [Attribute action] -> View context action
stop_ = flip (nodeSvg "stop") []
-----------------------------------------------------------------------------
-- | [\<feBlend\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feBlend)
feBlend_ :: [Attribute action] -> View context action
feBlend_ = flip (nodeSvg "feBlend") []
-----------------------------------------------------------------------------
-- | [\<feColorMatrix\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feColorMatrix)
feColorMatrix_ :: [Attribute action] -> View context action
feColorMatrix_ = flip (nodeSvg "feColorMatrix") []
-----------------------------------------------------------------------------
-- | [\<feComponentTransfer\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feComponentTransfer)
feComponentTransfer_ :: [Attribute action] -> [View context action] -> View context action
feComponentTransfer_ = nodeSvg "feComponentTransfer"
-----------------------------------------------------------------------------
-- | [\<feComposite\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feComposite)
feComposite_ :: [Attribute action] -> View context action
feComposite_ = flip (nodeSvg "feComposite") []
-----------------------------------------------------------------------------
-- | [\<feConvolveMatrix\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feConvolveMatrix)
feConvolveMatrix_ :: [Attribute action] -> View context action
feConvolveMatrix_ = flip (nodeSvg "feConvolveMatrix") []
-----------------------------------------------------------------------------
-- | [\<feDiffuseLighting\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDiffuseLighting)
feDiffuseLighting_ :: [Attribute action] -> [View context action] -> View context action
feDiffuseLighting_ = nodeSvg "feDiffuseLighting"
-----------------------------------------------------------------------------
-- | [\<feDisplacementMap\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDisplacementMap)
feDisplacementMap_ :: [Attribute action] -> View context action
feDisplacementMap_ = flip (nodeSvg "feDisplacementMap") []
-----------------------------------------------------------------------------
-- | [\<feDropShadow\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDropShadow)
--
-- @since 1.9.0.0
feDropShadow_ :: [Attribute action] -> View context action
feDropShadow_ = flip (nodeSvg "feDropShadow") []
-----------------------------------------------------------------------------
-- | [\<feFlood\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFlood)
feFlood_ :: [Attribute action] -> View context action
feFlood_ = flip (nodeSvg "feFlood") []
-----------------------------------------------------------------------------
-- | [\<feFuncA\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncA)
feFuncA_ :: [Attribute action] -> [View context action] -> View context action
feFuncA_ = nodeSvg "feFuncA"
-----------------------------------------------------------------------------
-- | [\<feFuncB\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncB)
feFuncB_ :: [Attribute action] -> [View context action] -> View context action
feFuncB_ = nodeSvg "feFuncB"
-----------------------------------------------------------------------------
-- | [\<feFuncG\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncG)
feFuncG_ :: [Attribute action] -> [View context action] -> View context action
feFuncG_ = nodeSvg "feFuncG"
-----------------------------------------------------------------------------
-- | [\<feFuncR\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feFuncR)
feFuncR_ :: [Attribute action] -> [View context action] -> View context action
feFuncR_ = nodeSvg "feFuncR"
-----------------------------------------------------------------------------
-- | [\<feGaussianBlur\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feGaussianBlur)
feGaussianBlur_ :: [Attribute action] -> View context action
feGaussianBlur_ = flip (nodeSvg "feGaussianBlur") []
-----------------------------------------------------------------------------
-- | [\<feImage\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feImage)
feImage_ :: [Attribute action] -> View context action
feImage_ = flip (nodeSvg "feImage") []
-----------------------------------------------------------------------------
-- | [\<feMerge\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMerge)
feMerge_ :: [Attribute action] -> [View context action] -> View context action
feMerge_ = nodeSvg "feMerge"
-----------------------------------------------------------------------------
-- | [\<feMergeNode\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMergeNode)
feMergeNode_ :: [Attribute action] -> View context action
feMergeNode_ = flip (nodeSvg "feMergeNode") []
-----------------------------------------------------------------------------
-- | [\<feMorphology\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feMorphology)
--
-- @since 1.9.0.0
feMorphology_ :: [Attribute action] -> View context action
feMorphology_ = flip (nodeSvg "feMorphology") []
-----------------------------------------------------------------------------
-- | [\<feOffset\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feOffset)
feOffset_ :: [Attribute action] -> View context action
feOffset_ = flip (nodeSvg "feOffset") []
-----------------------------------------------------------------------------
-- | [\<feSpecularLighting\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feSpecularLighting)
feSpecularLighting_ :: [Attribute action] -> [View context action] -> View context action
feSpecularLighting_ = nodeSvg "feSpecularLighting"
-----------------------------------------------------------------------------
-- | [\<feTile\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feTile)
feTile_ :: [Attribute action] -> View context action
feTile_ = flip (nodeSvg "feTile") []
-----------------------------------------------------------------------------
-- | [\<feTurbulence\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feTurbulence)
feTurbulence_ :: [Attribute action] -> View context action
feTurbulence_ = flip (nodeSvg "feTurbulence") []
-----------------------------------------------------------------------------
-- | [\<feDistantLight\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feDistantLight)
feDistantLight_ :: [Attribute action] -> [View context action] -> View context action
feDistantLight_ = nodeSvg "feDistantLight"
-----------------------------------------------------------------------------
-- | [\<fePointLight\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/fePointLight)
fePointLight_ :: [Attribute action] -> View context action
fePointLight_ = flip (nodeSvg "fePointLight") []
-----------------------------------------------------------------------------
-- | [\<feSpotLight\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/feSpotLight)
feSpotLight_ :: [Attribute action] -> View context action
feSpotLight_ = flip (nodeSvg "feSpotLight") []
-----------------------------------------------------------------------------
-- | [\<clipPath\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/clipPath)
clipPath_ :: [Attribute action] -> [View context action] -> View context action
clipPath_ = nodeSvg "clipPath"
-----------------------------------------------------------------------------
-- | [\<filter\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/filter)
filter_ :: [Attribute action] -> [View context action] -> View context action
filter_ = nodeSvg "filter"
-----------------------------------------------------------------------------
-- | [\<script\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/script)
script_ :: [Attribute action] -> [View context action] -> View context action
script_ = nodeSvg "script"
-----------------------------------------------------------------------------
-- | [\<style\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/style)
style_ :: [Attribute action] -> [View context action] -> View context action
style_ = nodeSvg "style"
-----------------------------------------------------------------------------
-- | [\<view\>](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/view)
view_ :: [Attribute action] -> View context action
view_ = flip (nodeSvg "view") []
-----------------------------------------------------------------------------
