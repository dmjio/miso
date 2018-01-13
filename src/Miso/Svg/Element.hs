{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Element
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Svg.Element
  ( -- * HTML Embedding
    svg_
  , foreignObject_
    -- * Graphics Elements
  , circle_
  , ellipse_
  , image_
  , line_
  , path_
  , polygon_
  , polyline_
  , rect_
  , use_
  -- * Animation Elements
  , animate_
  , animateColor_
  , animateMotion_
  , animateTransform_
  , mpath_
  , set_
  -- * Descriptive Elements
  , desc_
  , metadata_
  , title_
  -- * Containers
  , a_
  , defs_
  , g_
  , marker_
  , mask_
  , missingGlyph_
  , pattern_
  , switch_
  , symbol_
  -- * Text
  , altGlyph_
  , altGlyphDef_
  , altGlyphItem_
  , glyph_
  , glyphRef_
  , textPath_
  , text_
  , tref_
  , tspan_
  -- * Fonts
  , font_
  , fontFace_
  , fontFaceFormat_
  , fontFaceName_
  , fontFaceSrc_
  , fontFaceUri_
  , hkern_
  , vkern_
  -- * Gradients
  , linearGradient_
  , radialGradient_
  , stop_
  -- * Filters
  , feBlend_
  , feColorMatrix_
  , feComponentTransfer_
  , feComposite_
  , feConvolveMatrix_
  , feDiffuseLighting_
  , feDisplacementMap_
  , feFlood_
  , feFuncA_
  , feFuncB_
  , feFuncG_
  , feFuncR_
  , feGaussianBlur_
  , feImage_
  , feMerge_
  , feMergeNode_
  , feMorhpology_
  , feOffset_
  , feSpecularLighting_
  , feTile_
  , feTurbulence_
  -- * Light source elements
  , feDistantLight_
  , fePointLight_
  , feSpotLight_
  -- * Miscellaneous
  , clipPath_
  , colorProfile_
  , cursor_
  , filter_
  , script_
  , style_
  , view_
  ) where

import           Miso.Html.Internal hiding (style_)
import           Miso.String        (MisoString)
import qualified Prelude            as P

-- | Used to construct a `VNode` with namespace "svg"
--
-- > document.createElementNS('http://www.w3.org/2000/svg', 'circle');
--
nodeSvg_ :: MisoString -> [Attribute action] -> [View action] -> View action
nodeSvg_ = P.flip (node SVG) P.Nothing

-- | Creates an svg tag
svg_ :: [Attribute action] -> [View action] -> View action
svg_ = nodeSvg_ "svg"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/foreignObject>
foreignObject_ :: [Attribute action] -> [View action] -> View action
foreignObject_ = nodeSvg_ "foreignObject"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/circle>
circle_ :: [Attribute action] -> [View action] -> View action
circle_ = nodeSvg_ "circle"

-- | <https__://developer.mozilla.org/en-US/docs/Web/SVG/Element/ellipse>
ellipse_ :: [Attribute action] -> [View action] -> View action
ellipse_ = nodeSvg_ "ellipse"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/image>
image_ :: [Attribute action] -> [View action] -> View action
image_ = nodeSvg_ "image"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/image>
line_ :: [Attribute action] -> [View action] -> View action
line_ = nodeSvg_ "line"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/path>
path_ :: [Attribute action] -> [View action] -> View action
path_ = nodeSvg_ "path"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polygon>
polygon_ :: [Attribute action] -> [View action] -> View action
polygon_ = nodeSvg_ "polygon"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polyline>
polyline_ :: [Attribute action] -> [View action] -> View action
polyline_ = nodeSvg_ "polyline"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect>
rect_ :: [Attribute action] -> [View action] -> View action
rect_ = nodeSvg_ "rect"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use>
use_ :: [Attribute action] -> [View action] -> View action
use_ = nodeSvg_ "use"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animate>
animate_ :: [Attribute action] -> [View action] -> View action
animate_ = nodeSvg_ "animate"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateColor>
animateColor_ :: [Attribute action] -> [View action] -> View action
animateColor_ = nodeSvg_ "animateColor"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateMotion>
animateMotion_ :: [Attribute action] -> [View action] -> View action
animateMotion_ = nodeSvg_ "animateMotion"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateMotion>
animateTransform_ :: [Attribute action] -> [View action] -> View action
animateTransform_ = nodeSvg_ "animateTransform"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mpath>
mpath_ :: [Attribute action] -> [View action] -> View action
mpath_ = nodeSvg_ "mpath"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/set>
set_ :: [Attribute action] -> [View action] -> View action
set_ = nodeSvg_ "set"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/desc>
desc_ :: [Attribute action] -> [View action] -> View action
desc_ = nodeSvg_ "desc"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/metadata>
metadata_ :: [Attribute action] -> [View action] -> View action
metadata_ = nodeSvg_ "metadata"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/title>
title_ :: [Attribute action] -> [View action] -> View action
title_ = nodeSvg_ "title"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/a>
a_ :: [Attribute action] -> [View action] -> View action
a_ = nodeSvg_ "a"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/defs>
defs_ :: [Attribute action] -> [View action] -> View action
defs_ = nodeSvg_ "defs"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/g>
g_ :: [Attribute action] -> [View action] -> View action
g_ = nodeSvg_ "g"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/marker>
marker_ :: [Attribute action] -> [View action] -> View action
marker_ = nodeSvg_ "marker"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mask>
mask_ :: [Attribute action] -> [View action] -> View action
mask_ = nodeSvg_ "mask"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/missingGlyph>
missingGlyph_ :: [Attribute action] -> [View action] -> View action
missingGlyph_ = nodeSvg_ "missingGlyph"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/pattern>
pattern_ :: [Attribute action] -> [View action] -> View action
pattern_ = nodeSvg_ "pattern"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/switch>
switch_ :: [Attribute action] -> [View action] -> View action
switch_ = nodeSvg_ "switch"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/symbol>
symbol_ :: [Attribute action] -> [View action] -> View action
symbol_ = nodeSvg_ "symbol"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/altGlyph>
altGlyph_ :: [Attribute action] -> [View action] -> View action
altGlyph_ = nodeSvg_ "altGlyph"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/altGlyphDef>
altGlyphDef_ :: [Attribute action] -> [View action] -> View action
altGlyphDef_ = nodeSvg_ "altGlyphDef"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/altGlyphItem>
altGlyphItem_ :: [Attribute action] -> [View action] -> View action
altGlyphItem_ = nodeSvg_ "altGlyphItem"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/glyph>
glyph_ :: [Attribute action] -> [View action] -> View action
glyph_ = nodeSvg_ "glyph"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/glyphRef>
glyphRef_ :: [Attribute action] -> [View action] -> View action
glyphRef_ = nodeSvg_ "glyphRef"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/glyphRef>
textPath_ :: [Attribute action] -> [View action] -> View action
textPath_ = nodeSvg_ "textPath"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/text>
text_ :: [Attribute action] -> [View action] -> View action
text_ = nodeSvg_ "text"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/tref>
tref_ :: [Attribute action] -> [View action] -> View action
tref_ = nodeSvg_ "tref"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/tspan>
tspan_ :: [Attribute action] -> [View action] -> View action
tspan_ = nodeSvg_ "tspan"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font>
font_ :: [Attribute action] -> [View action] -> View action
font_ = nodeSvg_ "font"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font-face>
fontFace_ :: [Attribute action] -> [View action] -> View action
fontFace_ = nodeSvg_ "font-face"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font-face-format>
fontFaceFormat_ :: [Attribute action] -> [View action] -> View action
fontFaceFormat_ = nodeSvg_ "font-face-format"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font-face-name>
fontFaceName_ :: [Attribute action] -> [View action] -> View action
fontFaceName_ = nodeSvg_ "font-face-name"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font-face-src>
fontFaceSrc_ :: [Attribute action] -> [View action] -> View action
fontFaceSrc_ = nodeSvg_ "font-face-src"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font-face-uri>
fontFaceUri_ :: [Attribute action] -> [View action] -> View action
fontFaceUri_ = nodeSvg_ "font-face-uri"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/hkern>
hkern_ :: [Attribute action] -> [View action] -> View action
hkern_ = nodeSvg_ "hkern"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/vkern>
vkern_ :: [Attribute action] -> [View action] -> View action
vkern_ = nodeSvg_ "vkern"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/linearGradient>
linearGradient_ :: [Attribute action] -> [View action] -> View action
linearGradient_ = nodeSvg_ "linearGradient"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/radialGradient>
radialGradient_ :: [Attribute action] -> [View action] -> View action
radialGradient_ = nodeSvg_ "radialGradient"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/stop>
stop_ :: [Attribute action] -> [View action] -> View action
stop_ = nodeSvg_ "stop"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feBlend>
feBlend_ :: [Attribute action] -> [View action] -> View action
feBlend_ = nodeSvg_ "feBlend"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feColorMatrix>
feColorMatrix_ :: [Attribute action] -> [View action] -> View action
feColorMatrix_ = nodeSvg_ "feColorMatrix"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feComponentTransfer>
feComponentTransfer_ :: [Attribute action] -> [View action] -> View action
feComponentTransfer_ = nodeSvg_ "feComponentTransfer"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feComposite>
feComposite_ :: [Attribute action] -> [View action] -> View action
feComposite_ = nodeSvg_ "feComposite"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feConvolveMatrix>
feConvolveMatrix_ :: [Attribute action] -> [View action] -> View action
feConvolveMatrix_ = nodeSvg_ "feConvolveMatrix"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDiffuseLighting>
feDiffuseLighting_ :: [Attribute action] -> [View action] -> View action
feDiffuseLighting_ = nodeSvg_ "feDiffuseLighting"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDisplacementMap>
feDisplacementMap_ :: [Attribute action] -> [View action] -> View action
feDisplacementMap_ = nodeSvg_ "feDisplacementMap"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFlood>
feFlood_ :: [Attribute action] -> [View action] -> View action
feFlood_ = nodeSvg_ "feFlood"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncA>
feFuncA_ :: [Attribute action] -> [View action] -> View action
feFuncA_ = nodeSvg_ "feFuncA"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncB>
feFuncB_ :: [Attribute action] -> [View action] -> View action
feFuncB_ = nodeSvg_ "feFuncB"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncG>
feFuncG_ :: [Attribute action] -> [View action] -> View action
feFuncG_ = nodeSvg_ "feFuncG"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncR>
feFuncR_ :: [Attribute action] -> [View action] -> View action
feFuncR_ = nodeSvg_ "feFuncR"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feGaussianBlur>
feGaussianBlur_ :: [Attribute action] -> [View action] -> View action
feGaussianBlur_ = nodeSvg_ "feGaussianBlur"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feImage>
feImage_ :: [Attribute action] -> [View action] -> View action
feImage_ = nodeSvg_ "feImage"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feMerge>
feMerge_ :: [Attribute action] -> [View action] -> View action
feMerge_ = nodeSvg_ "feMerge"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feMergeNode>
feMergeNode_ :: [Attribute action] -> [View action] -> View action
feMergeNode_ = nodeSvg_ "feMergeNode"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feMorhpology>
feMorhpology_ :: [Attribute action] -> [View action] -> View action
feMorhpology_ = nodeSvg_ "feMorhpology"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feOffset>
feOffset_ :: [Attribute action] -> [View action] -> View action
feOffset_ = nodeSvg_ "feOffset"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feSpecularLighting>
feSpecularLighting_ :: [Attribute action] -> [View action] -> View action
feSpecularLighting_ = nodeSvg_ "feSpecularLighting"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feTile>
feTile_ :: [Attribute action] -> [View action] -> View action
feTile_ = nodeSvg_ "feTile"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feTurbulence>
feTurbulence_ :: [Attribute action] -> [View action] -> View action
feTurbulence_ = nodeSvg_ "feTurbulence"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDistantLight>
feDistantLight_ :: [Attribute action] -> [View action] -> View action
feDistantLight_ = nodeSvg_ "feDistantLight"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fePointLight>
fePointLight_ :: [Attribute action] -> [View action] -> View action
fePointLight_ = nodeSvg_ "fePointLight"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feSpotLight>
feSpotLight_ :: [Attribute action] -> [View action] -> View action
feSpotLight_ = nodeSvg_ "feSpotLight"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/clipPath>
clipPath_ :: [Attribute action] -> [View action] -> View action
clipPath_ = nodeSvg_ "clipPath"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/color-profile>
colorProfile_ :: [Attribute action] -> [View action] -> View action
colorProfile_ = nodeSvg_ "color-profile"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/cursor>
cursor_ :: [Attribute action] -> [View action] -> View action
cursor_ = nodeSvg_ "cursor"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/filter>
filter_ :: [Attribute action] -> [View action] -> View action
filter_ = nodeSvg_ "filter"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/script>
script_ :: [Attribute action] -> [View action] -> View action
script_ = nodeSvg_ "script"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/style>
style_ :: [Attribute action] -> [View action] -> View action
style_ = nodeSvg_ "style"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/view>
view_ :: [Attribute action] -> [View action] -> View action
view_ = nodeSvg_ "view"
