{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Combinator
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Svg.Combinator
  ( -- * HTML Embedding
    svg
  , foreignObject
    -- * Graphics Elements
  , circle
  , ellipse
  , image
  , line
  , path
  , polygon
  , polyline
  , rect
  , use
  -- * Animation Elements
  , animate
  , animateColor
  , animateMotion
  , animateTransform
  , mpath
  , set
  -- * Descriptive Elements
  , desc
  , metadata
  , title
  -- * Containers
  , a
  , defs
  , g
  , marker
  , mask
  , missingGlyph
  , pattern
  , switch
  , symbol
  -- * Text
  , altGlyph
  , altGlyphDef
  , altGlyphItem
  , glyph
  , glyphRef
  , textPath
  , text'
  , tref
  , tspan
  -- * Fonts
  , font
  , fontFace
  , fontFaceFormat
  , fontFaceName
  , fontFaceSrc
  , fontFaceUri
  , hkern
  , vkern
  -- * Gradients
  , linearGradient
  , radialGradient
  , stop
  -- * Filters
  , feBlend
  , feColorMatrix
  , feComponentTransfer
  , feComposite
  , feConvolveMatrix
  , feDiffuseLighting
  , feDisplacementMap
  , feFlood
  , feFuncA
  , feFuncB
  , feFuncG
  , feFuncR
  , feGaussianBlur
  , feImage
  , feMerge
  , feMergeNode
  , feMorhpology
  , feOffset
  , feSpecularLighting
  , feTile
  , feTurbulence
  -- * Light source elements
  , feDistantLight
  , fePointLight
  , feSpotLight
  -- * Miscellaneous
  , clipPath
  , colorProfile
  , cursor
  , filter
  , script
  , style
  , view
  ) where

import qualified Prelude         as P

import           Miso.Html.Types
import           Miso.Html.Internal (MisoString, node, View, NS(..))

-- | Used to construct a `VNode` with namespace "svg"
--
-- > document.createElementNS('http://www.w3.org/2000/svg', 'circle');
--
nodeSvg :: MisoString -> [Attribute action] -> [View action] -> View action
nodeSvg = P.flip (node SVG) P.Nothing

-- | Creates an svg tag
svg :: [Attribute action] -> [View action] -> View action
svg = nodeSvg "svg"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/foreignObject>
foreignObject :: [Attribute action] -> [View action] -> View action
foreignObject = nodeSvg "foreignObject"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/circle>
circle :: [Attribute action] -> [View action] -> View action
circle = nodeSvg "circle"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/ellipse>
ellipse :: [Attribute action] -> [View action] -> View action
ellipse = nodeSvg "ellipse"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/image>
image :: [Attribute action] -> [View action] -> View action
image = nodeSvg "image"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/image>
line :: [Attribute action] -> [View action] -> View action
line = nodeSvg "line"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/path>
path :: [Attribute action] -> [View action] -> View action
path = nodeSvg "path"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polygon>
polygon :: [Attribute action] -> [View action] -> View action
polygon = nodeSvg "polygon"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polyline>
polyline :: [Attribute action] -> [View action] -> View action
polyline = nodeSvg "polyline"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect>
rect :: [Attribute action] -> [View action] -> View action
rect = nodeSvg "rect"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use>
use :: [Attribute action] -> [View action] -> View action
use = nodeSvg "use"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animate>
animate :: [Attribute action] -> [View action] -> View action
animate = nodeSvg "animate"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateColor>
animateColor :: [Attribute action] -> [View action] -> View action
animateColor = nodeSvg "animateColor"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateMotion>
animateMotion :: [Attribute action] -> [View action] -> View action
animateMotion = nodeSvg "animateMotion"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animateMotion>
animateTransform :: [Attribute action] -> [View action] -> View action
animateTransform = nodeSvg "animateTransform"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mpath>
mpath :: [Attribute action] -> [View action] -> View action
mpath = nodeSvg "mpath"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/set>
set :: [Attribute action] -> [View action] -> View action
set = nodeSvg "set"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/desc>
desc :: [Attribute action] -> [View action] -> View action
desc = nodeSvg "desc"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/metadata>
metadata :: [Attribute action] -> [View action] -> View action
metadata = nodeSvg "metadata"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/title>
title :: [Attribute action] -> [View action] -> View action
title = nodeSvg "title"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/a>
a :: [Attribute action] -> [View action] -> View action
a = nodeSvg "a"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/defs>
defs :: [Attribute action] -> [View action] -> View action
defs = nodeSvg "defs"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/g>
g :: [Attribute action] -> [View action] -> View action
g = nodeSvg "g"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/marker>
marker :: [Attribute action] -> [View action] -> View action
marker = nodeSvg "marker"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/mask>
mask :: [Attribute action] -> [View action] -> View action
mask = nodeSvg "mask"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/missingGlyph>
missingGlyph :: [Attribute action] -> [View action] -> View action
missingGlyph = nodeSvg "missingGlyph"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/pattern>
pattern :: [Attribute action] -> [View action] -> View action
pattern = nodeSvg "pattern"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/switch>
switch :: [Attribute action] -> [View action] -> View action
switch = nodeSvg "switch"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/symbol>
symbol :: [Attribute action] -> [View action] -> View action
symbol = nodeSvg "symbol"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/altGlyph>
altGlyph :: [Attribute action] -> [View action] -> View action
altGlyph = nodeSvg "altGlyph"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/altGlyphDef>
altGlyphDef :: [Attribute action] -> [View action] -> View action
altGlyphDef = nodeSvg "altGlyphDef"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/altGlyphItem>
altGlyphItem :: [Attribute action] -> [View action] -> View action
altGlyphItem = nodeSvg "altGlyphItem"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/glyph>
glyph :: [Attribute action] -> [View action] -> View action
glyph = nodeSvg "glyph"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/glyphRef>
glyphRef :: [Attribute action] -> [View action] -> View action
glyphRef = nodeSvg "glyphRef"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/glyphRef>
textPath :: [Attribute action] -> [View action] -> View action
textPath = nodeSvg "textPath"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/text>
text' :: [Attribute action] -> [View action] -> View action
text' = nodeSvg "text"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/tref>
tref :: [Attribute action] -> [View action] -> View action
tref = nodeSvg "tref"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/tspan>
tspan :: [Attribute action] -> [View action] -> View action
tspan = nodeSvg "tspan"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/font>
font :: [Attribute action] -> [View action] -> View action
font = nodeSvg "font"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fontFace>
fontFace :: [Attribute action] -> [View action] -> View action
fontFace = nodeSvg "fontFace"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fontFaceFormat>
fontFaceFormat :: [Attribute action] -> [View action] -> View action
fontFaceFormat = nodeSvg "fontFaceFormat"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fontFaceName>
fontFaceName :: [Attribute action] -> [View action] -> View action
fontFaceName = nodeSvg "fontFaceName"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fontFaceSrc>
fontFaceSrc :: [Attribute action] -> [View action] -> View action
fontFaceSrc = nodeSvg "fontFaceSrc"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fontFaceUri>
fontFaceUri :: [Attribute action] -> [View action] -> View action
fontFaceUri = nodeSvg "fontFaceUri"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/hkern>
hkern :: [Attribute action] -> [View action] -> View action
hkern = nodeSvg "hkern"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/vkern>
vkern :: [Attribute action] -> [View action] -> View action
vkern = nodeSvg "vkern"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/linearGradient>
linearGradient :: [Attribute action] -> [View action] -> View action
linearGradient = nodeSvg "linearGradient"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/radialGradient>
radialGradient :: [Attribute action] -> [View action] -> View action
radialGradient = nodeSvg "radialGradient"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/stop>
stop :: [Attribute action] -> [View action] -> View action
stop = nodeSvg "stop"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feBlend>
feBlend :: [Attribute action] -> [View action] -> View action
feBlend = nodeSvg "feBlend"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feColorMatrix>
feColorMatrix :: [Attribute action] -> [View action] -> View action
feColorMatrix = nodeSvg "feColorMatrix"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feComponentTransfer>
feComponentTransfer :: [Attribute action] -> [View action] -> View action
feComponentTransfer = nodeSvg "feComponentTransfer"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feComposite>
feComposite :: [Attribute action] -> [View action] -> View action
feComposite = nodeSvg "feComposite"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feConvolveMatrix>
feConvolveMatrix :: [Attribute action] -> [View action] -> View action
feConvolveMatrix = nodeSvg "feConvolveMatrix"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDiffuseLighting>
feDiffuseLighting :: [Attribute action] -> [View action] -> View action
feDiffuseLighting = nodeSvg "feDiffuseLighting"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDisplacementMap>
feDisplacementMap :: [Attribute action] -> [View action] -> View action
feDisplacementMap = nodeSvg "feDisplacementMap"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFlood>
feFlood :: [Attribute action] -> [View action] -> View action
feFlood = nodeSvg "feFlood"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncA>
feFuncA :: [Attribute action] -> [View action] -> View action
feFuncA = nodeSvg "feFuncA"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncB>
feFuncB :: [Attribute action] -> [View action] -> View action
feFuncB = nodeSvg "feFuncB"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncG>
feFuncG :: [Attribute action] -> [View action] -> View action
feFuncG = nodeSvg "feFuncG"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feFuncR>
feFuncR :: [Attribute action] -> [View action] -> View action
feFuncR = nodeSvg "feFuncR"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feGaussianBlur>
feGaussianBlur :: [Attribute action] -> [View action] -> View action
feGaussianBlur = nodeSvg "feGaussianBlur"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feImage>
feImage :: [Attribute action] -> [View action] -> View action
feImage = nodeSvg "feImage"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feMerge>
feMerge :: [Attribute action] -> [View action] -> View action
feMerge = nodeSvg "feMerge"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feMergeNode>
feMergeNode :: [Attribute action] -> [View action] -> View action
feMergeNode = nodeSvg "feMergeNode"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feMorhpology>
feMorhpology :: [Attribute action] -> [View action] -> View action
feMorhpology = nodeSvg "feMorhpology"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feOffset>
feOffset :: [Attribute action] -> [View action] -> View action
feOffset = nodeSvg "feOffset"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feSpecularLighting>
feSpecularLighting :: [Attribute action] -> [View action] -> View action
feSpecularLighting = nodeSvg "feSpecularLighting"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feTile>
feTile :: [Attribute action] -> [View action] -> View action
feTile = nodeSvg "feTile"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feTurbulence>
feTurbulence :: [Attribute action] -> [View action] -> View action
feTurbulence = nodeSvg "feTurbulence"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDistantLight>
feDistantLight :: [Attribute action] -> [View action] -> View action
feDistantLight = nodeSvg "feDistantLight"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/fePointLight>
fePointLight :: [Attribute action] -> [View action] -> View action
fePointLight = nodeSvg "fePointLight"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feSpotLight>
feSpotLight :: [Attribute action] -> [View action] -> View action
feSpotLight = nodeSvg "feSpotLight"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/clipPath>
clipPath :: [Attribute action] -> [View action] -> View action
clipPath = nodeSvg "clipPath"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/clipPath>
colorProfile :: [Attribute action] -> [View action] -> View action
colorProfile = nodeSvg "colorProfile"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/cursor>
cursor :: [Attribute action] -> [View action] -> View action
cursor = nodeSvg "cursor"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/filter>
filter :: [Attribute action] -> [View action] -> View action
filter = nodeSvg "filter"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/script>
script :: [Attribute action] -> [View action] -> View action
script = nodeSvg "script"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/style>
style :: [Attribute action] -> [View action] -> View action
style = nodeSvg "style"

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/view>
view :: [Attribute action] -> [View action] -> View action
view = nodeSvg "view"
