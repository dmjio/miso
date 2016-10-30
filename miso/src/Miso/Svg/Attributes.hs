{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Attributes
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute>
--
----------------------------------------------------------------------------
module Miso.Svg.Attributes
  ( -- * Regular attributes
    accentHeight
  , accelerate
  , accumulate
  , additive
  , alphabetic
  , allowReorder
  , amplitude
  , arabicForm
  , ascent
  , attributeName
  , attributeType
  , autoReverse
  , azimuth
  , baseFrequency
  , baseProfile
  , bbox
  , begin
  , bias
  , by
  , calcMode
  , capHeight
  , class'
  , clipPathUnits
  , contentScriptType
  , contentStyleType
  , cx
  , cy
  , d
  , decelerate
  , descent
  , diffuseConstant
  , divisor
  , dur
  , dx
  , dy
  , edgeMode
  , elevation
  , end
  , exponent
  , externalResourcesRequired
  , filterRes
  , filterUnits
  , format
  , from
  , fx
  , fy
  , g1
  , g2
  , glyphName
  , glyphRef
  , gradientTransform
  , gradientUnits
  , hanging
  , height
  , horizAdvX
  , horizOriginX
  , horizOriginY
  , id
  , ideographic
  , in'
  , in2
  , intercept
  , k
  , k1
  , k2
  , k3
  , k4
  , kernelMatrix
  , kernelUnitLength
  , keyPoints
  , keySplines
  , keyTimes
  , lang
  , lengthAdjust
  , limitingConeAngle
  , local
  , markerHeight
  , markerUnits
  , markerWidth
  , maskContentUnits
  , maskUnits
  , mathematical
  , max
  , media
  , method
  , min
  , mode
  , name
  , numOctaves
  , offset
  , operator
  , order
  , orient
  , orientation
  , origin
  , overlinePosition
  , overlineThickness
  , panose1
  , path
  , pathLength
  , patternContentUnits
  , patternTransform
  , patternUnits
  , pointOrder
  , points
  , pointsAtX
  , pointsAtY
  , pointsAtZ
  , preserveAlpha
  , preserveAspectRatio
  , primitiveUnits
  , r
  , radius
  , refX
  , refY
  , renderingIntent
  , repeatCount
  , repeatDur
  , requiredExtensions
  , requiredFeatures
  , restart
  , result
  , rotate
  , rx
  , ry
  , scale
  , seed
  , slope
  , spacing
  , specularConstant
  , specularExponent
  , speed
  , spreadMethod
  , startOffset
  , stdDeviation
  , stemh
  , stemv
  , stitchTiles
  , strikethroughPosition
  , strikethroughThickness
  , string
  , style
  , surfaceScale
  , systemLanguage
  , tableValues
  , target
  , targetX
  , targetY
  , textLength
  , title
  , to
  , transform
  , type'
  , u1
  , u2
  , underlinePosition
  , underlineThickness
  , unicode
  , unicodeRange
  , unitsPerEm
  , vAlphabetic
  , vHanging
  , vIdeographic
  , vMathematical
  , values
  , version
  , vertAdvY
  , vertOriginX
  , vertOriginY
  , viewBox
  , viewTarget
  , width
  , widths
  , x
  , xHeight
  , x1
  , x2
  , xChannelSelector
  , xlinkActuate
  , xlinkArcrole
  , xlinkHref
  , xlinkRole
  , xlinkShow
  , xlinkTitle
  , xlinkType
  , xmlBase
  , xmlLang
  , xmlSpace
  , y
  , y1
  , y2
  , yChannelSelector
  , z
  , zoomAndPan
  -- * Presentation attributes
  , alignmentBaseline
  , baselineShift
  , clipPath
  , clipRule
  , clip
  , colorInterpolationFilters
  , colorInterpolation
  , colorProfile
  , colorRendering
  , color
  , cursor
  , direction
  , display
  , dominantBaseline
  , enableBackground
  , fillOpacity
  , fillRule
  , fill
  , filter
  , floodColor
  , floodOpacity
  , fontFamily
  , fontSizeAdjust
  , fontSize
  , fontStretch
  , fontStyle
  , fontVariant
  , fontWeight
  , glyphOrientationHorizontal
  , glyphOrientationVertical
  , imageRendering
  , kerning
  , letterSpacing
  , lightingColor
  , markerEnd
  , markerMid
  , markerStart
  , mask
  , opacity
  , overflow
  , pointerEvents
  , shapeRendering
  , stopColor
  , stopOpacity
  , strokeDasharray
  , strokeDashoffset
  , strokeLinecap
  , strokeLinejoin
  , strokeMiterlimit
  , strokeOpacity
  , strokeWidth
  , stroke
  , textAnchor
  , textDecoration
  , textRendering
  , unicodeBidi
  , visibility
  , wordSpacing
  , writingMode
  ) where

import Miso.Html.Attr
import Miso.Html.Types
import Miso.Html.String

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accent-height>
accentHeight ::  MisoString -> Attribute action
accentHeight = attr "accentHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accelerate>
accelerate ::  MisoString -> Attribute action
accelerate = attr "accelerate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accumulate>
accumulate ::  MisoString -> Attribute action
accumulate = attr "accumulate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/additive>
additive ::  MisoString -> Attribute action
additive = attr "additive"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alphabetic>
alphabetic ::  MisoString -> Attribute action
alphabetic = attr "alphabetic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/allowReorder>
allowReorder ::  MisoString -> Attribute action
allowReorder = attr "allowReorder"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/amplitude>
amplitude ::  MisoString -> Attribute action
amplitude = attr "amplitude"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/arabicForm>
arabicForm ::  MisoString -> Attribute action
arabicForm = attr "arabicForm"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ascent>
ascent ::  MisoString -> Attribute action
ascent = attr "ascent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeName>
attributeName ::  MisoString -> Attribute action
attributeName = attr "attributeName"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeType>
attributeType ::  MisoString -> Attribute action
attributeType = attr "attributeType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/autoReverse>
autoReverse ::  MisoString -> Attribute action
autoReverse = attr "autoReverse"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/azimuth>
azimuth ::  MisoString -> Attribute action
azimuth = attr "azimuth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseFrequency>
baseFrequency ::  MisoString -> Attribute action
baseFrequency = attr "baseFrequency"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseProfile>
baseProfile ::  MisoString -> Attribute action
baseProfile = attr "baseProfile"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bbox>
bbox ::  MisoString -> Attribute action
bbox = attr "bbox"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin>
begin ::  MisoString -> Attribute action
begin = attr "begin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bias>
bias ::  MisoString -> Attribute action
bias = attr "bias"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/by>
by ::  MisoString -> Attribute action
by = attr "by"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/calcMode>
calcMode ::  MisoString -> Attribute action
calcMode = attr "calcMode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/capHeight>
capHeight ::  MisoString -> Attribute action
capHeight = attr "capHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/class>
class' ::  MisoString -> Attribute action
class' = attr "class"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipPathUnits>
clipPathUnits ::  MisoString -> Attribute action
clipPathUnits = attr "clipPathUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/contentScriptType>
contentScriptType ::  MisoString -> Attribute action
contentScriptType = attr "contentScriptType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/contentStyleType>
contentStyleType ::  MisoString -> Attribute action
contentStyleType = attr "contentStyleType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cx>
cx ::  MisoString -> Attribute action
cx = attr "cx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cy>
cy ::  MisoString -> Attribute action
cy = attr "cy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d>
d ::  MisoString -> Attribute action
d = attr "d"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/decelerate>
decelerate ::  MisoString -> Attribute action
decelerate = attr "decelerate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/descent>
descent ::  MisoString -> Attribute action
descent = attr "descent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/diffuseConstant>
diffuseConstant ::  MisoString -> Attribute action
diffuseConstant = attr "diffuseConstant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/divisor>
divisor ::  MisoString -> Attribute action
divisor = attr "divisor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur>
dur ::  MisoString -> Attribute action
dur = attr "dur"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dx>
dx ::  MisoString -> Attribute action
dx = attr "dx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dy>
dy ::  MisoString -> Attribute action
dy = attr "dy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/edgeMode>
edgeMode ::  MisoString -> Attribute action
edgeMode = attr "edgeMode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/elevation>
elevation ::  MisoString -> Attribute action
elevation = attr "elevation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/end>
end ::  MisoString -> Attribute action
end = attr "end"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/exponent>
exponent ::  MisoString -> Attribute action
exponent = attr "exponent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/externalResourcesRequired>
externalResourcesRequired ::  MisoString -> Attribute action
externalResourcesRequired = attr "externalResourcesRequired"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterRes>
filterRes ::  MisoString -> Attribute action
filterRes = attr "filterRes"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterUnits>
filterUnits ::  MisoString -> Attribute action
filterUnits = attr "filterUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/format>
format ::  MisoString -> Attribute action
format = attr "format"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from>
from ::  MisoString -> Attribute action
from = attr "from"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fx>
fx ::  MisoString -> Attribute action
fx = attr "fx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fy>
fy ::  MisoString -> Attribute action
fy = attr "fy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/g1>
g1 ::  MisoString -> Attribute action
g1 = attr "g1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/g2>
g2 ::  MisoString -> Attribute action
g2 = attr "g2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphName>
glyphName ::  MisoString -> Attribute action
glyphName = attr "glyphName"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphRef>
glyphRef ::  MisoString -> Attribute action
glyphRef = attr "glyphRef"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientTransform>
gradientTransform ::  MisoString -> Attribute action
gradientTransform = attr "gradientTransform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits>
gradientUnits ::  MisoString -> Attribute action
gradientUnits = attr "gradientUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/hanging>
hanging ::  MisoString -> Attribute action
hanging = attr "hanging"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/height>
height ::  MisoString -> Attribute action
height = attr "height"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horizAdvX>
horizAdvX ::  MisoString -> Attribute action
horizAdvX = attr "horizAdvX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horizOriginX>
horizOriginX ::  MisoString -> Attribute action
horizOriginX = attr "horizOriginX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horizOriginY>
horizOriginY ::  MisoString -> Attribute action
horizOriginY = attr "horizOriginY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/id>
id ::  MisoString -> Attribute action
id = attr "id"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ideographic>
ideographic ::  MisoString -> Attribute action
ideographic = attr "ideographic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in>
in' ::  MisoString -> Attribute action
in' = attr "in"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in2>
in2 ::  MisoString -> Attribute action
in2 = attr "in2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/intercept>
intercept ::  MisoString -> Attribute action
intercept = attr "intercept"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k>
k ::  MisoString -> Attribute action
k = attr "k"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k1>
k1 ::  MisoString -> Attribute action
k1 = attr "k1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k2>
k2 ::  MisoString -> Attribute action
k2 = attr "k2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k3>
k3 ::  MisoString -> Attribute action
k3 = attr "k3"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k4>
k4 ::  MisoString -> Attribute action
k4 = attr "k4"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelMatrix>
kernelMatrix ::  MisoString -> Attribute action
kernelMatrix = attr "kernelMatrix"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelUnitLength>
kernelUnitLength ::  MisoString -> Attribute action
kernelUnitLength = attr "kernelUnitLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyPoints>
keyPoints ::  MisoString -> Attribute action
keyPoints = attr "keyPoints"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keySplines>
keySplines ::  MisoString -> Attribute action
keySplines = attr "keySplines"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyTimes>
keyTimes ::  MisoString -> Attribute action
keyTimes = attr "keyTimes"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lang>
lang ::  MisoString -> Attribute action
lang = attr "lang"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lengthAdjust>
lengthAdjust ::  MisoString -> Attribute action
lengthAdjust = attr "lengthAdjust"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/limitingConeAngle>
limitingConeAngle ::  MisoString -> Attribute action
limitingConeAngle = attr "limitingConeAngle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/local>
local ::  MisoString -> Attribute action
local = attr "local"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerHeight>
markerHeight ::  MisoString -> Attribute action
markerHeight = attr "markerHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits>
markerUnits ::  MisoString -> Attribute action
markerUnits = attr "markerUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerWidth>
markerWidth ::  MisoString -> Attribute action
markerWidth = attr "markerWidth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskContentUnits>
maskContentUnits ::  MisoString -> Attribute action
maskContentUnits = attr "maskContentUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskUnits>
maskUnits ::  MisoString -> Attribute action
maskUnits = attr "maskUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mathematical>
mathematical ::  MisoString -> Attribute action
mathematical = attr "mathematical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/max>
max ::  MisoString -> Attribute action
max = attr "max"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/media>
media ::  MisoString -> Attribute action
media = attr "media"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/method>
method ::  MisoString -> Attribute action
method = attr "method"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/min>
min ::  MisoString -> Attribute action
min = attr "min"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mode>
mode ::  MisoString -> Attribute action
mode = attr "mode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/name>
name ::  MisoString -> Attribute action
name = attr "name"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/numOctaves>
numOctaves ::  MisoString -> Attribute action
numOctaves = attr "numOctaves"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/offset>
offset ::  MisoString -> Attribute action
offset = attr "offset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/operator>
operator ::  MisoString -> Attribute action
operator = attr "operator"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/order>
order ::  MisoString -> Attribute action
order = attr "order"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orient>
orient ::  MisoString -> Attribute action
orient = attr "orient"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orientation>
orientation ::  MisoString -> Attribute action
orientation = attr "orientation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/origin>
origin ::  MisoString -> Attribute action
origin = attr "origin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overlinePosition>
overlinePosition ::  MisoString -> Attribute action
overlinePosition = attr "overlinePosition"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overlineThickness>
overlineThickness ::  MisoString -> Attribute action
overlineThickness = attr "overlineThickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/panose1>
panose1 ::  MisoString -> Attribute action
panose1 = attr "panose1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/path>
path ::  MisoString -> Attribute action
path = attr "path"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pathLength>
pathLength ::  MisoString -> Attribute action
pathLength = attr "pathLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternContentUnits>
patternContentUnits ::  MisoString -> Attribute action
patternContentUnits = attr "patternContentUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternTransform>
patternTransform ::  MisoString -> Attribute action
patternTransform = attr "patternTransform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternUnits>
patternUnits ::  MisoString -> Attribute action
patternUnits = attr "patternUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointOrder>
pointOrder ::  MisoString -> Attribute action
pointOrder = attr "pointOrder"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/points>
points ::  MisoString -> Attribute action
points = attr "points"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtX>
pointsAtX ::  MisoString -> Attribute action
pointsAtX = attr "pointsAtX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtY>
pointsAtY ::  MisoString -> Attribute action
pointsAtY = attr "pointsAtY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtZ>
pointsAtZ ::  MisoString -> Attribute action
pointsAtZ = attr "pointsAtZ"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAlpha>
preserveAlpha ::  MisoString -> Attribute action
preserveAlpha = attr "preserveAlpha"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio>
preserveAspectRatio ::  MisoString -> Attribute action
preserveAspectRatio = attr "preserveAspectRatio"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/primitiveUnits>
primitiveUnits ::  MisoString -> Attribute action
primitiveUnits = attr "primitiveUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/r>
r ::  MisoString -> Attribute action
r = attr "r"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/radius>
radius ::  MisoString -> Attribute action
radius = attr "radius"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refX>
refX ::  MisoString -> Attribute action
refX = attr "refX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refY>
refY ::  MisoString -> Attribute action
refY = attr "refY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/renderingIntent>
renderingIntent ::  MisoString -> Attribute action
renderingIntent = attr "renderingIntent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatCount>
repeatCount ::  MisoString -> Attribute action
repeatCount = attr "repeatCount"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatDur>
repeatDur ::  MisoString -> Attribute action
repeatDur = attr "repeatDur"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/requiredExtensions>
requiredExtensions ::  MisoString -> Attribute action
requiredExtensions = attr "requiredExtensions"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/requiredFeatures>
requiredFeatures ::  MisoString -> Attribute action
requiredFeatures = attr "requiredFeatures"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/restart>
restart ::  MisoString -> Attribute action
restart = attr "restart"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/result>
result ::  MisoString -> Attribute action
result = attr "result"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rotate>
rotate ::  MisoString -> Attribute action
rotate = attr "rotate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rx>
rx ::  MisoString -> Attribute action
rx = attr "rx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ry>
ry ::  MisoString -> Attribute action
ry = attr "ry"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/scale>
scale ::  MisoString -> Attribute action
scale = attr "scale"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/seed>
seed ::  MisoString -> Attribute action
seed = attr "seed"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/slope>
slope ::  MisoString -> Attribute action
slope = attr "slope"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spacing>
spacing ::  MisoString -> Attribute action
spacing = attr "spacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularConstant>
specularConstant ::  MisoString -> Attribute action
specularConstant = attr "specularConstant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularExponent>
specularExponent ::  MisoString -> Attribute action
specularExponent = attr "specularExponent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/speed>
speed ::  MisoString -> Attribute action
speed = attr "speed"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spreadMethod>
spreadMethod ::  MisoString -> Attribute action
spreadMethod = attr "spreadMethod"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/startOffset>
startOffset ::  MisoString -> Attribute action
startOffset = attr "startOffset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stdDeviation>
stdDeviation ::  MisoString -> Attribute action
stdDeviation = attr "stdDeviation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stemh>
stemh ::  MisoString -> Attribute action
stemh = attr "stemh"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stemv>
stemv ::  MisoString -> Attribute action
stemv = attr "stemv"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stitchTiles>
stitchTiles ::  MisoString -> Attribute action
stitchTiles = attr "stitchTiles"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strikethroughPosition>
strikethroughPosition ::  MisoString -> Attribute action
strikethroughPosition = attr "strikethroughPosition"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strikethroughThickness>
strikethroughThickness ::  MisoString -> Attribute action
strikethroughThickness = attr "strikethroughThickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/string>
string ::  MisoString -> Attribute action
string = attr "string"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/style>
style ::  MisoString -> Attribute action
style = attr "style"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/surfaceScale>
surfaceScale ::  MisoString -> Attribute action
surfaceScale = attr "surfaceScale"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/systemLanguage>
systemLanguage ::  MisoString -> Attribute action
systemLanguage = attr "systemLanguage"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/tableValues>
tableValues ::  MisoString -> Attribute action
tableValues = attr "tableValues"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/target>
target ::  MisoString -> Attribute action
target = attr "target"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetX>
targetX ::  MisoString -> Attribute action
targetX = attr "targetX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetY>
targetY ::  MisoString -> Attribute action
targetY = attr "targetY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textLength>
textLength ::  MisoString -> Attribute action
textLength = attr "textLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/title>
title ::  MisoString -> Attribute action
title = attr "title"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to>
to ::  MisoString -> Attribute action
to = attr "to"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
transform ::  MisoString -> Attribute action
transform = attr "transform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/type>
type' ::  MisoString -> Attribute action
type' = attr "type"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/u1>
u1 ::  MisoString -> Attribute action
u1 = attr "u1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/u2>
u2 ::  MisoString -> Attribute action
u2 = attr "u2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/underlinePosition>
underlinePosition ::  MisoString -> Attribute action
underlinePosition = attr "underlinePosition"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/underlineThickness>
underlineThickness ::  MisoString -> Attribute action
underlineThickness = attr "underlineThickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicode>
unicode ::  MisoString -> Attribute action
unicode = attr "unicode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicodeRange>
unicodeRange ::  MisoString -> Attribute action
unicodeRange = attr "unicodeRange"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unitsPerEm>
unitsPerEm ::  MisoString -> Attribute action
unitsPerEm = attr "unitsPerEm"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vAlphabetic>
vAlphabetic ::  MisoString -> Attribute action
vAlphabetic = attr "vAlphabetic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vHanging>
vHanging ::  MisoString -> Attribute action
vHanging = attr "vHanging"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vIdeographic>
vIdeographic ::  MisoString -> Attribute action
vIdeographic = attr "vIdeographic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vMathematical>
vMathematical ::  MisoString -> Attribute action
vMathematical = attr "vMathematical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values>
values ::  MisoString -> Attribute action
values = attr "values"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/version>
version ::  MisoString -> Attribute action
version = attr "version"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vertAdvY>
vertAdvY ::  MisoString -> Attribute action
vertAdvY = attr "vertAdvY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vertOriginX>
vertOriginX ::  MisoString -> Attribute action
vertOriginX = attr "vertOriginX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vertOriginY>
vertOriginY ::  MisoString -> Attribute action
vertOriginY = attr "vertOriginY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewBox>
viewBox ::  MisoString -> Attribute action
viewBox = attr "viewBox"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewTarget>
viewTarget ::  MisoString -> Attribute action
viewTarget = attr "viewTarget"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/width>
width ::  MisoString -> Attribute action
width = attr "width"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/widths>
widths ::  MisoString -> Attribute action
widths = attr "widths"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x>
x ::  MisoString -> Attribute action
x = attr "x"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xHeight>
xHeight ::  MisoString -> Attribute action
xHeight = attr "xHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x1>
x1 ::  MisoString -> Attribute action
x1 = attr "x1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x2>
x2 ::  MisoString -> Attribute action
x2 = attr "x2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xChannelSelector>
xChannelSelector ::  MisoString -> Attribute action
xChannelSelector = attr "xChannelSelector"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkActuate>
xlinkActuate ::  MisoString -> Attribute action
xlinkActuate = attr "xlinkActuate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkArcrole>
xlinkArcrole ::  MisoString -> Attribute action
xlinkArcrole = attr "xlinkArcrole"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkHref>
xlinkHref ::  MisoString -> Attribute action
xlinkHref = attr "xlinkHref"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkRole>
xlinkRole ::  MisoString -> Attribute action
xlinkRole = attr "xlinkRole"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkShow>
xlinkShow ::  MisoString -> Attribute action
xlinkShow = attr "xlinkShow"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkTitle>
xlinkTitle ::  MisoString -> Attribute action
xlinkTitle = attr "xlinkTitle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkType>
xlinkType ::  MisoString -> Attribute action
xlinkType = attr "xlinkType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlBase>
xmlBase ::  MisoString -> Attribute action
xmlBase = attr "xmlBase"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlLang>
xmlLang ::  MisoString -> Attribute action
xmlLang = attr "xmlLang"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlSpace>
xmlSpace ::  MisoString -> Attribute action
xmlSpace = attr "xmlSpace"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y>
y ::  MisoString -> Attribute action
y = attr "y"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y1>
y1 ::  MisoString -> Attribute action
y1 = attr "y1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y2>
y2 ::  MisoString -> Attribute action
y2 = attr "y2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/yChannelSelector>
yChannelSelector ::  MisoString -> Attribute action
yChannelSelector = attr "yChannelSelector"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/z>
z ::  MisoString -> Attribute action
z = attr "z"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/zoomAndPan>
zoomAndPan ::  MisoString -> Attribute action
zoomAndPan = attr "zoomAndPan"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignmentBaseline>
alignmentBaseline ::  MisoString -> Attribute action
alignmentBaseline = attr "alignmentBaseline"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baselineShift>
baselineShift ::  MisoString -> Attribute action
baselineShift = attr "baselineShift"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipPath>
clipPath ::  MisoString -> Attribute action
clipPath = attr "clipPath"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipRule>
clipRule ::  MisoString -> Attribute action
clipRule = attr "clipRule"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip>
clip ::  MisoString -> Attribute action
clip = attr "clip"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorInterpolationFilters>
colorInterpolationFilters ::  MisoString -> Attribute action
colorInterpolationFilters = attr "colorInterpolationFilters"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorInterpolation>
colorInterpolation ::  MisoString -> Attribute action
colorInterpolation = attr "colorInterpolation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorProfile>
colorProfile ::  MisoString -> Attribute action
colorProfile = attr "colorProfile"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorRendering>
colorRendering ::  MisoString -> Attribute action
colorRendering = attr "colorRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color>
color ::  MisoString -> Attribute action
color = attr "color"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cursor>
cursor ::  MisoString -> Attribute action
cursor = attr "cursor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/direction>
direction ::  MisoString -> Attribute action
direction = attr "direction"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/display>
display ::  MisoString -> Attribute action
display = attr "display"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dominantBaseline>
dominantBaseline ::  MisoString -> Attribute action
dominantBaseline = attr "dominantBaseline"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/enableBackground>
enableBackground ::  MisoString -> Attribute action
enableBackground = attr "enableBackground"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fillOpacity>
fillOpacity ::  MisoString -> Attribute action
fillOpacity = attr "fillOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fillRule>
fillRule ::  MisoString -> Attribute action
fillRule = attr "fillRule"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
fill ::  MisoString -> Attribute action
fill = attr "fill"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filter>
filter ::  MisoString -> Attribute action
filter = attr "filter"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/floodColor>
floodColor ::  MisoString -> Attribute action
floodColor = attr "floodColor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/floodOpacity>
floodOpacity ::  MisoString -> Attribute action
floodOpacity = attr "floodOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontFamily>
fontFamily ::  MisoString -> Attribute action
fontFamily = attr "fontFamily"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontSizeAdjust>
fontSizeAdjust ::  MisoString -> Attribute action
fontSizeAdjust = attr "fontSizeAdjust"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontSize>
fontSize ::  MisoString -> Attribute action
fontSize = attr "fontSize"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontStretch>
fontStretch ::  MisoString -> Attribute action
fontStretch = attr "fontStretch"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontStyle>
fontStyle ::  MisoString -> Attribute action
fontStyle = attr "fontStyle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontVariant>
fontVariant ::  MisoString -> Attribute action
fontVariant = attr "fontVariant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontWeight>
fontWeight ::  MisoString -> Attribute action
fontWeight = attr "fontWeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphOrientationHorizontal>
glyphOrientationHorizontal ::  MisoString -> Attribute action
glyphOrientationHorizontal = attr "glyphOrientationHorizontal"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphOrientationVertical>
glyphOrientationVertical ::  MisoString -> Attribute action
glyphOrientationVertical = attr "glyphOrientationVertical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/imageRendering>
imageRendering ::  MisoString -> Attribute action
imageRendering = attr "imageRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kerning>
kerning ::  MisoString -> Attribute action
kerning = attr "kerning"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/letterSpacing>
letterSpacing ::  MisoString -> Attribute action
letterSpacing = attr "letterSpacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lightingColor>
lightingColor ::  MisoString -> Attribute action
lightingColor = attr "lightingColor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerEnd>
markerEnd ::  MisoString -> Attribute action
markerEnd = attr "markerEnd"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerMid>
markerMid ::  MisoString -> Attribute action
markerMid = attr "markerMid"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerStart>
markerStart ::  MisoString -> Attribute action
markerStart = attr "markerStart"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mask>
mask ::  MisoString -> Attribute action
mask = attr "mask"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/opacity>
opacity ::  MisoString -> Attribute action
opacity = attr "opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overflow>
overflow ::  MisoString -> Attribute action
overflow = attr "overflow"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointerEvents>
pointerEvents ::  MisoString -> Attribute action
pointerEvents = attr "pointerEvents"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/shapeRendering>
shapeRendering ::  MisoString -> Attribute action
shapeRendering = attr "shapeRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stopColor>
stopColor ::  MisoString -> Attribute action
stopColor = attr "stopColor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stopOpacity>
stopOpacity ::  MisoString -> Attribute action
stopOpacity = attr "stopOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeDasharray>
strokeDasharray ::  MisoString -> Attribute action
strokeDasharray = attr "strokeDasharray"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeDashoffset>
strokeDashoffset ::  MisoString -> Attribute action
strokeDashoffset = attr "strokeDashoffset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeLinecap>
strokeLinecap ::  MisoString -> Attribute action
strokeLinecap = attr "strokeLinecap"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeLinejoin>
strokeLinejoin ::  MisoString -> Attribute action
strokeLinejoin = attr "strokeLinejoin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeMiterlimit>
strokeMiterlimit ::  MisoString -> Attribute action
strokeMiterlimit = attr "strokeMiterlimit"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeOpacity>
strokeOpacity ::  MisoString -> Attribute action
strokeOpacity = attr "strokeOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeWidth>
strokeWidth ::  MisoString -> Attribute action
strokeWidth = attr "strokeWidth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke>
stroke ::  MisoString -> Attribute action
stroke = attr "stroke"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textAnchor>
textAnchor ::  MisoString -> Attribute action
textAnchor = attr "textAnchor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textDecoration>
textDecoration ::  MisoString -> Attribute action
textDecoration = attr "textDecoration"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textRendering>
textRendering ::  MisoString -> Attribute action
textRendering = attr "textRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicodeBidi>
unicodeBidi ::  MisoString -> Attribute action
unicodeBidi = attr "unicodeBidi"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/visibility>
visibility ::  MisoString -> Attribute action
visibility = attr "visibility"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/wordSpacing>
wordSpacing ::  MisoString -> Attribute action
wordSpacing = attr "wordSpacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/writingMode>
writingMode ::  MisoString -> Attribute action
writingMode = attr "writingMode"
