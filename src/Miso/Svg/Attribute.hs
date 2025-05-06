-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Attribute
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute>
--
----------------------------------------------------------------------------
module Miso.Svg.Attribute
  ( -- *** Attributes
    accumulate_
  , additive_
  , amplitude_
  , attributeName_
  , azimuth_
  , baseFrequency_
  , begin_
  , bias_
  , by_
  , calcMode_
  , class_'
  , clipPathUnits_
  , cx_
  , cy_
  , d_
  -- TODO decoding
  , diffuseConstant_
  , divisor_
  , dur_
  , dx_
  , dy_
  , edgeMode_
  , elevation_
  , end_
  , exponent_
  , filterUnits_
  -- TODO fr
  -- TODO glyph-orientation-horizontal
  -- TODO glyph-orientation-vertical
  -- TODO href
  -- TODO hreflang
  , from_
  , fx_
  , fy_
  , gradientTransform_
  , gradientUnits_
  , height_
  , id_
  , in_'
  , in2_
  , intercept_
  , k1_
  , k2_
  , k3_
  , k4_
  , kernelMatrix_
  , keyPoints_
  , keySplines_
  , keyTimes_
  , lang_
  , lengthAdjust_
  , limitingConeAngle_
  , markerHeight_
  , markerUnits_
  , markerWidth_
  , maskContentUnits_
  , maskUnits_
  , max_
  , media_
  , method_
  , min_
  , mode_
  , numOctaves_
  , operator_
  , order_
  , orient_
  -- TODO paint-order
  -- TODO ping
  , origin_
  , path_
  , pathLength_
  , patternContentUnits_
  , patternTransform_
  , patternUnits_
  -- TODO referrerPolicy
  -- TODO rel
  -- TODO requiredExtensions
  -- TODO requiredFeatures
  -- TODO side
  , points_
  , pointsAtX_
  , pointsAtY_
  , pointsAtZ_
  , preserveAlpha_
  , preserveAspectRatio_
  , primitiveUnits_
  , r_
  , radius_
  , refX_
  , refY_
  , repeatCount_
  , repeatDur_
  , restart_
  , result_
  , rotate_
  , rx_
  , ry_
  , scale_
  , seed_
  , slope_
  , spacing_
  , specularConstant_
  , specularExponent_
  , spreadMethod_
  , startOffset_
  , stdDeviation_
  , stitchTiles_
  , style_
  , surfaceScale_
  , systemLanguage_
  , tableValues_
  , target_
  , targetX_
  , targetY_
  , textLength_
  , to_
  , transform_
  -- TODO transform-origin
  , type_'
  , values_
  -- TODO vector-effect
  -- TODO version
  , viewBox_
  -- TODO zoomAndPan
  , width_
  , x_
  , x1_
  , x2_
  , xChannelSelector_
  , y_
  , y1_
  , y2_
  , yChannelSelector_
  , z_
  -- *** Presentation attributes
  --
  -- | All SVG presentation attributes can be used as CSS properties.
  , alignmentBaseline_
  , baselineShift_
  , clipPath_
  , clipRule_
  , colorInterpolationFilters_
  , colorInterpolation_
  -- TODO crossorigin
  , color_
  , cursor_
  , direction_
  , display_
  , dominantBaseline_
  , fillOpacity_
  , fillRule_
  , fill_
  , filter_
  , floodColor_
  , floodOpacity_
  , fontFamily_
  , fontSizeAdjust_
  , fontSize_
  , fontStyle_
  , fontVariant_
  , fontWeight_
  , imageRendering_
  , letterSpacing_
  , lightingColor_
  , markerEnd_
  , markerMid_
  , markerStart_
  , mask_
  , opacity_
  , overflow_
  , pointerEvents_
  , shapeRendering_
  , stopColor_
  , stopOpacity_
  , strokeDasharray_
  , strokeDashoffset_
  , strokeLinecap_
  , strokeLinejoin_
  , strokeMiterlimit_
  , strokeOpacity_
  , strokeWidth_
  , stroke_
  , textAnchor_
  , textDecoration_
  , textRendering_
  , unicodeBidi_
  , visibility_
  , wordSpacing_
  , writingMode_
  ) where
-----------------------------------------------------------------------------
import Miso.Html.Types ( Attribute )
import Miso.Html.Property ( textProp )
import Miso.String        ( MisoString )
-----------------------------------------------------------------------------
attr :: MisoString -> MisoString -> Attribute action
attr = textProp
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accumulate>
accumulate_ ::  MisoString -> Attribute action
accumulate_ = attr "accumulate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/additive>
additive_ ::  MisoString -> Attribute action
additive_ = attr "additive"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/amplitude>
amplitude_ ::  MisoString -> Attribute action
amplitude_ = attr "amplitude"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeName>
attributeName_ ::  MisoString -> Attribute action
attributeName_ = attr "attributeName"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/azimuth>
azimuth_ ::  MisoString -> Attribute action
azimuth_ = attr "azimuth"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseFrequency>
baseFrequency_ ::  MisoString -> Attribute action
baseFrequency_ = attr "baseFrequency"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin>
begin_ ::  MisoString -> Attribute action
begin_ = attr "begin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bias>
bias_ ::  MisoString -> Attribute action
bias_ = attr "bias"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/by>
by_ ::  MisoString -> Attribute action
by_ = attr "by"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/calcMode>
calcMode_ ::  MisoString -> Attribute action
calcMode_ = attr "calcMode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/class>
class_' ::  MisoString -> Attribute action
class_' = attr "class"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipPathUnits>
clipPathUnits_ ::  MisoString -> Attribute action
clipPathUnits_ = attr "clipPathUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cx>
cx_ ::  MisoString -> Attribute action
cx_ = attr "cx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cy>
cy_ ::  MisoString -> Attribute action
cy_ = attr "cy"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d>
d_ ::  MisoString -> Attribute action
d_ = attr "d"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/diffuseConstant>
diffuseConstant_ ::  MisoString -> Attribute action
diffuseConstant_ = attr "diffuseConstant"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/divisor>
divisor_ ::  MisoString -> Attribute action
divisor_ = attr "divisor"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur>
dur_ ::  MisoString -> Attribute action
dur_ = attr "dur"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dx>
dx_ ::  MisoString -> Attribute action
dx_ = attr "dx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dy>
dy_ ::  MisoString -> Attribute action
dy_ = attr "dy"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/edgeMode>
edgeMode_ ::  MisoString -> Attribute action
edgeMode_ = attr "edgeMode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/elevation>
elevation_ ::  MisoString -> Attribute action
elevation_ = attr "elevation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/end>
end_ ::  MisoString -> Attribute action
end_ = attr "end"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/exponent>
exponent_ ::  MisoString -> Attribute action
exponent_ = attr "exponent"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterUnits>
filterUnits_ ::  MisoString -> Attribute action
filterUnits_ = attr "filterUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from>
from_ ::  MisoString -> Attribute action
from_ = attr "from"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fx>
fx_ ::  MisoString -> Attribute action
fx_ = attr "fx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fy>
fy_ ::  MisoString -> Attribute action
fy_ = attr "fy"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientTransform>
gradientTransform_ ::  MisoString -> Attribute action
gradientTransform_ = attr "gradientTransform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits>
gradientUnits_ ::  MisoString -> Attribute action
gradientUnits_ = attr "gradientUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/height>
height_ ::  MisoString -> Attribute action
height_ = attr "height"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/id>
id_ ::  MisoString -> Attribute action
id_ = attr "id"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in>
in_' ::  MisoString -> Attribute action
in_' = attr "in"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in2>
in2_ ::  MisoString -> Attribute action
in2_ = attr "in2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/intercept>
intercept_ ::  MisoString -> Attribute action
intercept_ = attr "intercept"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k1>
k1_ ::  MisoString -> Attribute action
k1_ = attr "k1"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k2>
k2_ ::  MisoString -> Attribute action
k2_ = attr "k2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k3>
k3_ ::  MisoString -> Attribute action
k3_ = attr "k3"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k4>
k4_ ::  MisoString -> Attribute action
k4_ = attr "k4"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelMatrix>
kernelMatrix_ ::  MisoString -> Attribute action
kernelMatrix_ = attr "kernelMatrix"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyPoints>
keyPoints_ ::  MisoString -> Attribute action
keyPoints_ = attr "keyPoints"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keySplines>
keySplines_ ::  MisoString -> Attribute action
keySplines_ = attr "keySplines"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyTimes>
keyTimes_ ::  MisoString -> Attribute action
keyTimes_ = attr "keyTimes"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lang>
lang_ ::  MisoString -> Attribute action
lang_ = attr "lang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lengthAdjust>
lengthAdjust_ ::  MisoString -> Attribute action
lengthAdjust_ = attr "lengthAdjust"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/limitingConeAngle>
limitingConeAngle_ ::  MisoString -> Attribute action
limitingConeAngle_ = attr "limitingConeAngle"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerHeight>
markerHeight_ ::  MisoString -> Attribute action
markerHeight_ = attr "markerHeight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits>
markerUnits_ ::  MisoString -> Attribute action
markerUnits_ = attr "markerUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerWidth>
markerWidth_ ::  MisoString -> Attribute action
markerWidth_ = attr "markerWidth"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskContentUnits>
maskContentUnits_ ::  MisoString -> Attribute action
maskContentUnits_ = attr "maskContentUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskUnits>
maskUnits_ ::  MisoString -> Attribute action
maskUnits_ = attr "maskUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/max>
max_ ::  MisoString -> Attribute action
max_ = attr "max"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/media>
media_ ::  MisoString -> Attribute action
media_ = attr "media"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/method>
method_ ::  MisoString -> Attribute action
method_ = attr "method"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/min>
min_ ::  MisoString -> Attribute action
min_ = attr "min"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mode>
mode_ ::  MisoString -> Attribute action
mode_ = attr "mode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/numOctaves>
numOctaves_ ::  MisoString -> Attribute action
numOctaves_ = attr "numOctaves"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/operator>
operator_ ::  MisoString -> Attribute action
operator_ = attr "operator"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/order>
order_ ::  MisoString -> Attribute action
order_ = attr "order"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orient>
orient_ ::  MisoString -> Attribute action
orient_ = attr "orient"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/origin>
origin_ ::  MisoString -> Attribute action
origin_ = attr "origin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/path>
path_ ::  MisoString -> Attribute action
path_ = attr "path"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pathLength>
pathLength_ ::  MisoString -> Attribute action
pathLength_ = attr "pathLength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternContentUnits>
patternContentUnits_ ::  MisoString -> Attribute action
patternContentUnits_ = attr "patternContentUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternTransform>
patternTransform_ ::  MisoString -> Attribute action
patternTransform_ = attr "patternTransform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternUnits>
patternUnits_ ::  MisoString -> Attribute action
patternUnits_ = attr "patternUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/points>
points_ ::  MisoString -> Attribute action
points_ = attr "points"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtX>
pointsAtX_ ::  MisoString -> Attribute action
pointsAtX_ = attr "pointsAtX"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtY>
pointsAtY_ ::  MisoString -> Attribute action
pointsAtY_ = attr "pointsAtY"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtZ>
pointsAtZ_ ::  MisoString -> Attribute action
pointsAtZ_ = attr "pointsAtZ"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAlpha>
preserveAlpha_ ::  MisoString -> Attribute action
preserveAlpha_ = attr "preserveAlpha"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio>
preserveAspectRatio_ ::  MisoString -> Attribute action
preserveAspectRatio_ = attr "preserveAspectRatio"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/primitiveUnits>
primitiveUnits_ ::  MisoString -> Attribute action
primitiveUnits_ = attr "primitiveUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/r>
r_ ::  MisoString -> Attribute action
r_ = attr "r"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/radius>
radius_ ::  MisoString -> Attribute action
radius_ = attr "radius"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refX>
refX_ ::  MisoString -> Attribute action
refX_ = attr "refX"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refY>
refY_ ::  MisoString -> Attribute action
refY_ = attr "refY"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatCount>
repeatCount_ ::  MisoString -> Attribute action
repeatCount_ = attr "repeatCount"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatDur>
repeatDur_ ::  MisoString -> Attribute action
repeatDur_ = attr "repeatDur"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/restart>
restart_ ::  MisoString -> Attribute action
restart_ = attr "restart"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/result>
result_ ::  MisoString -> Attribute action
result_ = attr "result"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rotate>
rotate_ ::  MisoString -> Attribute action
rotate_ = attr "rotate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rx>
rx_ ::  MisoString -> Attribute action
rx_ = attr "rx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ry>
ry_ ::  MisoString -> Attribute action
ry_ = attr "ry"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/scale>
scale_ ::  MisoString -> Attribute action
scale_ = attr "scale"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/seed>
seed_ ::  MisoString -> Attribute action
seed_ = attr "seed"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/slope>
slope_ ::  MisoString -> Attribute action
slope_ = attr "slope"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spacing>
spacing_ ::  MisoString -> Attribute action
spacing_ = attr "spacing"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularConstant>
specularConstant_ ::  MisoString -> Attribute action
specularConstant_ = attr "specularConstant"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularExponent>
specularExponent_ ::  MisoString -> Attribute action
specularExponent_ = attr "specularExponent"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spreadMethod>
spreadMethod_ ::  MisoString -> Attribute action
spreadMethod_ = attr "spreadMethod"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/startOffset>
startOffset_ ::  MisoString -> Attribute action
startOffset_ = attr "startOffset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stdDeviation>
stdDeviation_ ::  MisoString -> Attribute action
stdDeviation_ = attr "stdDeviation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stitchTiles>
stitchTiles_ ::  MisoString -> Attribute action
stitchTiles_ = attr "stitchTiles"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/style>
style_ ::  MisoString -> Attribute action
style_ = attr "style"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/surfaceScale>
surfaceScale_ ::  MisoString -> Attribute action
surfaceScale_ = attr "surfaceScale"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/systemLanguage>
systemLanguage_ ::  MisoString -> Attribute action
systemLanguage_ = attr "systemLanguage"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/tableValues>
tableValues_ ::  MisoString -> Attribute action
tableValues_ = attr "tableValues"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/target>
target_ ::  MisoString -> Attribute action
target_ = attr "target"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetX>
targetX_ ::  MisoString -> Attribute action
targetX_ = attr "targetX"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetY>
targetY_ ::  MisoString -> Attribute action
targetY_ = attr "targetY"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textLength>
textLength_ ::  MisoString -> Attribute action
textLength_ = attr "textLength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to>
to_ ::  MisoString -> Attribute action
to_ = attr "to"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
transform_ ::  MisoString -> Attribute action
transform_ = attr "transform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/type>
type_' ::  MisoString -> Attribute action
type_' = attr "type"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values>
values_ ::  MisoString -> Attribute action
values_ = attr "values"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewBox>
viewBox_ ::  MisoString -> Attribute action
viewBox_ = attr "viewBox"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/width>
width_ ::  MisoString -> Attribute action
width_ = attr "width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x>
x_ ::  MisoString -> Attribute action
x_ = attr "x"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x1>
x1_ ::  MisoString -> Attribute action
x1_ = attr "x1"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x2>
x2_ ::  MisoString -> Attribute action
x2_ = attr "x2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xChannelSelector>
xChannelSelector_ ::  MisoString -> Attribute action
xChannelSelector_ = attr "x-channel-selector"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y>
y_ ::  MisoString -> Attribute action
y_ = attr "y"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y1>
y1_ ::  MisoString -> Attribute action
y1_ = attr "y1"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y2>
y2_ ::  MisoString -> Attribute action
y2_ = attr "y2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/yChannelSelector>
yChannelSelector_ ::  MisoString -> Attribute action
yChannelSelector_ = attr "yChannelSelector"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/z>
z_ ::  MisoString -> Attribute action
z_ = attr "z"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline>
alignmentBaseline_ ::  MisoString -> Attribute action
alignmentBaseline_ = attr "alignment-baseline"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseline-shift>
baselineShift_ ::  MisoString -> Attribute action
baselineShift_ = attr "baseline-shift"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip-path>
clipPath_ ::  MisoString -> Attribute action
clipPath_ = attr "clip-path"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip-rule>
clipRule_ ::  MisoString -> Attribute action
clipRule_ = attr "clip-rule"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color-interpolation-filters>
colorInterpolationFilters_ ::  MisoString -> Attribute action
colorInterpolationFilters_ = attr "color-interpolation-filters"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color-interpolation>
colorInterpolation_ ::  MisoString -> Attribute action
colorInterpolation_ = attr "color-interpolation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color>
color_ ::  MisoString -> Attribute action
color_ = attr "color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cursor>
cursor_ ::  MisoString -> Attribute action
cursor_ = attr "cursor"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/direction>
direction_ ::  MisoString -> Attribute action
direction_ = attr "direction"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/display>
display_ ::  MisoString -> Attribute action
display_ = attr "display"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dominant-baseline>
dominantBaseline_ ::  MisoString -> Attribute action
dominantBaseline_ = attr "dominant-baseline"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-opacity>
fillOpacity_ ::  MisoString -> Attribute action
fillOpacity_ = attr "fill-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-rule>
fillRule_ ::  MisoString -> Attribute action
fillRule_ = attr "fill-rule"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
fill_ ::  MisoString -> Attribute action
fill_ = attr "fill"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filter>
filter_ ::  MisoString -> Attribute action
filter_ = attr "filter"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/flood-color>
floodColor_ ::  MisoString -> Attribute action
floodColor_ = attr "flood-color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/flood-opacity>
floodOpacity_ ::  MisoString -> Attribute action
floodOpacity_ = attr "flood-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-family>
fontFamily_ ::  MisoString -> Attribute action
fontFamily_ = attr "font-family"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-size-adjust>
fontSizeAdjust_ ::  MisoString -> Attribute action
fontSizeAdjust_ = attr "font-size-adjust"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-size>
fontSize_ ::  MisoString -> Attribute action
fontSize_ = attr "font-size"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-style>
fontStyle_ ::  MisoString -> Attribute action
fontStyle_ = attr "font-style"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-variant>
fontVariant_ ::  MisoString -> Attribute action
fontVariant_ = attr "font-variant"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-weight>
fontWeight_ ::  MisoString -> Attribute action
fontWeight_ = attr "font-weight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/image-rendering>
imageRendering_ ::  MisoString -> Attribute action
imageRendering_ = attr "image-rendering"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/letter-spacing>
letterSpacing_ ::  MisoString -> Attribute action
letterSpacing_ = attr "letter-spacing"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lighting-color>
lightingColor_ ::  MisoString -> Attribute action
lightingColor_ = attr "lighting-color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/marker-end>
markerEnd_ ::  MisoString -> Attribute action
markerEnd_ = attr "marker-end"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/marker-mid>
markerMid_ ::  MisoString -> Attribute action
markerMid_ = attr "marker-mid"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/marker-start>
markerStart_ ::  MisoString -> Attribute action
markerStart_ = attr "marker-start"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mask>
mask_ ::  MisoString -> Attribute action
mask_ = attr "mask"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/opacity>
opacity_ ::  MisoString -> Attribute action
opacity_ = attr "opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overflow>
overflow_ ::  MisoString -> Attribute action
overflow_ = attr "overflow"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointer-events>
pointerEvents_ ::  MisoString -> Attribute action
pointerEvents_ = attr "pointer-events"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/shape-rendering>
shapeRendering_ ::  MisoString -> Attribute action
shapeRendering_ = attr "shape-rendering"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stop-color>
stopColor_ ::  MisoString -> Attribute action
stopColor_ = attr "stop-color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stop-opacity>
stopOpacity_ ::  MisoString -> Attribute action
stopOpacity_ = attr "stop-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-dasharray>
strokeDasharray_ ::  MisoString -> Attribute action
strokeDasharray_ = attr "stroke-dasharray"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-dashoffset>
strokeDashoffset_ ::  MisoString -> Attribute action
strokeDashoffset_ = attr "stroke-dashoffset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-linecap>
strokeLinecap_ ::  MisoString -> Attribute action
strokeLinecap_ = attr "stroke-linecap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-linejoin>
strokeLinejoin_ ::  MisoString -> Attribute action
strokeLinejoin_ = attr "stroke-linejoin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-miterlimit>
strokeMiterlimit_ ::  MisoString -> Attribute action
strokeMiterlimit_ = attr "stroke-miterlimit"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-opacity>
strokeOpacity_ ::  MisoString -> Attribute action
strokeOpacity_ = attr "stroke-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width>
strokeWidth_ ::  MisoString -> Attribute action
strokeWidth_ = attr "stroke-width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke>
stroke_ ::  MisoString -> Attribute action
stroke_ = attr "stroke"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-anchor>
textAnchor_ ::  MisoString -> Attribute action
textAnchor_ = attr "text-anchor"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-decoration>
textDecoration_ ::  MisoString -> Attribute action
textDecoration_ = attr "text-decoration"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-rendering>
textRendering_ ::  MisoString -> Attribute action
textRendering_ = attr "text-rendering"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicode-bidi>
unicodeBidi_ ::  MisoString -> Attribute action
unicodeBidi_ = attr "unicode-bidi"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/visibility>
visibility_ ::  MisoString -> Attribute action
visibility_ = attr "visibility"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/word-spacing>
wordSpacing_ ::  MisoString -> Attribute action
wordSpacing_ = attr "word-spacing"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/writing-mode>
writingMode_ ::  MisoString -> Attribute action
writingMode_ = attr "writing-mode"
-----------------------------------------------------------------------------
