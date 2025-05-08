-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute>
--
----------------------------------------------------------------------------
module Miso.Svg.Property
  ( -- *** Regular Attributes
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
  , decoding_
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
  , fr_
  , from_
  , fx_
  , fy_
  , gradientTransform_
  , gradientUnits_
  , height_
  , href_
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
  , origin_
  , paintOrder_
  , path_
  , pathLength_
  , patternContentUnits_
  , patternTransform_
  , patternUnits_
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
  , side_
  , slope_
  , spacing_
  , specularConstant_
  , specularExponent_
  , spreadMethod_
  , startOffset_
  , stdDeviation_
  , stitchTiles_
  , surfaceScale_
  , systemLanguage_
  , tabindex_
  , tableValues_
  , target_
  , targetX_
  , targetY_
  , textLength_
  , to_
  , transform_
  , transformOrigin_
  , type_'
  , values_
  , vectorEffect_
  , viewBox_
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
  , color_
  , colorInterpolation_
  , colorInterpolationFilters_
  , crossorigin_
  , cursor_
  , direction_
  , display_
  , dominantBaseline_
  , fill_
  , fillOpacity_
  , fillRule_
  , filter_
  , floodColor_
  , floodOpacity_
  , fontFamily_
  , fontSize_
  , fontSizeAdjust_
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
  , stroke_
  , strokeDasharray_
  , strokeDashoffset_
  , strokeLinecap_
  , strokeLinejoin_
  , strokeMiterlimit_
  , strokeOpacity_
  , strokeWidth_
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
import Miso.Property ( textProp )
import Miso.String ( MisoString )
-----------------------------------------------------------------------------
attr :: MisoString -> MisoString -> Attribute action
attr = textProp
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/accumulate>
accumulate_ ::  MisoString -> Attribute action
accumulate_ = attr "accumulate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/additive>
additive_ ::  MisoString -> Attribute action
additive_ = attr "additive"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/amplitude>
amplitude_ ::  MisoString -> Attribute action
amplitude_ = attr "amplitude"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/attributeName>
attributeName_ ::  MisoString -> Attribute action
attributeName_ = attr "attributeName"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/azimuth>
azimuth_ ::  MisoString -> Attribute action
azimuth_ = attr "azimuth"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/baseFrequency>
baseFrequency_ ::  MisoString -> Attribute action
baseFrequency_ = attr "baseFrequency"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/begin>
begin_ ::  MisoString -> Attribute action
begin_ = attr "begin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/bias>
bias_ ::  MisoString -> Attribute action
bias_ = attr "bias"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/by>
by_ ::  MisoString -> Attribute action
by_ = attr "by"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/calcMode>
calcMode_ ::  MisoString -> Attribute action
calcMode_ = attr "calcMode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/class>
class_' ::  MisoString -> Attribute action
class_' = attr "class"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clipPathUnits>
clipPathUnits_ ::  MisoString -> Attribute action
clipPathUnits_ = attr "clipPathUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cx>
cx_ ::  MisoString -> Attribute action
cx_ = attr "cx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cy>
cy_ ::  MisoString -> Attribute action
cy_ = attr "cy"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d>
d_ ::  MisoString -> Attribute action
d_ = attr "d"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/decoding>
decoding_ ::  MisoString -> Attribute action
decoding_ = attr "decoding"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/diffuseConstant>
diffuseConstant_ ::  MisoString -> Attribute action
diffuseConstant_ = attr "diffuseConstant"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/divisor>
divisor_ ::  MisoString -> Attribute action
divisor_ = attr "divisor"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dur>
dur_ ::  MisoString -> Attribute action
dur_ = attr "dur"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dx>
dx_ ::  MisoString -> Attribute action
dx_ = attr "dx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dy>
dy_ ::  MisoString -> Attribute action
dy_ = attr "dy"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/edgeMode>
edgeMode_ ::  MisoString -> Attribute action
edgeMode_ = attr "edgeMode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/elevation>
elevation_ ::  MisoString -> Attribute action
elevation_ = attr "elevation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/end>
end_ ::  MisoString -> Attribute action
end_ = attr "end"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/exponent>
exponent_ ::  MisoString -> Attribute action
exponent_ = attr "exponent"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/filterUnits>
filterUnits_ ::  MisoString -> Attribute action
filterUnits_ = attr "filterUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fr>
fr_ ::  MisoString -> Attribute action
fr_ = attr "fr"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/from>
from_ ::  MisoString -> Attribute action
from_ = attr "from"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fx>
fx_ ::  MisoString -> Attribute action
fx_ = attr "fx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fy>
fy_ ::  MisoString -> Attribute action
fy_ = attr "fy"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/gradientTransform>
gradientTransform_ ::  MisoString -> Attribute action
gradientTransform_ = attr "gradientTransform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/gradientUnits>
gradientUnits_ ::  MisoString -> Attribute action
gradientUnits_ = attr "gradientUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/height>
height_ ::  MisoString -> Attribute action
height_ = attr "height"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/href>
href_ ::  MisoString -> Attribute action
href_ = attr "href"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/id>
id_ ::  MisoString -> Attribute action
id_ = attr "id"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/in>
in_' ::  MisoString -> Attribute action
in_' = attr "in"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/in2>
in2_ ::  MisoString -> Attribute action
in2_ = attr "in2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/intercept>
intercept_ ::  MisoString -> Attribute action
intercept_ = attr "intercept"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k1>
k1_ ::  MisoString -> Attribute action
k1_ = attr "k1"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k2>
k2_ ::  MisoString -> Attribute action
k2_ = attr "k2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k3>
k3_ ::  MisoString -> Attribute action
k3_ = attr "k3"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k4>
k4_ ::  MisoString -> Attribute action
k4_ = attr "k4"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/kernelMatrix>
kernelMatrix_ ::  MisoString -> Attribute action
kernelMatrix_ = attr "kernelMatrix"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keyPoints>
keyPoints_ ::  MisoString -> Attribute action
keyPoints_ = attr "keyPoints"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keySplines>
keySplines_ ::  MisoString -> Attribute action
keySplines_ = attr "keySplines"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keyTimes>
keyTimes_ ::  MisoString -> Attribute action
keyTimes_ = attr "keyTimes"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lang>
lang_ ::  MisoString -> Attribute action
lang_ = attr "lang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lengthAdjust>
lengthAdjust_ ::  MisoString -> Attribute action
lengthAdjust_ = attr "lengthAdjust"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/limitingConeAngle>
limitingConeAngle_ ::  MisoString -> Attribute action
limitingConeAngle_ = attr "limitingConeAngle"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerHeight>
markerHeight_ ::  MisoString -> Attribute action
markerHeight_ = attr "markerHeight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerUnits>
markerUnits_ ::  MisoString -> Attribute action
markerUnits_ = attr "markerUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerWidth>
markerWidth_ ::  MisoString -> Attribute action
markerWidth_ = attr "markerWidth"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/maskContentUnits>
maskContentUnits_ ::  MisoString -> Attribute action
maskContentUnits_ = attr "maskContentUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/maskUnits>
maskUnits_ ::  MisoString -> Attribute action
maskUnits_ = attr "maskUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/max>
max_ ::  MisoString -> Attribute action
max_ = attr "max"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/media>
media_ ::  MisoString -> Attribute action
media_ = attr "media"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/method>
method_ ::  MisoString -> Attribute action
method_ = attr "method"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/min>
min_ ::  MisoString -> Attribute action
min_ = attr "min"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/mode>
mode_ ::  MisoString -> Attribute action
mode_ = attr "mode"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/numOctaves>
numOctaves_ ::  MisoString -> Attribute action
numOctaves_ = attr "numOctaves"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/operator>
operator_ ::  MisoString -> Attribute action
operator_ = attr "operator"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/order>
order_ ::  MisoString -> Attribute action
order_ = attr "order"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/orient>
orient_ ::  MisoString -> Attribute action
orient_ = attr "orient"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/origin>
origin_ ::  MisoString -> Attribute action
origin_ = attr "origin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/path>
path_ ::  MisoString -> Attribute action
path_ = attr "path"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/paint-order>
paintOrder_ ::  MisoString -> Attribute action
paintOrder_ = attr "paint-order"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pathLength>
pathLength_ ::  MisoString -> Attribute action
pathLength_ = attr "pathLength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternContentUnits>
patternContentUnits_ ::  MisoString -> Attribute action
patternContentUnits_ = attr "patternContentUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternTransform>
patternTransform_ ::  MisoString -> Attribute action
patternTransform_ = attr "patternTransform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternUnits>
patternUnits_ ::  MisoString -> Attribute action
patternUnits_ = attr "patternUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/points>
points_ ::  MisoString -> Attribute action
points_ = attr "points"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtX>
pointsAtX_ ::  MisoString -> Attribute action
pointsAtX_ = attr "pointsAtX"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtY>
pointsAtY_ ::  MisoString -> Attribute action
pointsAtY_ = attr "pointsAtY"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtZ>
pointsAtZ_ ::  MisoString -> Attribute action
pointsAtZ_ = attr "pointsAtZ"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAlpha>
preserveAlpha_ ::  MisoString -> Attribute action
preserveAlpha_ = attr "preserveAlpha"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAspectRatio>
preserveAspectRatio_ ::  MisoString -> Attribute action
preserveAspectRatio_ = attr "preserveAspectRatio"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/primitiveUnits>
primitiveUnits_ ::  MisoString -> Attribute action
primitiveUnits_ = attr "primitiveUnits"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/r>
r_ ::  MisoString -> Attribute action
r_ = attr "r"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/radius>
radius_ ::  MisoString -> Attribute action
radius_ = attr "radius"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/refX>
refX_ ::  MisoString -> Attribute action
refX_ = attr "refX"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/refY>
refY_ ::  MisoString -> Attribute action
refY_ = attr "refY"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/repeatCount>
repeatCount_ ::  MisoString -> Attribute action
repeatCount_ = attr "repeatCount"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/repeatDur>
repeatDur_ ::  MisoString -> Attribute action
repeatDur_ = attr "repeatDur"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/restart>
restart_ ::  MisoString -> Attribute action
restart_ = attr "restart"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/result>
result_ ::  MisoString -> Attribute action
result_ = attr "result"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/rotate>
rotate_ ::  MisoString -> Attribute action
rotate_ = attr "rotate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/rx>
rx_ ::  MisoString -> Attribute action
rx_ = attr "rx"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/ry>
ry_ ::  MisoString -> Attribute action
ry_ = attr "ry"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/scale>
scale_ ::  MisoString -> Attribute action
scale_ = attr "scale"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/seed>
seed_ ::  MisoString -> Attribute action
seed_ = attr "seed"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/side>
side_ ::  MisoString -> Attribute action
side_ = attr "side"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/slope>
slope_ ::  MisoString -> Attribute action
slope_ = attr "slope"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/spacing>
spacing_ ::  MisoString -> Attribute action
spacing_ = attr "spacing"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/specularConstant>
specularConstant_ ::  MisoString -> Attribute action
specularConstant_ = attr "specularConstant"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/specularExponent>
specularExponent_ ::  MisoString -> Attribute action
specularExponent_ = attr "specularExponent"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/spreadMethod>
spreadMethod_ ::  MisoString -> Attribute action
spreadMethod_ = attr "spreadMethod"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/startOffset>
startOffset_ ::  MisoString -> Attribute action
startOffset_ = attr "startOffset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stdDeviation>
stdDeviation_ ::  MisoString -> Attribute action
stdDeviation_ = attr "stdDeviation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stitchTiles>
stitchTiles_ ::  MisoString -> Attribute action
stitchTiles_ = attr "stitchTiles"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/surfaceScale>
surfaceScale_ ::  MisoString -> Attribute action
surfaceScale_ = attr "surfaceScale"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/systemLanguage>
systemLanguage_ ::  MisoString -> Attribute action
systemLanguage_ = attr "systemLanguage"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/tabindex>
tabindex_ ::  MisoString -> Attribute action
tabindex_ = attr "tabindex"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/tableValues>
tableValues_ ::  MisoString -> Attribute action
tableValues_ = attr "tableValues"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/target>
target_ ::  MisoString -> Attribute action
target_ = attr "target"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/targetX>
targetX_ ::  MisoString -> Attribute action
targetX_ = attr "targetX"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/targetY>
targetY_ ::  MisoString -> Attribute action
targetY_ = attr "targetY"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/textLength>
textLength_ ::  MisoString -> Attribute action
textLength_ = attr "textLength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/to>
to_ ::  MisoString -> Attribute action
to_ = attr "to"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/transform>
transform_ ::  MisoString -> Attribute action
transform_ = attr "transform"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/transform-origin>
transformOrigin_ ::  MisoString -> Attribute action
transformOrigin_ = attr "transform-origin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/type>
type_' ::  MisoString -> Attribute action
type_' = attr "type"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/values>
values_ ::  MisoString -> Attribute action
values_ = attr "values"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/vector-effect>
vectorEffect_ ::  MisoString -> Attribute action
vectorEffect_ = attr "vector-effect"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/viewBox>
viewBox_ ::  MisoString -> Attribute action
viewBox_ = attr "viewBox"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/width>
width_ ::  MisoString -> Attribute action
width_ = attr "width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x>
x_ ::  MisoString -> Attribute action
x_ = attr "x"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x1>
x1_ ::  MisoString -> Attribute action
x1_ = attr "x1"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x2>
x2_ ::  MisoString -> Attribute action
x2_ = attr "x2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/xChannelSelector>
xChannelSelector_ ::  MisoString -> Attribute action
xChannelSelector_ = attr "x-channel-selector"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y>
y_ ::  MisoString -> Attribute action
y_ = attr "y"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y1>
y1_ ::  MisoString -> Attribute action
y1_ = attr "y1"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y2>
y2_ ::  MisoString -> Attribute action
y2_ = attr "y2"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/yChannelSelector>
yChannelSelector_ ::  MisoString -> Attribute action
yChannelSelector_ = attr "yChannelSelector"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/z>
z_ ::  MisoString -> Attribute action
z_ = attr "z"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/alignment-baseline>
alignmentBaseline_ ::  MisoString -> Attribute action
alignmentBaseline_ = attr "alignment-baseline"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/baseline-shift>
baselineShift_ ::  MisoString -> Attribute action
baselineShift_ = attr "baseline-shift"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clip-path>
clipPath_ ::  MisoString -> Attribute action
clipPath_ = attr "clip-path"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clip-rule>
clipRule_ ::  MisoString -> Attribute action
clipRule_ = attr "clip-rule"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color-interpolation>
colorInterpolation_ ::  MisoString -> Attribute action
colorInterpolation_ = attr "color-interpolation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color-interpolation-filters>
colorInterpolationFilters_ ::  MisoString -> Attribute action
colorInterpolationFilters_ = attr "color-interpolation-filters"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/crossorigin>
crossorigin_ ::  MisoString -> Attribute action
crossorigin_ = attr "crossorigin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color>
color_ ::  MisoString -> Attribute action
color_ = attr "color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cursor>
cursor_ ::  MisoString -> Attribute action
cursor_ = attr "cursor"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/direction>
direction_ ::  MisoString -> Attribute action
direction_ = attr "direction"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/display>
display_ ::  MisoString -> Attribute action
display_ = attr "display"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dominant-baseline>
dominantBaseline_ ::  MisoString -> Attribute action
dominantBaseline_ = attr "dominant-baseline"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill-opacity>
fillOpacity_ ::  MisoString -> Attribute action
fillOpacity_ = attr "fill-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill-rule>
fillRule_ ::  MisoString -> Attribute action
fillRule_ = attr "fill-rule"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill>
fill_ ::  MisoString -> Attribute action
fill_ = attr "fill"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/filter>
filter_ ::  MisoString -> Attribute action
filter_ = attr "filter"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/flood-color>
floodColor_ ::  MisoString -> Attribute action
floodColor_ = attr "flood-color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/flood-opacity>
floodOpacity_ ::  MisoString -> Attribute action
floodOpacity_ = attr "flood-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-family>
fontFamily_ ::  MisoString -> Attribute action
fontFamily_ = attr "font-family"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-size-adjust>
fontSizeAdjust_ ::  MisoString -> Attribute action
fontSizeAdjust_ = attr "font-size-adjust"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-size>
fontSize_ ::  MisoString -> Attribute action
fontSize_ = attr "font-size"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-style>
fontStyle_ ::  MisoString -> Attribute action
fontStyle_ = attr "font-style"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-variant>
fontVariant_ ::  MisoString -> Attribute action
fontVariant_ = attr "font-variant"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-weight>
fontWeight_ ::  MisoString -> Attribute action
fontWeight_ = attr "font-weight"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/image-rendering>
imageRendering_ ::  MisoString -> Attribute action
imageRendering_ = attr "image-rendering"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/letter-spacing>
letterSpacing_ ::  MisoString -> Attribute action
letterSpacing_ = attr "letter-spacing"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lighting-color>
lightingColor_ ::  MisoString -> Attribute action
lightingColor_ = attr "lighting-color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-end>
markerEnd_ ::  MisoString -> Attribute action
markerEnd_ = attr "marker-end"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-mid>
markerMid_ ::  MisoString -> Attribute action
markerMid_ = attr "marker-mid"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-start>
markerStart_ ::  MisoString -> Attribute action
markerStart_ = attr "marker-start"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/mask>
mask_ ::  MisoString -> Attribute action
mask_ = attr "mask"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/opacity>
opacity_ ::  MisoString -> Attribute action
opacity_ = attr "opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/overflow>
overflow_ ::  MisoString -> Attribute action
overflow_ = attr "overflow"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointer-events>
pointerEvents_ ::  MisoString -> Attribute action
pointerEvents_ = attr "pointer-events"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/shape-rendering>
shapeRendering_ ::  MisoString -> Attribute action
shapeRendering_ = attr "shape-rendering"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stop-color>
stopColor_ ::  MisoString -> Attribute action
stopColor_ = attr "stop-color"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stop-opacity>
stopOpacity_ ::  MisoString -> Attribute action
stopOpacity_ = attr "stop-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dasharray>
strokeDasharray_ ::  MisoString -> Attribute action
strokeDasharray_ = attr "stroke-dasharray"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dashoffset>
strokeDashoffset_ ::  MisoString -> Attribute action
strokeDashoffset_ = attr "stroke-dashoffset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-linecap>
strokeLinecap_ ::  MisoString -> Attribute action
strokeLinecap_ = attr "stroke-linecap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-linejoin>
strokeLinejoin_ ::  MisoString -> Attribute action
strokeLinejoin_ = attr "stroke-linejoin"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-miterlimit>
strokeMiterlimit_ ::  MisoString -> Attribute action
strokeMiterlimit_ = attr "stroke-miterlimit"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-opacity>
strokeOpacity_ ::  MisoString -> Attribute action
strokeOpacity_ = attr "stroke-opacity"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-width>
strokeWidth_ ::  MisoString -> Attribute action
strokeWidth_ = attr "stroke-width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke>
stroke_ ::  MisoString -> Attribute action
stroke_ = attr "stroke"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-anchor>
textAnchor_ ::  MisoString -> Attribute action
textAnchor_ = attr "text-anchor"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-decoration>
textDecoration_ ::  MisoString -> Attribute action
textDecoration_ = attr "text-decoration"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-rendering>
textRendering_ ::  MisoString -> Attribute action
textRendering_ = attr "text-rendering"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/unicode-bidi>
unicodeBidi_ ::  MisoString -> Attribute action
unicodeBidi_ = attr "unicode-bidi"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/visibility>
visibility_ ::  MisoString -> Attribute action
visibility_ = attr "visibility"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/word-spacing>
wordSpacing_ ::  MisoString -> Attribute action
wordSpacing_ = attr "word-spacing"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/writing-mode>
writingMode_ ::  MisoString -> Attribute action
writingMode_ = attr "writing-mode"
-----------------------------------------------------------------------------
