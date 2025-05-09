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
-- | [accumulate](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/accumulate) attribute
accumulate_ ::  MisoString -> Attribute action
accumulate_ = attr "accumulate"
-----------------------------------------------------------------------------
-- | [additive](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/additive) attribute
additive_ ::  MisoString -> Attribute action
additive_ = attr "additive"
-----------------------------------------------------------------------------
-- | [amplitude](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/amplitude) attribute
amplitude_ ::  MisoString -> Attribute action
amplitude_ = attr "amplitude"
-----------------------------------------------------------------------------
-- | [attributeName](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/attributeName) attribute
attributeName_ ::  MisoString -> Attribute action
attributeName_ = attr "attributeName"
-----------------------------------------------------------------------------
-- | [azimuth](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/azimuth) attribute
azimuth_ ::  MisoString -> Attribute action
azimuth_ = attr "azimuth"
-----------------------------------------------------------------------------
-- | [baseFrequency](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/baseFrequency) attribute
baseFrequency_ ::  MisoString -> Attribute action
baseFrequency_ = attr "baseFrequency"
-----------------------------------------------------------------------------
-- | [begin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/begin) attribute
begin_ ::  MisoString -> Attribute action
begin_ = attr "begin"
-----------------------------------------------------------------------------
-- | [bias](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/bias) attribute
bias_ ::  MisoString -> Attribute action
bias_ = attr "bias"
-----------------------------------------------------------------------------
-- | [by](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/by) attribute
by_ ::  MisoString -> Attribute action
by_ = attr "by"
-----------------------------------------------------------------------------
-- | [calcMode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/calcMode) attribute
calcMode_ ::  MisoString -> Attribute action
calcMode_ = attr "calcMode"
-----------------------------------------------------------------------------
-- | [class](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/class) attribute
class_' ::  MisoString -> Attribute action
class_' = attr "class"
-----------------------------------------------------------------------------
-- | [clipPathUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clipPathUnits) attribute
clipPathUnits_ ::  MisoString -> Attribute action
clipPathUnits_ = attr "clipPathUnits"
-----------------------------------------------------------------------------
-- | [cx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cx) attribute
cx_ ::  MisoString -> Attribute action
cx_ = attr "cx"
-----------------------------------------------------------------------------
-- | [cy](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cy) attribute
cy_ ::  MisoString -> Attribute action
cy_ = attr "cy"
-----------------------------------------------------------------------------
-- | [d](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d) attribute
d_ ::  MisoString -> Attribute action
d_ = attr "d"
-----------------------------------------------------------------------------
-- | [decoding](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/decoding) attribute
--
-- @since 1.9.0.0
decoding_ ::  MisoString -> Attribute action
decoding_ = attr "decoding"
-----------------------------------------------------------------------------
-- | [diffuseConstant](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/diffuseConstant) attribute
diffuseConstant_ ::  MisoString -> Attribute action
diffuseConstant_ = attr "diffuseConstant"
-----------------------------------------------------------------------------
-- | [divisor](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/divisor) attribute
divisor_ ::  MisoString -> Attribute action
divisor_ = attr "divisor"
-----------------------------------------------------------------------------
-- | [dur](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dur) attribute
dur_ ::  MisoString -> Attribute action
dur_ = attr "dur"
-----------------------------------------------------------------------------
-- | [dx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dx) attribute
dx_ ::  MisoString -> Attribute action
dx_ = attr "dx"
-----------------------------------------------------------------------------
-- | [dy](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dy) attribute
dy_ ::  MisoString -> Attribute action
dy_ = attr "dy"
-----------------------------------------------------------------------------
-- | [edgeMode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/edgeMode) attribute
edgeMode_ ::  MisoString -> Attribute action
edgeMode_ = attr "edgeMode"
-----------------------------------------------------------------------------
-- | [elevation](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/elevation) attribute
elevation_ ::  MisoString -> Attribute action
elevation_ = attr "elevation"
-----------------------------------------------------------------------------
-- | [end](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/end) attribute
end_ ::  MisoString -> Attribute action
end_ = attr "end"
-----------------------------------------------------------------------------
-- | [exponent](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/exponent) attribute
exponent_ ::  MisoString -> Attribute action
exponent_ = attr "exponent"
-----------------------------------------------------------------------------
-- | [filterUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/filterUnits) attribute
filterUnits_ ::  MisoString -> Attribute action
filterUnits_ = attr "filterUnits"
-----------------------------------------------------------------------------
-- | [fr](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fr) attribute
--
-- @since 1.9.0.0
fr_ ::  MisoString -> Attribute action
fr_ = attr "fr"
-----------------------------------------------------------------------------
-- | [from](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/from) attribute
from_ ::  MisoString -> Attribute action
from_ = attr "from"
-----------------------------------------------------------------------------
-- | [fx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fx) attribute
fx_ ::  MisoString -> Attribute action
fx_ = attr "fx"
-----------------------------------------------------------------------------
-- | [fy](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fy) attribute
fy_ ::  MisoString -> Attribute action
fy_ = attr "fy"
-----------------------------------------------------------------------------
-- | [gradientTransform](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/gradientTransform) attribute
gradientTransform_ ::  MisoString -> Attribute action
gradientTransform_ = attr "gradientTransform"
-----------------------------------------------------------------------------
-- | [gradientUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/gradientUnits) attribute
gradientUnits_ ::  MisoString -> Attribute action
gradientUnits_ = attr "gradientUnits"
-----------------------------------------------------------------------------
-- | [height](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/height) attribute
height_ ::  MisoString -> Attribute action
height_ = attr "height"
-----------------------------------------------------------------------------
-- | [href](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/href) attribute
--
-- @since 1.9.0.0
href_ ::  MisoString -> Attribute action
href_ = attr "href"
-----------------------------------------------------------------------------
-- | [id](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/id) attribute
id_ ::  MisoString -> Attribute action
id_ = attr "id"
-----------------------------------------------------------------------------
-- | [in](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/in) attribute
in_' ::  MisoString -> Attribute action
in_' = attr "in"
-----------------------------------------------------------------------------
-- | [in2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/in2) attribute
in2_ ::  MisoString -> Attribute action
in2_ = attr "in2"
-----------------------------------------------------------------------------
-- | [intercept](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/intercept) attribute
intercept_ ::  MisoString -> Attribute action
intercept_ = attr "intercept"
-----------------------------------------------------------------------------
-- | [k1](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k1) attribute
k1_ ::  MisoString -> Attribute action
k1_ = attr "k1"
-----------------------------------------------------------------------------
-- | [k2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k2) attribute
k2_ ::  MisoString -> Attribute action
k2_ = attr "k2"
-----------------------------------------------------------------------------
-- | [k3](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k3) attribute
k3_ ::  MisoString -> Attribute action
k3_ = attr "k3"
-----------------------------------------------------------------------------
-- | [k4](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k4) attribute
k4_ ::  MisoString -> Attribute action
k4_ = attr "k4"
-----------------------------------------------------------------------------
-- | [kernelMatrix](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/kernelMatrix) attribute
kernelMatrix_ ::  MisoString -> Attribute action
kernelMatrix_ = attr "kernelMatrix"
-----------------------------------------------------------------------------
-- | [keyPoints](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keyPoints) attribute
keyPoints_ ::  MisoString -> Attribute action
keyPoints_ = attr "keyPoints"
-----------------------------------------------------------------------------
-- | [keySplines](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keySplines) attribute
keySplines_ ::  MisoString -> Attribute action
keySplines_ = attr "keySplines"
-----------------------------------------------------------------------------
-- | [keyTimes](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keyTimes) attribute
keyTimes_ ::  MisoString -> Attribute action
keyTimes_ = attr "keyTimes"
-----------------------------------------------------------------------------
-- | [lang](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lang) attribute
lang_ ::  MisoString -> Attribute action
lang_ = attr "lang"
-----------------------------------------------------------------------------
-- | [lengthAdjust](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lengthAdjust) attribute
lengthAdjust_ ::  MisoString -> Attribute action
lengthAdjust_ = attr "lengthAdjust"
-----------------------------------------------------------------------------
-- | [limitingConeAngle](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/limitingConeAngle) attribute
limitingConeAngle_ ::  MisoString -> Attribute action
limitingConeAngle_ = attr "limitingConeAngle"
-----------------------------------------------------------------------------
-- | [markerHeight](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerHeight) attribute
markerHeight_ ::  MisoString -> Attribute action
markerHeight_ = attr "markerHeight"
-----------------------------------------------------------------------------
-- | [markerUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerUnits) attribute
markerUnits_ ::  MisoString -> Attribute action
markerUnits_ = attr "markerUnits"
-----------------------------------------------------------------------------
-- | [markerWidth](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerWidth) attribute
markerWidth_ ::  MisoString -> Attribute action
markerWidth_ = attr "markerWidth"
-----------------------------------------------------------------------------
-- | [maskContentUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/maskContentUnits) attribute
maskContentUnits_ ::  MisoString -> Attribute action
maskContentUnits_ = attr "maskContentUnits"
-----------------------------------------------------------------------------
-- | [maskUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/maskUnits) attribute
maskUnits_ ::  MisoString -> Attribute action
maskUnits_ = attr "maskUnits"
-----------------------------------------------------------------------------
-- | [max](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/max) attribute
max_ ::  MisoString -> Attribute action
max_ = attr "max"
-----------------------------------------------------------------------------
-- | [media](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/media) attribute
media_ ::  MisoString -> Attribute action
media_ = attr "media"
-----------------------------------------------------------------------------
-- | [method](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/method) attribute
method_ ::  MisoString -> Attribute action
method_ = attr "method"
-----------------------------------------------------------------------------
-- | [min](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/min) attribute
min_ ::  MisoString -> Attribute action
min_ = attr "min"
-----------------------------------------------------------------------------
-- | [mode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/mode) attribute
mode_ ::  MisoString -> Attribute action
mode_ = attr "mode"
-----------------------------------------------------------------------------
-- | [numOctaves](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/numOctaves) attribute
numOctaves_ ::  MisoString -> Attribute action
numOctaves_ = attr "numOctaves"
-----------------------------------------------------------------------------
-- | [operator](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/operator) attribute
operator_ ::  MisoString -> Attribute action
operator_ = attr "operator"
-----------------------------------------------------------------------------
-- | [order](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/order) attribute
order_ ::  MisoString -> Attribute action
order_ = attr "order"
-----------------------------------------------------------------------------
-- | [orient](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/orient) attribute
orient_ ::  MisoString -> Attribute action
orient_ = attr "orient"
-----------------------------------------------------------------------------
-- | [origin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/origin) attribute
origin_ ::  MisoString -> Attribute action
origin_ = attr "origin"
-----------------------------------------------------------------------------
-- | [path](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/path) attribute
path_ ::  MisoString -> Attribute action
path_ = attr "path"
-----------------------------------------------------------------------------
-- | [paint-order](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/paint-order) attribute
--
-- @since 1.9.0.0
paintOrder_ ::  MisoString -> Attribute action
paintOrder_ = attr "paint-order"
-----------------------------------------------------------------------------
-- | [pathLength](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pathLength) attribute
pathLength_ ::  MisoString -> Attribute action
pathLength_ = attr "pathLength"
-----------------------------------------------------------------------------
-- | [patternContentUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternContentUnits) attribute
patternContentUnits_ ::  MisoString -> Attribute action
patternContentUnits_ = attr "patternContentUnits"
-----------------------------------------------------------------------------
-- | [patternTransform](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternTransform) attribute
patternTransform_ ::  MisoString -> Attribute action
patternTransform_ = attr "patternTransform"
-----------------------------------------------------------------------------
-- | [patternUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternUnits) attribute
patternUnits_ ::  MisoString -> Attribute action
patternUnits_ = attr "patternUnits"
-----------------------------------------------------------------------------
-- | [points](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/points) attribute
points_ ::  MisoString -> Attribute action
points_ = attr "points"
-----------------------------------------------------------------------------
-- | [pointsAtX](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtX) attribute
pointsAtX_ ::  MisoString -> Attribute action
pointsAtX_ = attr "pointsAtX"
-----------------------------------------------------------------------------
-- | [pointsAtY](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtY) attribute
pointsAtY_ ::  MisoString -> Attribute action
pointsAtY_ = attr "pointsAtY"
-----------------------------------------------------------------------------
-- | [pointsAtZ](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtZ) attribute
pointsAtZ_ ::  MisoString -> Attribute action
pointsAtZ_ = attr "pointsAtZ"
-----------------------------------------------------------------------------
-- | [preserveAlpha](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAlpha) attribute
preserveAlpha_ ::  MisoString -> Attribute action
preserveAlpha_ = attr "preserveAlpha"
-----------------------------------------------------------------------------
-- | [preserveAspectRatio](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAspectRatio) attribute
preserveAspectRatio_ ::  MisoString -> Attribute action
preserveAspectRatio_ = attr "preserveAspectRatio"
-----------------------------------------------------------------------------
-- | [primitiveUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/primitiveUnits) attribute
primitiveUnits_ ::  MisoString -> Attribute action
primitiveUnits_ = attr "primitiveUnits"
-----------------------------------------------------------------------------
-- | [r](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/r) attribute
r_ ::  MisoString -> Attribute action
r_ = attr "r"
-----------------------------------------------------------------------------
-- | [radius](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/radius) attribute
radius_ ::  MisoString -> Attribute action
radius_ = attr "radius"
-----------------------------------------------------------------------------
-- | [refX](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/refX) attribute
refX_ ::  MisoString -> Attribute action
refX_ = attr "refX"
-----------------------------------------------------------------------------
-- | [refY](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/refY) attribute
refY_ ::  MisoString -> Attribute action
refY_ = attr "refY"
-----------------------------------------------------------------------------
-- | [repeatCount](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/repeatCount) attribute
repeatCount_ ::  MisoString -> Attribute action
repeatCount_ = attr "repeatCount"
-----------------------------------------------------------------------------
-- | [repeatDur](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/repeatDur) attribute
repeatDur_ ::  MisoString -> Attribute action
repeatDur_ = attr "repeatDur"
-----------------------------------------------------------------------------
-- | [restart](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/restart) attribute
restart_ ::  MisoString -> Attribute action
restart_ = attr "restart"
-----------------------------------------------------------------------------
-- | [result](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/result) attribute
result_ ::  MisoString -> Attribute action
result_ = attr "result"
-----------------------------------------------------------------------------
-- | [rotate](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/rotate) attribute
rotate_ ::  MisoString -> Attribute action
rotate_ = attr "rotate"
-----------------------------------------------------------------------------
-- | [rx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/rx) attribute
rx_ ::  MisoString -> Attribute action
rx_ = attr "rx"
-----------------------------------------------------------------------------
-- | [ry](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/ry) attribute
ry_ ::  MisoString -> Attribute action
ry_ = attr "ry"
-----------------------------------------------------------------------------
-- | [scale](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/scale) attribute
scale_ ::  MisoString -> Attribute action
scale_ = attr "scale"
-----------------------------------------------------------------------------
-- | [seed](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/seed) attribute
seed_ ::  MisoString -> Attribute action
seed_ = attr "seed"
-----------------------------------------------------------------------------
-- | [side](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/side) attribute
--
-- @since 1.9.0.0
side_ ::  MisoString -> Attribute action
side_ = attr "side"
-----------------------------------------------------------------------------
-- | [slope](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/slope) attribute
slope_ ::  MisoString -> Attribute action
slope_ = attr "slope"
-----------------------------------------------------------------------------
-- | [spacing](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/spacing) attribute
spacing_ ::  MisoString -> Attribute action
spacing_ = attr "spacing"
-----------------------------------------------------------------------------
-- | [specularConstant](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/specularConstant) attribute
specularConstant_ ::  MisoString -> Attribute action
specularConstant_ = attr "specularConstant"
-----------------------------------------------------------------------------
-- | [specularExponent](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/specularExponent) attribute
specularExponent_ ::  MisoString -> Attribute action
specularExponent_ = attr "specularExponent"
-----------------------------------------------------------------------------
-- | [spreadMethod](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/spreadMethod) attribute
spreadMethod_ ::  MisoString -> Attribute action
spreadMethod_ = attr "spreadMethod"
-----------------------------------------------------------------------------
-- | [startOffset](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/startOffset) attribute
startOffset_ ::  MisoString -> Attribute action
startOffset_ = attr "startOffset"
-----------------------------------------------------------------------------
-- | [stdDeviation](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stdDeviation) attribute
stdDeviation_ ::  MisoString -> Attribute action
stdDeviation_ = attr "stdDeviation"
-----------------------------------------------------------------------------
-- | [stitchTiles](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stitchTiles) attribute
stitchTiles_ ::  MisoString -> Attribute action
stitchTiles_ = attr "stitchTiles"
-----------------------------------------------------------------------------
-- | [surfaceScale](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/surfaceScale) attribute
surfaceScale_ ::  MisoString -> Attribute action
surfaceScale_ = attr "surfaceScale"
-----------------------------------------------------------------------------
-- | [systemLanguage](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/systemLanguage) attribute
systemLanguage_ ::  MisoString -> Attribute action
systemLanguage_ = attr "systemLanguage"
-----------------------------------------------------------------------------
-- | [tabindex](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/tabindex) attribute
--
-- @since 1.9.0.0
tabindex_ ::  MisoString -> Attribute action
tabindex_ = attr "tabindex"
-----------------------------------------------------------------------------
-- | [tableValues](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/tableValues) attribute
tableValues_ ::  MisoString -> Attribute action
tableValues_ = attr "tableValues"
-----------------------------------------------------------------------------
-- | [target](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/target) attribute
target_ ::  MisoString -> Attribute action
target_ = attr "target"
-----------------------------------------------------------------------------
-- | [targetX](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/targetX) attribute
targetX_ ::  MisoString -> Attribute action
targetX_ = attr "targetX"
-----------------------------------------------------------------------------
-- | [targetY](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/targetY) attribute
targetY_ ::  MisoString -> Attribute action
targetY_ = attr "targetY"
-----------------------------------------------------------------------------
-- | [textLength](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/textLength) attribute
textLength_ ::  MisoString -> Attribute action
textLength_ = attr "textLength"
-----------------------------------------------------------------------------
-- | [to](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/to) attribute
to_ ::  MisoString -> Attribute action
to_ = attr "to"
-----------------------------------------------------------------------------
-- | [transform](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/transform) attribute
transform_ ::  MisoString -> Attribute action
transform_ = attr "transform"
-----------------------------------------------------------------------------
-- | [transform-origin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/transform-origin) attribute
--
-- @since 1.9.0.0
transformOrigin_ ::  MisoString -> Attribute action
transformOrigin_ = attr "transform-origin"
-----------------------------------------------------------------------------
-- | [type](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/type) attribute
type_' ::  MisoString -> Attribute action
type_' = attr "type"
-----------------------------------------------------------------------------
-- | [values](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/values) attribute
values_ ::  MisoString -> Attribute action
values_ = attr "values"
-----------------------------------------------------------------------------
-- | [vector-effect](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/vector-effect) attribute
--
-- @since 1.9.0.0
vectorEffect_ ::  MisoString -> Attribute action
vectorEffect_ = attr "vector-effect"
-----------------------------------------------------------------------------
-- | [viewBox](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/viewBox) attribute
viewBox_ ::  MisoString -> Attribute action
viewBox_ = attr "viewBox"
-----------------------------------------------------------------------------
-- | [width](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/width) attribute
width_ ::  MisoString -> Attribute action
width_ = attr "width"
-----------------------------------------------------------------------------
-- | [x](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x) attribute
x_ ::  MisoString -> Attribute action
x_ = attr "x"
-----------------------------------------------------------------------------
-- | [x1](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x1) attribute
x1_ ::  MisoString -> Attribute action
x1_ = attr "x1"
-----------------------------------------------------------------------------
-- | [x2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x2) attribute
x2_ ::  MisoString -> Attribute action
x2_ = attr "x2"
-----------------------------------------------------------------------------
-- | [xChannelSelector](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/xChannelSelector) attribute
xChannelSelector_ ::  MisoString -> Attribute action
xChannelSelector_ = attr "x-channel-selector"
-----------------------------------------------------------------------------
-- | [y](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y) attribute
y_ ::  MisoString -> Attribute action
y_ = attr "y"
-----------------------------------------------------------------------------
-- | [y1](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y1) attribute
y1_ ::  MisoString -> Attribute action
y1_ = attr "y1"
-----------------------------------------------------------------------------
-- | [y2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y2) attribute
y2_ ::  MisoString -> Attribute action
y2_ = attr "y2"
-----------------------------------------------------------------------------
-- | [yChannelSelector](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/yChannelSelector) attribute
yChannelSelector_ ::  MisoString -> Attribute action
yChannelSelector_ = attr "yChannelSelector"
-----------------------------------------------------------------------------
-- | [z](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/z) attribute
z_ ::  MisoString -> Attribute action
z_ = attr "z"
-----------------------------------------------------------------------------
-- | [alignment-baseline](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/alignment-baseline) attribute
alignmentBaseline_ ::  MisoString -> Attribute action
alignmentBaseline_ = attr "alignment-baseline"
-----------------------------------------------------------------------------
-- | [baseline-shift](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/baseline-shift) attribute
baselineShift_ ::  MisoString -> Attribute action
baselineShift_ = attr "baseline-shift"
-----------------------------------------------------------------------------
-- | [clip-path](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clip-path) attribute
clipPath_ ::  MisoString -> Attribute action
clipPath_ = attr "clip-path"
-----------------------------------------------------------------------------
-- | [clip-rule](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clip-rule) attribute
clipRule_ ::  MisoString -> Attribute action
clipRule_ = attr "clip-rule"
-----------------------------------------------------------------------------
-- | [color-interpolation](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color-interpolation) attribute
colorInterpolation_ ::  MisoString -> Attribute action
colorInterpolation_ = attr "color-interpolation"
-----------------------------------------------------------------------------
-- | [color-interpolation-filters](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color-interpolation-filters) attribute
colorInterpolationFilters_ ::  MisoString -> Attribute action
colorInterpolationFilters_ = attr "color-interpolation-filters"
-----------------------------------------------------------------------------
-- | [crossorigin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/crossorigin) attribute
--
-- @since 1.9.0.0
crossorigin_ ::  MisoString -> Attribute action
crossorigin_ = attr "crossorigin"
-----------------------------------------------------------------------------
-- | [color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color) attribute
color_ ::  MisoString -> Attribute action
color_ = attr "color"
-----------------------------------------------------------------------------
-- | [cursor](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cursor) attribute
cursor_ ::  MisoString -> Attribute action
cursor_ = attr "cursor"
-----------------------------------------------------------------------------
-- | [direction](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/direction) attribute
direction_ ::  MisoString -> Attribute action
direction_ = attr "direction"
-----------------------------------------------------------------------------
-- | [display](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/display) attribute
display_ ::  MisoString -> Attribute action
display_ = attr "display"
-----------------------------------------------------------------------------
-- | [dominant-baseline](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dominant-baseline) attribute
dominantBaseline_ ::  MisoString -> Attribute action
dominantBaseline_ = attr "dominant-baseline"
-----------------------------------------------------------------------------
-- | [fill-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill-opacity) attribute
fillOpacity_ ::  MisoString -> Attribute action
fillOpacity_ = attr "fill-opacity"
-----------------------------------------------------------------------------
-- | [fill-rule](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill-rule) attribute
fillRule_ ::  MisoString -> Attribute action
fillRule_ = attr "fill-rule"
-----------------------------------------------------------------------------
-- | [fill](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill) attribute
fill_ ::  MisoString -> Attribute action
fill_ = attr "fill"
-----------------------------------------------------------------------------
-- | [filter](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/filter) attribute
filter_ ::  MisoString -> Attribute action
filter_ = attr "filter"
-----------------------------------------------------------------------------
-- | [flood-color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/flood-color) attribute
floodColor_ ::  MisoString -> Attribute action
floodColor_ = attr "flood-color"
-----------------------------------------------------------------------------
-- | [flood-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/flood-opacity) attribute
floodOpacity_ ::  MisoString -> Attribute action
floodOpacity_ = attr "flood-opacity"
-----------------------------------------------------------------------------
-- | [font-family](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-family) attribute
fontFamily_ ::  MisoString -> Attribute action
fontFamily_ = attr "font-family"
-----------------------------------------------------------------------------
-- | [font-size-adjust](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-size-adjust) attribute
fontSizeAdjust_ ::  MisoString -> Attribute action
fontSizeAdjust_ = attr "font-size-adjust"
-----------------------------------------------------------------------------
-- | [font-size](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-size) attribute
fontSize_ ::  MisoString -> Attribute action
fontSize_ = attr "font-size"
-----------------------------------------------------------------------------
-- | [font-style](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-style) attribute
fontStyle_ ::  MisoString -> Attribute action
fontStyle_ = attr "font-style"
-----------------------------------------------------------------------------
-- | [font-variant](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-variant) attribute
fontVariant_ ::  MisoString -> Attribute action
fontVariant_ = attr "font-variant"
-----------------------------------------------------------------------------
-- | [font-weight](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-weight) attribute
fontWeight_ ::  MisoString -> Attribute action
fontWeight_ = attr "font-weight"
-----------------------------------------------------------------------------
-- | [image-rendering](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/image-rendering) attribute
imageRendering_ ::  MisoString -> Attribute action
imageRendering_ = attr "image-rendering"
-----------------------------------------------------------------------------
-- | [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/letter-spacing) attribute
letterSpacing_ ::  MisoString -> Attribute action
letterSpacing_ = attr "letter-spacing"
-----------------------------------------------------------------------------
-- | [lighting-color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lighting-color) attribute
lightingColor_ ::  MisoString -> Attribute action
lightingColor_ = attr "lighting-color"
-----------------------------------------------------------------------------
-- | [marker-end](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-end) attribute
markerEnd_ ::  MisoString -> Attribute action
markerEnd_ = attr "marker-end"
-----------------------------------------------------------------------------
-- | [marker-mid](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-mid) attribute
markerMid_ ::  MisoString -> Attribute action
markerMid_ = attr "marker-mid"
-----------------------------------------------------------------------------
-- | [marker-start](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-start) attribute
markerStart_ ::  MisoString -> Attribute action
markerStart_ = attr "marker-start"
-----------------------------------------------------------------------------
-- | [mask](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/mask) attribute
mask_ ::  MisoString -> Attribute action
mask_ = attr "mask"
-----------------------------------------------------------------------------
-- | [opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/opacity) attribute
opacity_ ::  MisoString -> Attribute action
opacity_ = attr "opacity"
-----------------------------------------------------------------------------
-- | [overflow](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/overflow) attribute
overflow_ ::  MisoString -> Attribute action
overflow_ = attr "overflow"
-----------------------------------------------------------------------------
-- | [pointer-events](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointer-events) attribute
pointerEvents_ ::  MisoString -> Attribute action
pointerEvents_ = attr "pointer-events"
-----------------------------------------------------------------------------
-- | [shape-rendering](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/shape-rendering) attribute
shapeRendering_ ::  MisoString -> Attribute action
shapeRendering_ = attr "shape-rendering"
-----------------------------------------------------------------------------
-- | [stop-color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stop-color) attribute
stopColor_ ::  MisoString -> Attribute action
stopColor_ = attr "stop-color"
-----------------------------------------------------------------------------
-- | [stop-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stop-opacity) attribute
stopOpacity_ ::  MisoString -> Attribute action
stopOpacity_ = attr "stop-opacity"
-----------------------------------------------------------------------------
-- | [stroke-dasharray](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dasharray) attribute
strokeDasharray_ ::  MisoString -> Attribute action
strokeDasharray_ = attr "stroke-dasharray"
-----------------------------------------------------------------------------
-- | [stroke-dashoffset](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dashoffset) attribute
strokeDashoffset_ ::  MisoString -> Attribute action
strokeDashoffset_ = attr "stroke-dashoffset"
-----------------------------------------------------------------------------
-- | [stroke-linecap](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-linecap) attribute
strokeLinecap_ ::  MisoString -> Attribute action
strokeLinecap_ = attr "stroke-linecap"
-----------------------------------------------------------------------------
-- | [stroke-linejoin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-linejoin) attribute
strokeLinejoin_ ::  MisoString -> Attribute action
strokeLinejoin_ = attr "stroke-linejoin"
-----------------------------------------------------------------------------
-- | [stroke-miterlimit](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-miterlimit) attribute
strokeMiterlimit_ ::  MisoString -> Attribute action
strokeMiterlimit_ = attr "stroke-miterlimit"
-----------------------------------------------------------------------------
-- | [stroke-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-opacity) attribute
strokeOpacity_ ::  MisoString -> Attribute action
strokeOpacity_ = attr "stroke-opacity"
-----------------------------------------------------------------------------
-- | [stroke-width](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-width) attribute
strokeWidth_ ::  MisoString -> Attribute action
strokeWidth_ = attr "stroke-width"
-----------------------------------------------------------------------------
-- | [stroke](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke) attribute
stroke_ ::  MisoString -> Attribute action
stroke_ = attr "stroke"
-----------------------------------------------------------------------------
-- | [text-anchor](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-anchor) attribute
textAnchor_ ::  MisoString -> Attribute action
textAnchor_ = attr "text-anchor"
-----------------------------------------------------------------------------
-- | [text-decoration](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-decoration) attribute
textDecoration_ ::  MisoString -> Attribute action
textDecoration_ = attr "text-decoration"
-----------------------------------------------------------------------------
-- | [text-rendering](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-rendering) attribute
textRendering_ ::  MisoString -> Attribute action
textRendering_ = attr "text-rendering"
-----------------------------------------------------------------------------
-- | [unicode-bidi](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/unicode-bidi) attribute
unicodeBidi_ ::  MisoString -> Attribute action
unicodeBidi_ = attr "unicode-bidi"
-----------------------------------------------------------------------------
-- | [visibility](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/visibility) attribute
visibility_ ::  MisoString -> Attribute action
visibility_ = attr "visibility"
-----------------------------------------------------------------------------
-- | [word-spacing](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/word-spacing) attribute
wordSpacing_ ::  MisoString -> Attribute action
wordSpacing_ = attr "word-spacing"
-----------------------------------------------------------------------------
-- | [writing-mode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/writing-mode) attribute
writingMode_ ::  MisoString -> Attribute action
writingMode_ = attr "writing-mode"
-----------------------------------------------------------------------------
