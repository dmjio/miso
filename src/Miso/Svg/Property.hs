-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Svg.Property" provides 'Miso.Types.Attribute' smart constructors
-- for all
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute SVG attributes>.
-- Every combinator takes a 'Miso.String.MisoString' value and produces an
-- 'Miso.Types.Attribute' that is set on the SVG element by the virtual DOM.
--
-- This module is __not__ re-exported by "Miso.Svg" and must be imported
-- separately. Qualify it to avoid clashing with same-named combinators
-- from "Miso.Html.Property":
--
-- @
-- import qualified "Miso.Svg.Property" as SP
-- @
--
-- = Quick start
--
-- @
-- import "Miso.Svg"
-- import qualified "Miso.Svg.Property" as SP
--
-- arrow :: 'Miso.Types.View' model action
-- arrow =
--   'Miso.Svg.Element.svg_' [ SP.'viewBox_' \"0 0 100 100\", SP.'width_' \"100\" ]
--     [ 'Miso.Svg.Element.path_'
--         [ SP.'d_'           \"M 10 50 L 90 50 M 70 30 L 90 50 L 70 70\"
--         , SP.'stroke_'      \"black\"
--         , SP.'strokeWidth_' \"4\"
--         , SP.'fill_'        \"none\"
--         , SP.'strokeLinecap_' \"round\"
--         ]
--     ]
-- @
--
-- = Attribute groups
--
-- * __Geometry__: 'cx_', 'cy_', 'r_', 'rx_', 'ry_', 'x_', 'y_',
--   'x1_', 'y1_', 'x2_', 'y2_', 'width_', 'height_', 'd_', 'points_',
--   'viewBox_', 'preserveAspectRatio_', 'pathLength_', 'textLength_'
--
-- * __Paint__: 'fill_', 'fillOpacity_', 'fillRule_', 'stroke_',
--   'strokeWidth_', 'strokeOpacity_', 'strokeDasharray_',
--   'strokeDashoffset_', 'strokeLinecap_', 'strokeLinejoin_',
--   'strokeMiterlimit_', 'color_', 'opacity_', 'stopColor_', 'stopOpacity_'
--
-- * __Transform__: 'transform_', 'transformOrigin_', 'gradientTransform_',
--   'patternTransform_', 'rotate_', 'scale_'
--
-- * __Text__: 'textAnchor_', 'textDecoration_', 'textRendering_',
--   'fontFamily_', 'fontSize_', 'fontSizeAdjust_', 'fontStyle_',
--   'fontVariant_', 'fontWeight_', 'letterSpacing_', 'wordSpacing_',
--   'direction_', 'writingMode_', 'unicodeBidi_', 'dominantBaseline_',
--   'alignmentBaseline_', 'baselineShift_', 'dx_', 'dy_'
--
-- * __Gradients__: 'gradientUnits_', 'spreadMethod_', 'fr_', 'fx_', 'fy_',
--   'offset_', 'x1_', 'y1_', 'x2_', 'y2_'
--
-- * __Filters__: 'in_\'', 'in2_', 'result_', 'mode_', 'operator_',
--   'order_', 'kernelMatrix_', 'edgeMode_', 'stdDeviation_',
--   'bias_', 'divisor_', 'amplitude_', 'exponent_', 'intercept_',
--   'slope_', 'tableValues_', 'numOctaves_', 'seed_', 'baseFrequency_',
--   'stitchTiles_', 'filterUnits_', 'primitiveUnits_',
--   'diffuseConstant_', 'specularConstant_', 'specularExponent_',
--   'surfaceScale_', 'azimuth_', 'elevation_', 'pointsAtX_',
--   'pointsAtY_', 'pointsAtZ_', 'limitingConeAngle_', 'k1_',
--   'k2_', 'k3_', 'k4_', 'xChannelSelector_', 'yChannelSelector_',
--   'preserveAlpha_', 'radius_', 'scale_'
--
-- * __Markers__: 'markerHeight_', 'markerWidth_', 'markerUnits_',
--   'markerEnd_', 'markerMid_', 'markerStart_', 'orient_', 'refX_', 'refY_'
--
-- * __Masks \/ Clips__: 'maskContentUnits_', 'maskUnits_', 'mask_',
--   'clipPath_', 'clipRule_', 'clipPathUnits_'
--
-- * __Pattern__: 'patternContentUnits_', 'patternUnits_'
--
-- * __Animation__: 'begin_', 'dur_', 'end_', 'by_', 'from_', 'to_',
--   'values_', 'calcMode_', 'keyTimes_', 'keySplines_', 'keyPoints_',
--   'repeatCount_', 'repeatDur_', 'restart_', 'additive_',
--   'accumulate_', 'attributeName_', 'type_\'', 'path_'
--
-- * __Misc__: 'cursor_', 'display_', 'filter_', 'imageRendering_',
--   'lightingColor_', 'overflow_', 'paintOrder_', 'pointerEvents_',
--   'shapeRendering_', 'vectorEffect_', 'visibility_',
--   'colorInterpolation_', 'colorInterpolationFilters_',
--   'floodColor_', 'floodOpacity_', 'crossorigin_', 'decoding_',
--   'media_', 'method_', 'side_', 'spacing_', 'startOffset_',
--   'systemLanguage_', 'target_', 'targetX_', 'targetY_', 'z_'
--
-- __Note__: Two SVG attributes clash with Haskell keywords and are given
-- disambiguated names: @'in_\''@ (for the @in@ attribute) and @'type_\''@
-- (for the @type@ attribute).
--
-- = See also
--
-- * "Miso.Svg.Element" — SVG element constructors
-- * "Miso.Html.Property" — HTML property combinators (different namespace)
-- * <https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute MDN SVG attribute reference>
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
  , offset_
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
import Miso.Property ( textProp )
import Miso.String ( MisoString )
import Miso.Types ( Attribute )
-----------------------------------------------------------------------------
attr :: MisoString -> MisoString -> Attribute model action
attr = textProp
-----------------------------------------------------------------------------
-- | [accumulate](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/accumulate) attribute
accumulate_ ::  MisoString -> Attribute model action
accumulate_ = attr "accumulate"
-----------------------------------------------------------------------------
-- | [additive](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/additive) attribute
additive_ ::  MisoString -> Attribute model action
additive_ = attr "additive"
-----------------------------------------------------------------------------
-- | [amplitude](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/amplitude) attribute
amplitude_ ::  MisoString -> Attribute model action
amplitude_ = attr "amplitude"
-----------------------------------------------------------------------------
-- | [attributeName](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/attributeName) attribute
attributeName_ ::  MisoString -> Attribute model action
attributeName_ = attr "attributeName"
-----------------------------------------------------------------------------
-- | [azimuth](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/azimuth) attribute
azimuth_ ::  MisoString -> Attribute model action
azimuth_ = attr "azimuth"
-----------------------------------------------------------------------------
-- | [baseFrequency](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/baseFrequency) attribute
baseFrequency_ ::  MisoString -> Attribute model action
baseFrequency_ = attr "baseFrequency"
-----------------------------------------------------------------------------
-- | [begin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/begin) attribute
begin_ ::  MisoString -> Attribute model action
begin_ = attr "begin"
-----------------------------------------------------------------------------
-- | [bias](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/bias) attribute
bias_ ::  MisoString -> Attribute model action
bias_ = attr "bias"
-----------------------------------------------------------------------------
-- | [by](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/by) attribute
by_ ::  MisoString -> Attribute model action
by_ = attr "by"
-----------------------------------------------------------------------------
-- | [calcMode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/calcMode) attribute
calcMode_ ::  MisoString -> Attribute model action
calcMode_ = attr "calcMode"
-----------------------------------------------------------------------------
-- | [clipPathUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clipPathUnits) attribute
clipPathUnits_ ::  MisoString -> Attribute model action
clipPathUnits_ = attr "clipPathUnits"
-----------------------------------------------------------------------------
-- | [cx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cx) attribute
cx_ ::  MisoString -> Attribute model action
cx_ = attr "cx"
-----------------------------------------------------------------------------
-- | [cy](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cy) attribute
cy_ ::  MisoString -> Attribute model action
cy_ = attr "cy"
-----------------------------------------------------------------------------
-- | [d](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d) attribute
d_ ::  MisoString -> Attribute model action
d_ = attr "d"
-----------------------------------------------------------------------------
-- | [decoding](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/decoding) attribute
--
-- @since 1.9.0.0
decoding_ ::  MisoString -> Attribute model action
decoding_ = attr "decoding"
-----------------------------------------------------------------------------
-- | [diffuseConstant](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/diffuseConstant) attribute
diffuseConstant_ ::  MisoString -> Attribute model action
diffuseConstant_ = attr "diffuseConstant"
-----------------------------------------------------------------------------
-- | [divisor](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/divisor) attribute
divisor_ ::  MisoString -> Attribute model action
divisor_ = attr "divisor"
-----------------------------------------------------------------------------
-- | [dur](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dur) attribute
dur_ ::  MisoString -> Attribute model action
dur_ = attr "dur"
-----------------------------------------------------------------------------
-- | [dx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dx) attribute
dx_ ::  MisoString -> Attribute model action
dx_ = attr "dx"
-----------------------------------------------------------------------------
-- | [dy](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dy) attribute
dy_ ::  MisoString -> Attribute model action
dy_ = attr "dy"
-----------------------------------------------------------------------------
-- | [edgeMode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/edgeMode) attribute
edgeMode_ ::  MisoString -> Attribute model action
edgeMode_ = attr "edgeMode"
-----------------------------------------------------------------------------
-- | [elevation](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/elevation) attribute
elevation_ ::  MisoString -> Attribute model action
elevation_ = attr "elevation"
-----------------------------------------------------------------------------
-- | [end](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/end) attribute
end_ ::  MisoString -> Attribute model action
end_ = attr "end"
-----------------------------------------------------------------------------
-- | [exponent](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/exponent) attribute
exponent_ ::  MisoString -> Attribute model action
exponent_ = attr "exponent"
-----------------------------------------------------------------------------
-- | [filterUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/filterUnits) attribute
filterUnits_ ::  MisoString -> Attribute model action
filterUnits_ = attr "filterUnits"
-----------------------------------------------------------------------------
-- | [fr](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fr) attribute
--
-- @since 1.9.0.0
fr_ ::  MisoString -> Attribute model action
fr_ = attr "fr"
-----------------------------------------------------------------------------
-- | [from](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/from) attribute
from_ ::  MisoString -> Attribute model action
from_ = attr "from"
-----------------------------------------------------------------------------
-- | [fx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fx) attribute
fx_ ::  MisoString -> Attribute model action
fx_ = attr "fx"
-----------------------------------------------------------------------------
-- | [fy](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fy) attribute
fy_ ::  MisoString -> Attribute model action
fy_ = attr "fy"
-----------------------------------------------------------------------------
-- | [gradientTransform](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/gradientTransform) attribute
gradientTransform_ ::  MisoString -> Attribute model action
gradientTransform_ = attr "gradientTransform"
-----------------------------------------------------------------------------
-- | [gradientUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/gradientUnits) attribute
gradientUnits_ ::  MisoString -> Attribute model action
gradientUnits_ = attr "gradientUnits"
-----------------------------------------------------------------------------
-- | [in](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/in) attribute
in_' ::  MisoString -> Attribute model action
in_' = attr "in"
-----------------------------------------------------------------------------
-- | [in2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/in2) attribute
in2_ ::  MisoString -> Attribute model action
in2_ = attr "in2"
-----------------------------------------------------------------------------
-- | [intercept](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/intercept) attribute
intercept_ ::  MisoString -> Attribute model action
intercept_ = attr "intercept"
-----------------------------------------------------------------------------
-- | [k1](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k1) attribute
k1_ ::  MisoString -> Attribute model action
k1_ = attr "k1"
-----------------------------------------------------------------------------
-- | [k2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k2) attribute
k2_ ::  MisoString -> Attribute model action
k2_ = attr "k2"
-----------------------------------------------------------------------------
-- | [k3](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k3) attribute
k3_ ::  MisoString -> Attribute model action
k3_ = attr "k3"
-----------------------------------------------------------------------------
-- | [k4](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/k4) attribute
k4_ ::  MisoString -> Attribute model action
k4_ = attr "k4"
-----------------------------------------------------------------------------
-- | [kernelMatrix](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/kernelMatrix) attribute
kernelMatrix_ ::  MisoString -> Attribute model action
kernelMatrix_ = attr "kernelMatrix"
-----------------------------------------------------------------------------
-- | [keyPoints](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keyPoints) attribute
keyPoints_ ::  MisoString -> Attribute model action
keyPoints_ = attr "keyPoints"
-----------------------------------------------------------------------------
-- | [keySplines](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keySplines) attribute
keySplines_ ::  MisoString -> Attribute model action
keySplines_ = attr "keySplines"
-----------------------------------------------------------------------------
-- | [keyTimes](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/keyTimes) attribute
keyTimes_ ::  MisoString -> Attribute model action
keyTimes_ = attr "keyTimes"
-----------------------------------------------------------------------------
-- | [lengthAdjust](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lengthAdjust) attribute
lengthAdjust_ ::  MisoString -> Attribute model action
lengthAdjust_ = attr "lengthAdjust"
-----------------------------------------------------------------------------
-- | [limitingConeAngle](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/limitingConeAngle) attribute
limitingConeAngle_ ::  MisoString -> Attribute model action
limitingConeAngle_ = attr "limitingConeAngle"
-----------------------------------------------------------------------------
-- | [markerHeight](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerHeight) attribute
markerHeight_ ::  MisoString -> Attribute model action
markerHeight_ = attr "markerHeight"
-----------------------------------------------------------------------------
-- | [markerUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerUnits) attribute
markerUnits_ ::  MisoString -> Attribute model action
markerUnits_ = attr "markerUnits"
-----------------------------------------------------------------------------
-- | [markerWidth](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/markerWidth) attribute
markerWidth_ ::  MisoString -> Attribute model action
markerWidth_ = attr "markerWidth"
-----------------------------------------------------------------------------
-- | [maskContentUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/maskContentUnits) attribute
maskContentUnits_ ::  MisoString -> Attribute model action
maskContentUnits_ = attr "maskContentUnits"
-----------------------------------------------------------------------------
-- | [maskUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/maskUnits) attribute
maskUnits_ ::  MisoString -> Attribute model action
maskUnits_ = attr "maskUnits"
-----------------------------------------------------------------------------
-- | [max](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/max) attribute
max_ ::  MisoString -> Attribute model action
max_ = attr "max"
-----------------------------------------------------------------------------
-- | [media](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/media) attribute
media_ ::  MisoString -> Attribute model action
media_ = attr "media"
-----------------------------------------------------------------------------
-- | [method](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/method) attribute
method_ ::  MisoString -> Attribute model action
method_ = attr "method"
-----------------------------------------------------------------------------
-- | [min](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/min) attribute
min_ ::  MisoString -> Attribute model action
min_ = attr "min"
-----------------------------------------------------------------------------
-- | [mode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/mode) attribute
mode_ ::  MisoString -> Attribute model action
mode_ = attr "mode"
-----------------------------------------------------------------------------
-- | [numOctaves](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/numOctaves) attribute
numOctaves_ ::  MisoString -> Attribute model action
numOctaves_ = attr "numOctaves"
-----------------------------------------------------------------------------
-- | [offset](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/stop) attribute
offset_ ::  MisoString -> Attribute model action
offset_ = attr "offset"
-----------------------------------------------------------------------------
-- | [operator](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/operator) attribute
operator_ ::  MisoString -> Attribute model action
operator_ = attr "operator"
-----------------------------------------------------------------------------
-- | [order](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/order) attribute
order_ ::  MisoString -> Attribute model action
order_ = attr "order"
-----------------------------------------------------------------------------
-- | [orient](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/orient) attribute
orient_ ::  MisoString -> Attribute model action
orient_ = attr "orient"
-----------------------------------------------------------------------------
-- | [origin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/origin) attribute
origin_ ::  MisoString -> Attribute model action
origin_ = attr "origin"
-----------------------------------------------------------------------------
-- | [path](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/path) attribute
path_ ::  MisoString -> Attribute model action
path_ = attr "path"
-----------------------------------------------------------------------------
-- | [paint-order](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/paint-order) attribute
--
-- @since 1.9.0.0
paintOrder_ ::  MisoString -> Attribute model action
paintOrder_ = attr "paint-order"
-----------------------------------------------------------------------------
-- | [pathLength](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pathLength) attribute
pathLength_ ::  MisoString -> Attribute model action
pathLength_ = attr "pathLength"
-----------------------------------------------------------------------------
-- | [patternContentUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternContentUnits) attribute
patternContentUnits_ ::  MisoString -> Attribute model action
patternContentUnits_ = attr "patternContentUnits"
-----------------------------------------------------------------------------
-- | [patternTransform](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternTransform) attribute
patternTransform_ ::  MisoString -> Attribute model action
patternTransform_ = attr "patternTransform"
-----------------------------------------------------------------------------
-- | [patternUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/patternUnits) attribute
patternUnits_ ::  MisoString -> Attribute model action
patternUnits_ = attr "patternUnits"
-----------------------------------------------------------------------------
-- | [points](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/points) attribute
points_ ::  MisoString -> Attribute model action
points_ = attr "points"
-----------------------------------------------------------------------------
-- | [pointsAtX](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtX) attribute
pointsAtX_ ::  MisoString -> Attribute model action
pointsAtX_ = attr "pointsAtX"
-----------------------------------------------------------------------------
-- | [pointsAtY](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtY) attribute
pointsAtY_ ::  MisoString -> Attribute model action
pointsAtY_ = attr "pointsAtY"
-----------------------------------------------------------------------------
-- | [pointsAtZ](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointsAtZ) attribute
pointsAtZ_ ::  MisoString -> Attribute model action
pointsAtZ_ = attr "pointsAtZ"
-----------------------------------------------------------------------------
-- | [preserveAlpha](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAlpha) attribute
preserveAlpha_ ::  MisoString -> Attribute model action
preserveAlpha_ = attr "preserveAlpha"
-----------------------------------------------------------------------------
-- | [preserveAspectRatio](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAspectRatio) attribute
preserveAspectRatio_ ::  MisoString -> Attribute model action
preserveAspectRatio_ = attr "preserveAspectRatio"
-----------------------------------------------------------------------------
-- | [primitiveUnits](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/primitiveUnits) attribute
primitiveUnits_ ::  MisoString -> Attribute model action
primitiveUnits_ = attr "primitiveUnits"
-----------------------------------------------------------------------------
-- | [r](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/r) attribute
r_ ::  MisoString -> Attribute model action
r_ = attr "r"
-----------------------------------------------------------------------------
-- | [radius](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/radius) attribute
radius_ ::  MisoString -> Attribute model action
radius_ = attr "radius"
-----------------------------------------------------------------------------
-- | [refX](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/refX) attribute
refX_ ::  MisoString -> Attribute model action
refX_ = attr "refX"
-----------------------------------------------------------------------------
-- | [refY](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/refY) attribute
refY_ ::  MisoString -> Attribute model action
refY_ = attr "refY"
-----------------------------------------------------------------------------
-- | [repeatCount](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/repeatCount) attribute
repeatCount_ ::  MisoString -> Attribute model action
repeatCount_ = attr "repeatCount"
-----------------------------------------------------------------------------
-- | [repeatDur](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/repeatDur) attribute
repeatDur_ ::  MisoString -> Attribute model action
repeatDur_ = attr "repeatDur"
-----------------------------------------------------------------------------
-- | [restart](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/restart) attribute
restart_ ::  MisoString -> Attribute model action
restart_ = attr "restart"
-----------------------------------------------------------------------------
-- | [result](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/result) attribute
result_ ::  MisoString -> Attribute model action
result_ = attr "result"
-----------------------------------------------------------------------------
-- | [rotate](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/rotate) attribute
rotate_ ::  MisoString -> Attribute model action
rotate_ = attr "rotate"
-----------------------------------------------------------------------------
-- | [rx](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/rx) attribute
rx_ ::  MisoString -> Attribute model action
rx_ = attr "rx"
-----------------------------------------------------------------------------
-- | [ry](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/ry) attribute
ry_ ::  MisoString -> Attribute model action
ry_ = attr "ry"
-----------------------------------------------------------------------------
-- | [scale](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/scale) attribute
scale_ ::  MisoString -> Attribute model action
scale_ = attr "scale"
-----------------------------------------------------------------------------
-- | [seed](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/seed) attribute
seed_ ::  MisoString -> Attribute model action
seed_ = attr "seed"
-----------------------------------------------------------------------------
-- | [side](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/side) attribute
--
-- @since 1.9.0.0
side_ ::  MisoString -> Attribute model action
side_ = attr "side"
-----------------------------------------------------------------------------
-- | [slope](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/slope) attribute
slope_ ::  MisoString -> Attribute model action
slope_ = attr "slope"
-----------------------------------------------------------------------------
-- | [spacing](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/spacing) attribute
spacing_ ::  MisoString -> Attribute model action
spacing_ = attr "spacing"
-----------------------------------------------------------------------------
-- | [specularConstant](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/specularConstant) attribute
specularConstant_ ::  MisoString -> Attribute model action
specularConstant_ = attr "specularConstant"
-----------------------------------------------------------------------------
-- | [specularExponent](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/specularExponent) attribute
specularExponent_ ::  MisoString -> Attribute model action
specularExponent_ = attr "specularExponent"
-----------------------------------------------------------------------------
-- | [spreadMethod](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/spreadMethod) attribute
spreadMethod_ ::  MisoString -> Attribute model action
spreadMethod_ = attr "spreadMethod"
-----------------------------------------------------------------------------
-- | [startOffset](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/startOffset) attribute
startOffset_ ::  MisoString -> Attribute model action
startOffset_ = attr "startOffset"
-----------------------------------------------------------------------------
-- | [stdDeviation](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stdDeviation) attribute
stdDeviation_ ::  MisoString -> Attribute model action
stdDeviation_ = attr "stdDeviation"
-----------------------------------------------------------------------------
-- | [stitchTiles](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stitchTiles) attribute
stitchTiles_ ::  MisoString -> Attribute model action
stitchTiles_ = attr "stitchTiles"
-----------------------------------------------------------------------------
-- | [surfaceScale](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/surfaceScale) attribute
surfaceScale_ ::  MisoString -> Attribute model action
surfaceScale_ = attr "surfaceScale"
-----------------------------------------------------------------------------
-- | [systemLanguage](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/systemLanguage) attribute
systemLanguage_ ::  MisoString -> Attribute model action
systemLanguage_ = attr "systemLanguage"
-----------------------------------------------------------------------------
-- | [tableValues](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/tableValues) attribute
tableValues_ ::  MisoString -> Attribute model action
tableValues_ = attr "tableValues"
-----------------------------------------------------------------------------
-- | [target](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/target) attribute
target_ ::  MisoString -> Attribute model action
target_ = attr "target"
-----------------------------------------------------------------------------
-- | [targetX](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/targetX) attribute
targetX_ ::  MisoString -> Attribute model action
targetX_ = attr "targetX"
-----------------------------------------------------------------------------
-- | [targetY](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/targetY) attribute
targetY_ ::  MisoString -> Attribute model action
targetY_ = attr "targetY"
-----------------------------------------------------------------------------
-- | [textLength](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/textLength) attribute
textLength_ ::  MisoString -> Attribute model action
textLength_ = attr "textLength"
-----------------------------------------------------------------------------
-- | [to](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/to) attribute
to_ ::  MisoString -> Attribute model action
to_ = attr "to"
-----------------------------------------------------------------------------
-- | [transform](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/transform) attribute
transform_ ::  MisoString -> Attribute model action
transform_ = attr "transform"
-----------------------------------------------------------------------------
-- | [transform-origin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/transform-origin) attribute
--
-- @since 1.9.0.0
transformOrigin_ ::  MisoString -> Attribute model action
transformOrigin_ = attr "transform-origin"
-----------------------------------------------------------------------------
-- | [type](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/type) attribute
type_' ::  MisoString -> Attribute model action
type_' = attr "type"
-----------------------------------------------------------------------------
-- | [values](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/values) attribute
values_ ::  MisoString -> Attribute model action
values_ = attr "values"
-----------------------------------------------------------------------------
-- | [vector-effect](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/vector-effect) attribute
--
-- @since 1.9.0.0
vectorEffect_ ::  MisoString -> Attribute model action
vectorEffect_ = attr "vector-effect"
-----------------------------------------------------------------------------
-- | [viewBox](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/viewBox) attribute
viewBox_ ::  MisoString -> Attribute model action
viewBox_ = attr "viewBox"
-----------------------------------------------------------------------------
-- | [x](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x) attribute
x_ ::  MisoString -> Attribute model action
x_ = attr "x"
-----------------------------------------------------------------------------
-- | [x1](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x1) attribute
x1_ ::  MisoString -> Attribute model action
x1_ = attr "x1"
-----------------------------------------------------------------------------
-- | [x2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/x2) attribute
x2_ ::  MisoString -> Attribute model action
x2_ = attr "x2"
-----------------------------------------------------------------------------
-- | [xChannelSelector](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/xChannelSelector) attribute
xChannelSelector_ ::  MisoString -> Attribute model action
xChannelSelector_ = attr "x-channel-selector"
-----------------------------------------------------------------------------
-- | [y](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y) attribute
y_ ::  MisoString -> Attribute model action
y_ = attr "y"
-----------------------------------------------------------------------------
-- | [y1](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y1) attribute
y1_ ::  MisoString -> Attribute model action
y1_ = attr "y1"
-----------------------------------------------------------------------------
-- | [y2](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/y2) attribute
y2_ ::  MisoString -> Attribute model action
y2_ = attr "y2"
-----------------------------------------------------------------------------
-- | [yChannelSelector](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/yChannelSelector) attribute
yChannelSelector_ ::  MisoString -> Attribute model action
yChannelSelector_ = attr "yChannelSelector"
-----------------------------------------------------------------------------
-- | [z](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/z) attribute
z_ ::  MisoString -> Attribute model action
z_ = attr "z"
-----------------------------------------------------------------------------
-- | [alignment-baseline](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/alignment-baseline) attribute
alignmentBaseline_ ::  MisoString -> Attribute model action
alignmentBaseline_ = attr "alignment-baseline"
-----------------------------------------------------------------------------
-- | [baseline-shift](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/baseline-shift) attribute
baselineShift_ ::  MisoString -> Attribute model action
baselineShift_ = attr "baseline-shift"
-----------------------------------------------------------------------------
-- | [clip-path](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clip-path) attribute
clipPath_ ::  MisoString -> Attribute model action
clipPath_ = attr "clip-path"
-----------------------------------------------------------------------------
-- | [clip-rule](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/clip-rule) attribute
clipRule_ ::  MisoString -> Attribute model action
clipRule_ = attr "clip-rule"
-----------------------------------------------------------------------------
-- | [color-interpolation](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color-interpolation) attribute
colorInterpolation_ ::  MisoString -> Attribute model action
colorInterpolation_ = attr "color-interpolation"
-----------------------------------------------------------------------------
-- | [color-interpolation-filters](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color-interpolation-filters) attribute
colorInterpolationFilters_ ::  MisoString -> Attribute model action
colorInterpolationFilters_ = attr "color-interpolation-filters"
-----------------------------------------------------------------------------
-- | [crossorigin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/crossorigin) attribute
--
-- @since 1.9.0.0
crossorigin_ ::  MisoString -> Attribute model action
crossorigin_ = attr "crossorigin"
-----------------------------------------------------------------------------
-- | [color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/color) attribute
color_ ::  MisoString -> Attribute model action
color_ = attr "color"
-----------------------------------------------------------------------------
-- | [cursor](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/cursor) attribute
cursor_ ::  MisoString -> Attribute model action
cursor_ = attr "cursor"
-----------------------------------------------------------------------------
-- | [direction](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/direction) attribute
direction_ ::  MisoString -> Attribute model action
direction_ = attr "direction"
-----------------------------------------------------------------------------
-- | [display](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/display) attribute
display_ ::  MisoString -> Attribute model action
display_ = attr "display"
-----------------------------------------------------------------------------
-- | [dominant-baseline](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/dominant-baseline) attribute
dominantBaseline_ ::  MisoString -> Attribute model action
dominantBaseline_ = attr "dominant-baseline"
-----------------------------------------------------------------------------
-- | [fill-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill-opacity) attribute
fillOpacity_ ::  MisoString -> Attribute model action
fillOpacity_ = attr "fill-opacity"
-----------------------------------------------------------------------------
-- | [fill-rule](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill-rule) attribute
fillRule_ ::  MisoString -> Attribute model action
fillRule_ = attr "fill-rule"
-----------------------------------------------------------------------------
-- | [fill](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/fill) attribute
fill_ ::  MisoString -> Attribute model action
fill_ = attr "fill"
-----------------------------------------------------------------------------
-- | [filter](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/filter) attribute
filter_ ::  MisoString -> Attribute model action
filter_ = attr "filter"
-----------------------------------------------------------------------------
-- | [flood-color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/flood-color) attribute
floodColor_ ::  MisoString -> Attribute model action
floodColor_ = attr "flood-color"
-----------------------------------------------------------------------------
-- | [flood-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/flood-opacity) attribute
floodOpacity_ ::  MisoString -> Attribute model action
floodOpacity_ = attr "flood-opacity"
-----------------------------------------------------------------------------
-- | [font-family](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-family) attribute
fontFamily_ ::  MisoString -> Attribute model action
fontFamily_ = attr "font-family"
-----------------------------------------------------------------------------
-- | [font-size-adjust](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-size-adjust) attribute
fontSizeAdjust_ ::  MisoString -> Attribute model action
fontSizeAdjust_ = attr "font-size-adjust"
-----------------------------------------------------------------------------
-- | [font-size](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-size) attribute
fontSize_ ::  MisoString -> Attribute model action
fontSize_ = attr "font-size"
-----------------------------------------------------------------------------
-- | [font-style](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-style) attribute
fontStyle_ ::  MisoString -> Attribute model action
fontStyle_ = attr "font-style"
-----------------------------------------------------------------------------
-- | [font-variant](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-variant) attribute
fontVariant_ ::  MisoString -> Attribute model action
fontVariant_ = attr "font-variant"
-----------------------------------------------------------------------------
-- | [font-weight](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/font-weight) attribute
fontWeight_ ::  MisoString -> Attribute model action
fontWeight_ = attr "font-weight"
-----------------------------------------------------------------------------
-- | [image-rendering](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/image-rendering) attribute
imageRendering_ ::  MisoString -> Attribute model action
imageRendering_ = attr "image-rendering"
-----------------------------------------------------------------------------
-- | [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/letter-spacing) attribute
letterSpacing_ ::  MisoString -> Attribute model action
letterSpacing_ = attr "letter-spacing"
-----------------------------------------------------------------------------
-- | [lighting-color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/lighting-color) attribute
lightingColor_ ::  MisoString -> Attribute model action
lightingColor_ = attr "lighting-color"
-----------------------------------------------------------------------------
-- | [marker-end](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-end) attribute
markerEnd_ ::  MisoString -> Attribute model action
markerEnd_ = attr "marker-end"
-----------------------------------------------------------------------------
-- | [marker-mid](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-mid) attribute
markerMid_ ::  MisoString -> Attribute model action
markerMid_ = attr "marker-mid"
-----------------------------------------------------------------------------
-- | [marker-start](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/marker-start) attribute
markerStart_ ::  MisoString -> Attribute model action
markerStart_ = attr "marker-start"
-----------------------------------------------------------------------------
-- | [mask](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/mask) attribute
mask_ ::  MisoString -> Attribute model action
mask_ = attr "mask"
-----------------------------------------------------------------------------
-- | [opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/opacity) attribute
opacity_ ::  MisoString -> Attribute model action
opacity_ = attr "opacity"
-----------------------------------------------------------------------------
-- | [overflow](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/overflow) attribute
overflow_ ::  MisoString -> Attribute model action
overflow_ = attr "overflow"
-----------------------------------------------------------------------------
-- | [pointer-events](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/pointer-events) attribute
pointerEvents_ ::  MisoString -> Attribute model action
pointerEvents_ = attr "pointer-events"
-----------------------------------------------------------------------------
-- | [shape-rendering](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/shape-rendering) attribute
shapeRendering_ ::  MisoString -> Attribute model action
shapeRendering_ = attr "shape-rendering"
-----------------------------------------------------------------------------
-- | [stop-color](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stop-color) attribute
stopColor_ ::  MisoString -> Attribute model action
stopColor_ = attr "stop-color"
-----------------------------------------------------------------------------
-- | [stop-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stop-opacity) attribute
stopOpacity_ ::  MisoString -> Attribute model action
stopOpacity_ = attr "stop-opacity"
-----------------------------------------------------------------------------
-- | [stroke-dasharray](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dasharray) attribute
strokeDasharray_ ::  MisoString -> Attribute model action
strokeDasharray_ = attr "stroke-dasharray"
-----------------------------------------------------------------------------
-- | [stroke-dashoffset](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-dashoffset) attribute
strokeDashoffset_ ::  MisoString -> Attribute model action
strokeDashoffset_ = attr "stroke-dashoffset"
-----------------------------------------------------------------------------
-- | [stroke-linecap](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-linecap) attribute
strokeLinecap_ ::  MisoString -> Attribute model action
strokeLinecap_ = attr "stroke-linecap"
-----------------------------------------------------------------------------
-- | [stroke-linejoin](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-linejoin) attribute
strokeLinejoin_ ::  MisoString -> Attribute model action
strokeLinejoin_ = attr "stroke-linejoin"
-----------------------------------------------------------------------------
-- | [stroke-miterlimit](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-miterlimit) attribute
strokeMiterlimit_ ::  MisoString -> Attribute model action
strokeMiterlimit_ = attr "stroke-miterlimit"
-----------------------------------------------------------------------------
-- | [stroke-opacity](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-opacity) attribute
strokeOpacity_ ::  MisoString -> Attribute model action
strokeOpacity_ = attr "stroke-opacity"
-----------------------------------------------------------------------------
-- | [stroke-width](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke-width) attribute
strokeWidth_ ::  MisoString -> Attribute model action
strokeWidth_ = attr "stroke-width"
-----------------------------------------------------------------------------
-- | [stroke](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/stroke) attribute
stroke_ ::  MisoString -> Attribute model action
stroke_ = attr "stroke"
-----------------------------------------------------------------------------
-- | [text-anchor](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-anchor) attribute
textAnchor_ ::  MisoString -> Attribute model action
textAnchor_ = attr "text-anchor"
-----------------------------------------------------------------------------
-- | [text-decoration](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-decoration) attribute
textDecoration_ ::  MisoString -> Attribute model action
textDecoration_ = attr "text-decoration"
-----------------------------------------------------------------------------
-- | [text-rendering](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/text-rendering) attribute
textRendering_ ::  MisoString -> Attribute model action
textRendering_ = attr "text-rendering"
-----------------------------------------------------------------------------
-- | [unicode-bidi](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/unicode-bidi) attribute
unicodeBidi_ ::  MisoString -> Attribute model action
unicodeBidi_ = attr "unicode-bidi"
-----------------------------------------------------------------------------
-- | [visibility](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/visibility) attribute
visibility_ ::  MisoString -> Attribute model action
visibility_ = attr "visibility"
-----------------------------------------------------------------------------
-- | [word-spacing](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/word-spacing) attribute
wordSpacing_ ::  MisoString -> Attribute model action
wordSpacing_ = attr "word-spacing"
-----------------------------------------------------------------------------
-- | [writing-mode](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/writing-mode) attribute
writingMode_ ::  MisoString -> Attribute model action
writingMode_ = attr "writing-mode"
-----------------------------------------------------------------------------
