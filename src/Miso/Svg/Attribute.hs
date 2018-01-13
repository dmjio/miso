{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Attribute
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute>
--
----------------------------------------------------------------------------
module Miso.Svg.Attribute
  ( -- * Regular attributes
    accentHeight_
  , accelerate_
  , accumulate_
  , additive_
  , alphabetic_
  , allowReorder_
  , amplitude_
  , arabicForm_
  , ascent_
  , attributeName_
  , attributeType_
  , autoReverse_
  , azimuth_
  , baseFrequency_
  , baseProfile_
  , bbox_
  , begin_
  , bias_
  , by_
  , calcMode_
  , capHeight_
  , class_'
  , clipPathUnits_
  , contentScriptType_
  , contentStyleType_
  , cx_
  , cy_
  , d_
  , decelerate_
  , descent_
  , diffuseConstant_
  , divisor_
  , dur_
  , dx_
  , dy_
  , edgeMode_
  , elevation_
  , end_
  , exponent_
  , externalResourcesRequired_
  , filterRes_
  , filterUnits_
  , format_
  , from_
  , fx_
  , fy_
  , g1_
  , g2_
  , glyphName_
  , glyphRef_
  , gradientTransform_
  , gradientUnits_
  , hanging_
  , height_
  , horizAdvX_
  , horizOriginX_
  , horizOriginY_
  , id_
  , ideographic_
  , in_'
  , in2_
  , intercept_
  , k_
  , k1_
  , k2_
  , k3_
  , k4_
  , kernelMatrix_
  , kernelUnitLength_
  , keyPoints_
  , keySplines_
  , keyTimes_
  , lang_
  , lengthAdjust_
  , limitingConeAngle_
  , local_
  , markerHeight_
  , markerUnits_
  , markerWidth_
  , maskContentUnits_
  , maskUnits_
  , mathematical_
  , max_
  , media_
  , method_
  , min_
  , mode_
  , name_
  , numOctaves_
  , offset_
  , operator_
  , order_
  , orient_
  , orientation_
  , origin_
  , overlinePosition_
  , overlineThickness_
  , panose1_
  , path_
  , pathLength_
  , patternContentUnits_
  , patternTransform_
  , patternUnits_
  , pointOrder_
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
  , renderingIntent_
  , repeatCount_
  , repeatDur_
  , requiredExtensions_
  , requiredFeatures_
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
  , speed_
  , spreadMethod_
  , startOffset_
  , stdDeviation_
  , stemh_
  , stemv_
  , stitchTiles_
  , strikethroughPosition_
  , strikethroughThickness_
  , string_
  , style_
  , surfaceScale_
  , systemLanguage_
  , tableValues_
  , target_
  , targetX_
  , targetY_
  , textLength_
  , title_
  , to_
  , transform_
  , type_'
  , u1_
  , u2_
  , underlinePosition_
  , underlineThickness_
  , unicode_
  , unicodeRange_
  , unitsPerEm_
  , vAlphabetic_
  , vHanging_
  , vIdeographic_
  , vMathematical_
  , values_
  , version_
  , vertAdvY_
  , vertOriginX_
  , vertOriginY_
  , viewBox_
  , viewTarget_
  , width_
  , widths_
  , x_
  , xHeight_
  , x1_
  , x2_
  , xChannelSelector_
  , xlinkActuate_
  , xlinkArcrole_
  , xlinkHref_
  , xlinkRole_
  , xlinkShow_
  , xlinkTitle_
  , xlinkType_
  , xmlBase_
  , xmlLang_
  , xmlSpace_
  , y_
  , y1_
  , y2_
  , yChannelSelector_
  , z_
  , zoomAndPan_
  -- * Presentation_ attributes
  , alignmentBaseline_
  , baselineShift_
  , clipPath_
  , clipRule_
  , clip_
  , colorInterpolationFilters_
  , colorInterpolation_
  , colorProfile_
  , colorRendering_
  , color_
  , cursor_
  , direction_
  , display_
  , dominantBaseline_
  , enableBackground_
  , fillOpacity_
  , fillRule_
  , fill_
  , filter_
  , floodColor_
  , floodOpacity_
  , fontFamily_
  , fontSizeAdjust_
  , fontSize_
  , fontStretch_
  , fontStyle_
  , fontVariant_
  , fontWeight_
  , glyphOrientationHorizontal_
  , glyphOrientationVertical_
  , imageRendering_
  , kerning_
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

import Miso.Html.Internal ( Attribute )
import Miso.Html.Property ( textProp )
import Miso.String        ( MisoString )

attr :: MisoString -> MisoString -> Attribute action
attr = textProp

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accent-height>
accentHeight_ ::  MisoString -> Attribute action
accentHeight_ = attr "accent-height"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accelerate>
accelerate_ ::  MisoString -> Attribute action
accelerate_ = attr "accelerate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accumulate>
accumulate_ ::  MisoString -> Attribute action
accumulate_ = attr "accumulate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/additive>
additive_ ::  MisoString -> Attribute action
additive_ = attr "additive"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alphabetic>
alphabetic_ ::  MisoString -> Attribute action
alphabetic_ = attr "alphabetic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/allowReorder>
allowReorder_ ::  MisoString -> Attribute action
allowReorder_ = attr "allowReorder"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/amplitude>
amplitude_ ::  MisoString -> Attribute action
amplitude_ = attr "amplitude"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/arabic-form>
arabicForm_ ::  MisoString -> Attribute action
arabicForm_ = attr "arabic-form"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ascent>
ascent_ ::  MisoString -> Attribute action
ascent_ = attr "ascent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeName>
attributeName_ ::  MisoString -> Attribute action
attributeName_ = attr "attributeName"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeType>
attributeType_ ::  MisoString -> Attribute action
attributeType_ = attr "attributeType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/autoReverse>
autoReverse_ ::  MisoString -> Attribute action
autoReverse_ = attr "autoReverse"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/azimuth>
azimuth_ ::  MisoString -> Attribute action
azimuth_ = attr "azimuth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseFrequency>
baseFrequency_ ::  MisoString -> Attribute action
baseFrequency_ = attr "baseFrequency"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseProfile>
baseProfile_ ::  MisoString -> Attribute action
baseProfile_ = attr "baseProfile"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bbox>
bbox_ ::  MisoString -> Attribute action
bbox_ = attr "bbox"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin>
begin_ ::  MisoString -> Attribute action
begin_ = attr "begin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bias>
bias_ ::  MisoString -> Attribute action
bias_ = attr "bias"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/by>
by_ ::  MisoString -> Attribute action
by_ = attr "by"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/calcMode>
calcMode_ ::  MisoString -> Attribute action
calcMode_ = attr "calcMode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cap-height>
capHeight_ ::  MisoString -> Attribute action
capHeight_ = attr "cap-height"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/class>
class_' ::  MisoString -> Attribute action
class_' = attr "class"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipPathUnits>
clipPathUnits_ ::  MisoString -> Attribute action
clipPathUnits_ = attr "clipPathUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/contentScriptType>
contentScriptType_ ::  MisoString -> Attribute action
contentScriptType_ = attr "contentScriptType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/contentStyleType>
contentStyleType_ ::  MisoString -> Attribute action
contentStyleType_ = attr "contentStyleType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cx>
cx_ ::  MisoString -> Attribute action
cx_ = attr "cx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cy>
cy_ ::  MisoString -> Attribute action
cy_ = attr "cy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d>
d_ ::  MisoString -> Attribute action
d_ = attr "d"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/decelerate>
decelerate_ ::  MisoString -> Attribute action
decelerate_ = attr "decelerate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/descent>
descent_ ::  MisoString -> Attribute action
descent_ = attr "descent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/diffuseConstant>
diffuseConstant_ ::  MisoString -> Attribute action
diffuseConstant_ = attr "diffuseConstant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/divisor>
divisor_ ::  MisoString -> Attribute action
divisor_ = attr "divisor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur>
dur_ ::  MisoString -> Attribute action
dur_ = attr "dur"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dx>
dx_ ::  MisoString -> Attribute action
dx_ = attr "dx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dy>
dy_ ::  MisoString -> Attribute action
dy_ = attr "dy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/edgeMode>
edgeMode_ ::  MisoString -> Attribute action
edgeMode_ = attr "edgeMode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/elevation>
elevation_ ::  MisoString -> Attribute action
elevation_ = attr "elevation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/end>
end_ ::  MisoString -> Attribute action
end_ = attr "end"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/exponent>
exponent_ ::  MisoString -> Attribute action
exponent_ = attr "exponent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/externalResourcesRequired>
externalResourcesRequired_ ::  MisoString -> Attribute action
externalResourcesRequired_ = attr "externalResourcesRequired"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterRes>
filterRes_ ::  MisoString -> Attribute action
filterRes_ = attr "filterRes"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterUnits>
filterUnits_ ::  MisoString -> Attribute action
filterUnits_ = attr "filterUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/format>
format_ ::  MisoString -> Attribute action
format_ = attr "format"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from>
from_ ::  MisoString -> Attribute action
from_ = attr "from"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fx>
fx_ ::  MisoString -> Attribute action
fx_ = attr "fx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fy>
fy_ ::  MisoString -> Attribute action
fy_ = attr "fy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/g1>
g1_ ::  MisoString -> Attribute action
g1_ = attr "g1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/g2>
g2_ ::  MisoString -> Attribute action
g2_ = attr "g2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyph-name>
glyphName_ ::  MisoString -> Attribute action
glyphName_ = attr "glyph-name"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphRef>
glyphRef_ ::  MisoString -> Attribute action
glyphRef_ = attr "glyphRef"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientTransform>
gradientTransform_ ::  MisoString -> Attribute action
gradientTransform_ = attr "gradientTransform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits>
gradientUnits_ ::  MisoString -> Attribute action
gradientUnits_ = attr "gradientUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/hanging>
hanging_ ::  MisoString -> Attribute action
hanging_ = attr "hanging"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/height>
height_ ::  MisoString -> Attribute action
height_ = attr "height"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horiz-adv-x>
horizAdvX_ ::  MisoString -> Attribute action
horizAdvX_ = attr "horiz-adv-x"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horiz-origin-x>
horizOriginX_ ::  MisoString -> Attribute action
horizOriginX_ = attr "horiz-origin-x"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horiz-origin-y>
horizOriginY_ ::  MisoString -> Attribute action
horizOriginY_ = attr "horiz-origin-y"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/id>
id_ ::  MisoString -> Attribute action
id_ = attr "id"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ideographic>
ideographic_ ::  MisoString -> Attribute action
ideographic_ = attr "ideographic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in>
in_' ::  MisoString -> Attribute action
in_' = attr "in"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in2>
in2_ ::  MisoString -> Attribute action
in2_ = attr "in2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/intercept>
intercept_ ::  MisoString -> Attribute action
intercept_ = attr "intercept"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k>
k_ ::  MisoString -> Attribute action
k_ = attr "k"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k1>
k1_ ::  MisoString -> Attribute action
k1_ = attr "k1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k2>
k2_ ::  MisoString -> Attribute action
k2_ = attr "k2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k3>
k3_ ::  MisoString -> Attribute action
k3_ = attr "k3"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k4>
k4_ ::  MisoString -> Attribute action
k4_ = attr "k4"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelMatrix>
kernelMatrix_ ::  MisoString -> Attribute action
kernelMatrix_ = attr "kernelMatrix"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelUnitLength>
kernelUnitLength_ ::  MisoString -> Attribute action
kernelUnitLength_ = attr "kernelUnitLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyPoints>
keyPoints_ ::  MisoString -> Attribute action
keyPoints_ = attr "keyPoints"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keySplines>
keySplines_ ::  MisoString -> Attribute action
keySplines_ = attr "keySplines"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyTimes>
keyTimes_ ::  MisoString -> Attribute action
keyTimes_ = attr "keyTimes"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lang>
lang_ ::  MisoString -> Attribute action
lang_ = attr "lang"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lengthAdjust>
lengthAdjust_ ::  MisoString -> Attribute action
lengthAdjust_ = attr "lengthAdjust"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/limitingConeAngle>
limitingConeAngle_ ::  MisoString -> Attribute action
limitingConeAngle_ = attr "limitingConeAngle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/local>
local_ ::  MisoString -> Attribute action
local_ = attr "local"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerHeight>
markerHeight_ ::  MisoString -> Attribute action
markerHeight_ = attr "markerHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits>
markerUnits_ ::  MisoString -> Attribute action
markerUnits_ = attr "markerUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerWidth>
markerWidth_ ::  MisoString -> Attribute action
markerWidth_ = attr "markerWidth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskContentUnits>
maskContentUnits_ ::  MisoString -> Attribute action
maskContentUnits_ = attr "maskContentUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskUnits>
maskUnits_ ::  MisoString -> Attribute action
maskUnits_ = attr "maskUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mathematical>
mathematical_ ::  MisoString -> Attribute action
mathematical_ = attr "mathematical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/max>
max_ ::  MisoString -> Attribute action
max_ = attr "max"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/media>
media_ ::  MisoString -> Attribute action
media_ = attr "media"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/method>
method_ ::  MisoString -> Attribute action
method_ = attr "method"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/min>
min_ ::  MisoString -> Attribute action
min_ = attr "min"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mode>
mode_ ::  MisoString -> Attribute action
mode_ = attr "mode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/name>
name_ ::  MisoString -> Attribute action
name_ = attr "name"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/numOctaves>
numOctaves_ ::  MisoString -> Attribute action
numOctaves_ = attr "numOctaves"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/offset>
offset_ ::  MisoString -> Attribute action
offset_ = attr "offset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/operator>
operator_ ::  MisoString -> Attribute action
operator_ = attr "operator"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/order>
order_ ::  MisoString -> Attribute action
order_ = attr "order"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orient>
orient_ ::  MisoString -> Attribute action
orient_ = attr "orient"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orientation>
orientation_ ::  MisoString -> Attribute action
orientation_ = attr "orientation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/origin>
origin_ ::  MisoString -> Attribute action
origin_ = attr "origin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overline-position>
overlinePosition_ ::  MisoString -> Attribute action
overlinePosition_ = attr "overline-position"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overline-thickness>
overlineThickness_ ::  MisoString -> Attribute action
overlineThickness_ = attr "overline-thickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/panose-1>
panose1_ ::  MisoString -> Attribute action
panose1_ = attr "panose-1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/path>
path_ ::  MisoString -> Attribute action
path_ = attr "path"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pathLength>
pathLength_ ::  MisoString -> Attribute action
pathLength_ = attr "pathLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternContentUnits>
patternContentUnits_ ::  MisoString -> Attribute action
patternContentUnits_ = attr "patternContentUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternTransform>
patternTransform_ ::  MisoString -> Attribute action
patternTransform_ = attr "patternTransform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternUnits>
patternUnits_ ::  MisoString -> Attribute action
patternUnits_ = attr "patternUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/point-order>
pointOrder_ ::  MisoString -> Attribute action
pointOrder_ = attr "point-order"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/points>
points_ ::  MisoString -> Attribute action
points_ = attr "points"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtX>
pointsAtX_ ::  MisoString -> Attribute action
pointsAtX_ = attr "pointsAtX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtY>
pointsAtY_ ::  MisoString -> Attribute action
pointsAtY_ = attr "pointsAtY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtZ>
pointsAtZ_ ::  MisoString -> Attribute action
pointsAtZ_ = attr "pointsAtZ"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAlpha>
preserveAlpha_ ::  MisoString -> Attribute action
preserveAlpha_ = attr "preserveAlpha"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio>
preserveAspectRatio_ ::  MisoString -> Attribute action
preserveAspectRatio_ = attr "preserveAspectRatio"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/primitiveUnits>
primitiveUnits_ ::  MisoString -> Attribute action
primitiveUnits_ = attr "primitiveUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/r>
r_ ::  MisoString -> Attribute action
r_ = attr "r"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/radius>
radius_ ::  MisoString -> Attribute action
radius_ = attr "radius"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refX>
refX_ ::  MisoString -> Attribute action
refX_ = attr "refX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refY>
refY_ ::  MisoString -> Attribute action
refY_ = attr "refY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rendering-intent>
renderingIntent_ ::  MisoString -> Attribute action
renderingIntent_ = attr "rendering-intent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatCount>
repeatCount_ ::  MisoString -> Attribute action
repeatCount_ = attr "repeatCount"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatDur>
repeatDur_ ::  MisoString -> Attribute action
repeatDur_ = attr "repeatDur"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/requiredExtensions>
requiredExtensions_ ::  MisoString -> Attribute action
requiredExtensions_ = attr "requiredExtensions"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/requiredFeatures>
requiredFeatures_ ::  MisoString -> Attribute action
requiredFeatures_ = attr "requiredFeatures"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/restart>
restart_ ::  MisoString -> Attribute action
restart_ = attr "restart"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/result>
result_ ::  MisoString -> Attribute action
result_ = attr "result"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rotate>
rotate_ ::  MisoString -> Attribute action
rotate_ = attr "rotate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rx>
rx_ ::  MisoString -> Attribute action
rx_ = attr "rx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ry>
ry_ ::  MisoString -> Attribute action
ry_ = attr "ry"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/scale>
scale_ ::  MisoString -> Attribute action
scale_ = attr "scale"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/seed>
seed_ ::  MisoString -> Attribute action
seed_ = attr "seed"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/slope>
slope_ ::  MisoString -> Attribute action
slope_ = attr "slope"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spacing>
spacing_ ::  MisoString -> Attribute action
spacing_ = attr "spacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularConstant>
specularConstant_ ::  MisoString -> Attribute action
specularConstant_ = attr "specularConstant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularExponent>
specularExponent_ ::  MisoString -> Attribute action
specularExponent_ = attr "specularExponent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/speed>
speed_ ::  MisoString -> Attribute action
speed_ = attr "speed"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spreadMethod>
spreadMethod_ ::  MisoString -> Attribute action
spreadMethod_ = attr "spreadMethod"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/startOffset>
startOffset_ ::  MisoString -> Attribute action
startOffset_ = attr "startOffset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stdDeviation>
stdDeviation_ ::  MisoString -> Attribute action
stdDeviation_ = attr "stdDeviation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stemh>
stemh_ ::  MisoString -> Attribute action
stemh_ = attr "stemh"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stemv>
stemv_ ::  MisoString -> Attribute action
stemv_ = attr "stemv"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stitchTiles>
stitchTiles_ ::  MisoString -> Attribute action
stitchTiles_ = attr "stitchTiles"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strikethrough-position>
strikethroughPosition_ ::  MisoString -> Attribute action
strikethroughPosition_ = attr "strikethrough-position"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strikethrough-thickness>
strikethroughThickness_ ::  MisoString -> Attribute action
strikethroughThickness_ = attr "strikethrough-thickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/string>
string_ ::  MisoString -> Attribute action
string_ = attr "string"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/style>
style_ ::  MisoString -> Attribute action
style_ = attr "style"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/surfaceScale>
surfaceScale_ ::  MisoString -> Attribute action
surfaceScale_ = attr "surfaceScale"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/systemLanguage>
systemLanguage_ ::  MisoString -> Attribute action
systemLanguage_ = attr "systemLanguage"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/tableValues>
tableValues_ ::  MisoString -> Attribute action
tableValues_ = attr "tableValues"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/target>
target_ ::  MisoString -> Attribute action
target_ = attr "target"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetX>
targetX_ ::  MisoString -> Attribute action
targetX_ = attr "targetX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetY>
targetY_ ::  MisoString -> Attribute action
targetY_ = attr "targetY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textLength>
textLength_ ::  MisoString -> Attribute action
textLength_ = attr "textLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/title>
title_ ::  MisoString -> Attribute action
title_ = attr "title"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to>
to_ ::  MisoString -> Attribute action
to_ = attr "to"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
transform_ ::  MisoString -> Attribute action
transform_ = attr "transform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/type>
type_' ::  MisoString -> Attribute action
type_' = attr "type"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/u1>
u1_ ::  MisoString -> Attribute action
u1_ = attr "u1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/u2>
u2_ ::  MisoString -> Attribute action
u2_ = attr "u2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/underline-position>
underlinePosition_ ::  MisoString -> Attribute action
underlinePosition_ = attr "underline-position"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/underline-thickness>
underlineThickness_ ::  MisoString -> Attribute action
underlineThickness_ = attr "underline-thickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicode>
unicode_ ::  MisoString -> Attribute action
unicode_ = attr "unicode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicode-range>
unicodeRange_ ::  MisoString -> Attribute action
unicodeRange_ = attr "unicode-range"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/units-per-em>
unitsPerEm_ ::  MisoString -> Attribute action
unitsPerEm_ = attr "units-per-em"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/v-alphabetic>
vAlphabetic_ ::  MisoString -> Attribute action
vAlphabetic_ = attr "v-alphabetic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/v-hanging>
vHanging_ ::  MisoString -> Attribute action
vHanging_ = attr "v-hanging"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/v-ideographic>
vIdeographic_ ::  MisoString -> Attribute action
vIdeographic_ = attr "v-ideographic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/v-mathematical>
vMathematical_ ::  MisoString -> Attribute action
vMathematical_ = attr "v-mathematical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values>
values_ ::  MisoString -> Attribute action
values_ = attr "values"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/version>
version_ ::  MisoString -> Attribute action
version_ = attr "version"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vert-adv-y>
vertAdvY_ ::  MisoString -> Attribute action
vertAdvY_ = attr "vert-adv-y"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vert-origin-x>
vertOriginX_ ::  MisoString -> Attribute action
vertOriginX_ = attr "vert-origin-x"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vert-origin-y>
vertOriginY_ ::  MisoString -> Attribute action
vertOriginY_ = attr "vert-origin-y"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewBox>
viewBox_ ::  MisoString -> Attribute action
viewBox_ = attr "viewBox"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewTarget>
viewTarget_ ::  MisoString -> Attribute action
viewTarget_ = attr "viewTarget"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/width>
width_ ::  MisoString -> Attribute action
width_ = attr "width"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/widths>
widths_ ::  MisoString -> Attribute action
widths_ = attr "widths"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x>
x_ ::  MisoString -> Attribute action
x_ = attr "x"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x-height>
xHeight_ ::  MisoString -> Attribute action
xHeight_ = attr "x-height"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x1>
x1_ ::  MisoString -> Attribute action
x1_ = attr "x1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x2>
x2_ ::  MisoString -> Attribute action
x2_ = attr "x2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xChannelSelector>
xChannelSelector_ ::  MisoString -> Attribute action
xChannelSelector_ = attr "x-channel-selector"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkActuate>
xlinkActuate_ ::  MisoString -> Attribute action
xlinkActuate_ = attr "xlinkActuate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkArcrole>
xlinkArcrole_ ::  MisoString -> Attribute action
xlinkArcrole_ = attr "xlinkArcrole"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkHref>
xlinkHref_ ::  MisoString -> Attribute action
xlinkHref_ = attr "xlinkHref"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkRole>
xlinkRole_ ::  MisoString -> Attribute action
xlinkRole_ = attr "xlinkRole"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkShow>
xlinkShow_ ::  MisoString -> Attribute action
xlinkShow_ = attr "xlinkShow"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkTitle>
xlinkTitle_ ::  MisoString -> Attribute action
xlinkTitle_ = attr "xlinkTitle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkType>
xlinkType_ ::  MisoString -> Attribute action
xlinkType_ = attr "xlinkType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlBase>
xmlBase_ ::  MisoString -> Attribute action
xmlBase_ = attr "xmlBase"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlLang>
xmlLang_ ::  MisoString -> Attribute action
xmlLang_ = attr "xmlLang"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlSpace>
xmlSpace_ ::  MisoString -> Attribute action
xmlSpace_ = attr "xmlSpace"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y>
y_ ::  MisoString -> Attribute action
y_ = attr "y"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y1>
y1_ ::  MisoString -> Attribute action
y1_ = attr "y1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y2>
y2_ ::  MisoString -> Attribute action
y2_ = attr "y2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/yChannelSelector>
yChannelSelector_ ::  MisoString -> Attribute action
yChannelSelector_ = attr "yChannelSelector"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/z>
z_ ::  MisoString -> Attribute action
z_ = attr "z"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/zoomAndPan>
zoomAndPan_ ::  MisoString -> Attribute action
zoomAndPan_ = attr "zoomAndPan"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline>
alignmentBaseline_ ::  MisoString -> Attribute action
alignmentBaseline_ = attr "alignment-baseline"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseline-shift>
baselineShift_ ::  MisoString -> Attribute action
baselineShift_ = attr "baseline-shift"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip-path>
clipPath_ ::  MisoString -> Attribute action
clipPath_ = attr "clip-path"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip-rule>
clipRule_ ::  MisoString -> Attribute action
clipRule_ = attr "clip-rule"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip>
clip_ ::  MisoString -> Attribute action
clip_ = attr "clip"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color-interpolation-filters>
colorInterpolationFilters_ ::  MisoString -> Attribute action
colorInterpolationFilters_ = attr "color-interpolation-filters"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color-interpolation>
colorInterpolation_ ::  MisoString -> Attribute action
colorInterpolation_ = attr "color-interpolation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color-profile>
colorProfile_ ::  MisoString -> Attribute action
colorProfile_ = attr "color-profile"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color-rendering>
colorRendering_ ::  MisoString -> Attribute action
colorRendering_ = attr "color-rendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color>
color_ ::  MisoString -> Attribute action
color_ = attr "color"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cursor>
cursor_ ::  MisoString -> Attribute action
cursor_ = attr "cursor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/direction>
direction_ ::  MisoString -> Attribute action
direction_ = attr "direction"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/display>
display_ ::  MisoString -> Attribute action
display_ = attr "display"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dominant-baseline>
dominantBaseline_ ::  MisoString -> Attribute action
dominantBaseline_ = attr "dominant-baseline"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/enable-background>
enableBackground_ ::  MisoString -> Attribute action
enableBackground_ = attr "enable-background"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-opacity>
fillOpacity_ ::  MisoString -> Attribute action
fillOpacity_ = attr "fill-opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-rule>
fillRule_ ::  MisoString -> Attribute action
fillRule_ = attr "fill-rule"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
fill_ ::  MisoString -> Attribute action
fill_ = attr "fill"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filter>
filter_ ::  MisoString -> Attribute action
filter_ = attr "filter"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/flood-color>
floodColor_ ::  MisoString -> Attribute action
floodColor_ = attr "flood-color"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/flood-opacity>
floodOpacity_ ::  MisoString -> Attribute action
floodOpacity_ = attr "flood-opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-family>
fontFamily_ ::  MisoString -> Attribute action
fontFamily_ = attr "font-family"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-size-adjust>
fontSizeAdjust_ ::  MisoString -> Attribute action
fontSizeAdjust_ = attr "font-size-adjust"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-size>
fontSize_ ::  MisoString -> Attribute action
fontSize_ = attr "font-size"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-stretch>
fontStretch_ ::  MisoString -> Attribute action
fontStretch_ = attr "font-stretch"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-style>
fontStyle_ ::  MisoString -> Attribute action
fontStyle_ = attr "font-style"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-variant>
fontVariant_ ::  MisoString -> Attribute action
fontVariant_ = attr "font-variant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-weight>
fontWeight_ ::  MisoString -> Attribute action
fontWeight_ = attr "font-weight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyph-orientation-horizontal>
glyphOrientationHorizontal_ ::  MisoString -> Attribute action
glyphOrientationHorizontal_ = attr "glyph-orientation-horizontal"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyph-orientation-vertical>
glyphOrientationVertical_ ::  MisoString -> Attribute action
glyphOrientationVertical_ = attr "glyph-orientation-vertical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/image-rendering>
imageRendering_ ::  MisoString -> Attribute action
imageRendering_ = attr "image-rendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kerning>
kerning_ ::  MisoString -> Attribute action
kerning_ = attr "kerning"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/letter-spacing>
letterSpacing_ ::  MisoString -> Attribute action
letterSpacing_ = attr "letter-spacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lighting-color>
lightingColor_ ::  MisoString -> Attribute action
lightingColor_ = attr "lighting-color"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/marker-end>
markerEnd_ ::  MisoString -> Attribute action
markerEnd_ = attr "marker-end"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/marker-mid>
markerMid_ ::  MisoString -> Attribute action
markerMid_ = attr "marker-mid"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/marker-start>
markerStart_ ::  MisoString -> Attribute action
markerStart_ = attr "marker-start"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mask>
mask_ ::  MisoString -> Attribute action
mask_ = attr "mask"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/opacity>
opacity_ ::  MisoString -> Attribute action
opacity_ = attr "opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overflow>
overflow_ ::  MisoString -> Attribute action
overflow_ = attr "overflow"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointer-events>
pointerEvents_ ::  MisoString -> Attribute action
pointerEvents_ = attr "pointer-events"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/shape-rendering>
shapeRendering_ ::  MisoString -> Attribute action
shapeRendering_ = attr "shape-rendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stop-color>
stopColor_ ::  MisoString -> Attribute action
stopColor_ = attr "stop-color"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stop-opacity>
stopOpacity_ ::  MisoString -> Attribute action
stopOpacity_ = attr "stop-opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-dasharray>
strokeDasharray_ ::  MisoString -> Attribute action
strokeDasharray_ = attr "stroke-dasharray"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-dashoffset>
strokeDashoffset_ ::  MisoString -> Attribute action
strokeDashoffset_ = attr "stroke-dashoffset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-linecap>
strokeLinecap_ ::  MisoString -> Attribute action
strokeLinecap_ = attr "stroke-linecap"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-linejoin>
strokeLinejoin_ ::  MisoString -> Attribute action
strokeLinejoin_ = attr "stroke-linejoin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-miterlimit>
strokeMiterlimit_ ::  MisoString -> Attribute action
strokeMiterlimit_ = attr "stroke-miterlimit"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-opacity>
strokeOpacity_ ::  MisoString -> Attribute action
strokeOpacity_ = attr "stroke-opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width>
strokeWidth_ ::  MisoString -> Attribute action
strokeWidth_ = attr "stroke-width"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke>
stroke_ ::  MisoString -> Attribute action
stroke_ = attr "stroke"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-anchor>
textAnchor_ ::  MisoString -> Attribute action
textAnchor_ = attr "text-anchor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-decoration>
textDecoration_ ::  MisoString -> Attribute action
textDecoration_ = attr "text-decoration"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/text-rendering>
textRendering_ ::  MisoString -> Attribute action
textRendering_ = attr "text-rendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicode-bidi>
unicodeBidi_ ::  MisoString -> Attribute action
unicodeBidi_ = attr "unicode-bidi"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/visibility>
visibility_ ::  MisoString -> Attribute action
visibility_ = attr "visibility"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/word-spacing>
wordSpacing_ ::  MisoString -> Attribute action
wordSpacing_ = attr "word-spacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/writing-mode>
writingMode_ ::  MisoString -> Attribute action
writingMode_ = attr "writing-mode"
