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

import Miso.Html.Internal ( MisoString, Attribute )
import Miso.Html.Property ( textProp )

attr :: MisoString -> MisoString -> Attribute model
attr = textProp

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accent-height>
accentHeight_ ::  MisoString -> Attribute model
accentHeight_ = attr "accentHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accelerate>
accelerate_ ::  MisoString -> Attribute model
accelerate_ = attr "accelerate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/accumulate>
accumulate_ ::  MisoString -> Attribute model
accumulate_ = attr "accumulate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/additive>
additive_ ::  MisoString -> Attribute model
additive_ = attr "additive"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alphabetic>
alphabetic_ ::  MisoString -> Attribute model
alphabetic_ = attr "alphabetic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/allowReorder>
allowReorder_ ::  MisoString -> Attribute model
allowReorder_ = attr "allowReorder"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/amplitude>
amplitude_ ::  MisoString -> Attribute model
amplitude_ = attr "amplitude"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/arabicForm>
arabicForm_ ::  MisoString -> Attribute model
arabicForm_ = attr "arabicForm"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ascent>
ascent_ ::  MisoString -> Attribute model
ascent_ = attr "ascent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeName>
attributeName_ ::  MisoString -> Attribute model
attributeName_ = attr "attributeName"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/attributeType>
attributeType_ ::  MisoString -> Attribute model
attributeType_ = attr "attributeType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/autoReverse>
autoReverse_ ::  MisoString -> Attribute model
autoReverse_ = attr "autoReverse"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/azimuth>
azimuth_ ::  MisoString -> Attribute model
azimuth_ = attr "azimuth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseFrequency>
baseFrequency_ ::  MisoString -> Attribute model
baseFrequency_ = attr "baseFrequency"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baseProfile>
baseProfile_ ::  MisoString -> Attribute model
baseProfile_ = attr "baseProfile"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bbox>
bbox_ ::  MisoString -> Attribute model
bbox_ = attr "bbox"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin>
begin_ ::  MisoString -> Attribute model
begin_ = attr "begin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/bias>
bias_ ::  MisoString -> Attribute model
bias_ = attr "bias"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/by>
by_ ::  MisoString -> Attribute model
by_ = attr "by"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/calcMode>
calcMode_ ::  MisoString -> Attribute model
calcMode_ = attr "calcMode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/capHeight>
capHeight_ ::  MisoString -> Attribute model
capHeight_ = attr "capHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/class>
class_' ::  MisoString -> Attribute model
class_' = attr "class"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipPathUnits>
clipPathUnits_ ::  MisoString -> Attribute model
clipPathUnits_ = attr "clipPathUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/contentScriptType>
contentScriptType_ ::  MisoString -> Attribute model
contentScriptType_ = attr "contentScriptType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/contentStyleType>
contentStyleType_ ::  MisoString -> Attribute model
contentStyleType_ = attr "contentStyleType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cx>
cx_ ::  MisoString -> Attribute model
cx_ = attr "cx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cy>
cy_ ::  MisoString -> Attribute model
cy_ = attr "cy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d>
d_ ::  MisoString -> Attribute model
d_ = attr "d"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/decelerate>
decelerate_ ::  MisoString -> Attribute model
decelerate_ = attr "decelerate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/descent>
descent_ ::  MisoString -> Attribute model
descent_ = attr "descent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/diffuseConstant>
diffuseConstant_ ::  MisoString -> Attribute model
diffuseConstant_ = attr "diffuseConstant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/divisor>
divisor_ ::  MisoString -> Attribute model
divisor_ = attr "divisor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur>
dur_ ::  MisoString -> Attribute model
dur_ = attr "dur"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dx>
dx_ ::  MisoString -> Attribute model
dx_ = attr "dx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dy>
dy_ ::  MisoString -> Attribute model
dy_ = attr "dy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/edgeMode>
edgeMode_ ::  MisoString -> Attribute model
edgeMode_ = attr "edgeMode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/elevation>
elevation_ ::  MisoString -> Attribute model
elevation_ = attr "elevation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/end>
end_ ::  MisoString -> Attribute model
end_ = attr "end"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/exponent>
exponent_ ::  MisoString -> Attribute model
exponent_ = attr "exponent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/externalResourcesRequired>
externalResourcesRequired_ ::  MisoString -> Attribute model
externalResourcesRequired_ = attr "externalResourcesRequired"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterRes>
filterRes_ ::  MisoString -> Attribute model
filterRes_ = attr "filterRes"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filterUnits>
filterUnits_ ::  MisoString -> Attribute model
filterUnits_ = attr "filterUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/format>
format_ ::  MisoString -> Attribute model
format_ = attr "format"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from>
from_ ::  MisoString -> Attribute model
from_ = attr "from"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fx>
fx_ ::  MisoString -> Attribute model
fx_ = attr "fx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fy>
fy_ ::  MisoString -> Attribute model
fy_ = attr "fy"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/g1>
g1_ ::  MisoString -> Attribute model
g1_ = attr "g1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/g2>
g2_ ::  MisoString -> Attribute model
g2_ = attr "g2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphName>
glyphName_ ::  MisoString -> Attribute model
glyphName_ = attr "glyphName"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphRef>
glyphRef_ ::  MisoString -> Attribute model
glyphRef_ = attr "glyphRef"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientTransform>
gradientTransform_ ::  MisoString -> Attribute model
gradientTransform_ = attr "gradientTransform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits>
gradientUnits_ ::  MisoString -> Attribute model
gradientUnits_ = attr "gradientUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/hanging>
hanging_ ::  MisoString -> Attribute model
hanging_ = attr "hanging"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/height>
height_ ::  MisoString -> Attribute model
height_ = attr "height"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horizAdvX>
horizAdvX_ ::  MisoString -> Attribute model
horizAdvX_ = attr "horizAdvX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horizOriginX>
horizOriginX_ ::  MisoString -> Attribute model
horizOriginX_ = attr "horizOriginX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/horizOriginY>
horizOriginY_ ::  MisoString -> Attribute model
horizOriginY_ = attr "horizOriginY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/id>
id_ ::  MisoString -> Attribute model
id_ = attr "id"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ideographic>
ideographic_ ::  MisoString -> Attribute model
ideographic_ = attr "ideographic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in>
in_' ::  MisoString -> Attribute model
in_' = attr "in"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/in2>
in2_ ::  MisoString -> Attribute model
in2_ = attr "in2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/intercept>
intercept_ ::  MisoString -> Attribute model
intercept_ = attr "intercept"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k>
k_ ::  MisoString -> Attribute model
k_ = attr "k"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k1>
k1_ ::  MisoString -> Attribute model
k1_ = attr "k1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k2>
k2_ ::  MisoString -> Attribute model
k2_ = attr "k2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k3>
k3_ ::  MisoString -> Attribute model
k3_ = attr "k3"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/k4>
k4_ ::  MisoString -> Attribute model
k4_ = attr "k4"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelMatrix>
kernelMatrix_ ::  MisoString -> Attribute model
kernelMatrix_ = attr "kernelMatrix"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kernelUnitLength>
kernelUnitLength_ ::  MisoString -> Attribute model
kernelUnitLength_ = attr "kernelUnitLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyPoints>
keyPoints_ ::  MisoString -> Attribute model
keyPoints_ = attr "keyPoints"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keySplines>
keySplines_ ::  MisoString -> Attribute model
keySplines_ = attr "keySplines"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/keyTimes>
keyTimes_ ::  MisoString -> Attribute model
keyTimes_ = attr "keyTimes"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lang>
lang_ ::  MisoString -> Attribute model
lang_ = attr "lang"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lengthAdjust>
lengthAdjust_ ::  MisoString -> Attribute model
lengthAdjust_ = attr "lengthAdjust"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/limitingConeAngle>
limitingConeAngle_ ::  MisoString -> Attribute model
limitingConeAngle_ = attr "limitingConeAngle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/local>
local_ ::  MisoString -> Attribute model
local_ = attr "local"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerHeight>
markerHeight_ ::  MisoString -> Attribute model
markerHeight_ = attr "markerHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerUnits>
markerUnits_ ::  MisoString -> Attribute model
markerUnits_ = attr "markerUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerWidth>
markerWidth_ ::  MisoString -> Attribute model
markerWidth_ = attr "markerWidth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskContentUnits>
maskContentUnits_ ::  MisoString -> Attribute model
maskContentUnits_ = attr "maskContentUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/maskUnits>
maskUnits_ ::  MisoString -> Attribute model
maskUnits_ = attr "maskUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mathematical>
mathematical_ ::  MisoString -> Attribute model
mathematical_ = attr "mathematical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/max>
max_ ::  MisoString -> Attribute model
max_ = attr "max"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/media>
media_ ::  MisoString -> Attribute model
media_ = attr "media"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/method>
method_ ::  MisoString -> Attribute model
method_ = attr "method"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/min>
min_ ::  MisoString -> Attribute model
min_ = attr "min"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mode>
mode_ ::  MisoString -> Attribute model
mode_ = attr "mode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/name>
name_ ::  MisoString -> Attribute model
name_ = attr "name"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/numOctaves>
numOctaves_ ::  MisoString -> Attribute model
numOctaves_ = attr "numOctaves"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/offset>
offset_ ::  MisoString -> Attribute model
offset_ = attr "offset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/operator>
operator_ ::  MisoString -> Attribute model
operator_ = attr "operator"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/order>
order_ ::  MisoString -> Attribute model
order_ = attr "order"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orient>
orient_ ::  MisoString -> Attribute model
orient_ = attr "orient"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/orientation>
orientation_ ::  MisoString -> Attribute model
orientation_ = attr "orientation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/origin>
origin_ ::  MisoString -> Attribute model
origin_ = attr "origin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overlinePosition>
overlinePosition_ ::  MisoString -> Attribute model
overlinePosition_ = attr "overlinePosition"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overlineThickness>
overlineThickness_ ::  MisoString -> Attribute model
overlineThickness_ = attr "overlineThickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/panose1>
panose1_ ::  MisoString -> Attribute model
panose1_ = attr "panose1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/path>
path_ ::  MisoString -> Attribute model
path_ = attr "path"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pathLength>
pathLength_ ::  MisoString -> Attribute model
pathLength_ = attr "pathLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternContentUnits>
patternContentUnits_ ::  MisoString -> Attribute model
patternContentUnits_ = attr "patternContentUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternTransform>
patternTransform_ ::  MisoString -> Attribute model
patternTransform_ = attr "patternTransform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/patternUnits>
patternUnits_ ::  MisoString -> Attribute model
patternUnits_ = attr "patternUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointOrder>
pointOrder_ ::  MisoString -> Attribute model
pointOrder_ = attr "pointOrder"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/points>
points_ ::  MisoString -> Attribute model
points_ = attr "points"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtX>
pointsAtX_ ::  MisoString -> Attribute model
pointsAtX_ = attr "pointsAtX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtY>
pointsAtY_ ::  MisoString -> Attribute model
pointsAtY_ = attr "pointsAtY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointsAtZ>
pointsAtZ_ ::  MisoString -> Attribute model
pointsAtZ_ = attr "pointsAtZ"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAlpha>
preserveAlpha_ ::  MisoString -> Attribute model
preserveAlpha_ = attr "preserveAlpha"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio>
preserveAspectRatio_ ::  MisoString -> Attribute model
preserveAspectRatio_ = attr "preserveAspectRatio"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/primitiveUnits>
primitiveUnits_ ::  MisoString -> Attribute model
primitiveUnits_ = attr "primitiveUnits"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/r>
r_ ::  MisoString -> Attribute model
r_ = attr "r"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/radius>
radius_ ::  MisoString -> Attribute model
radius_ = attr "radius"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refX>
refX_ ::  MisoString -> Attribute model
refX_ = attr "refX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/refY>
refY_ ::  MisoString -> Attribute model
refY_ = attr "refY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/renderingIntent>
renderingIntent_ ::  MisoString -> Attribute model
renderingIntent_ = attr "renderingIntent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatCount>
repeatCount_ ::  MisoString -> Attribute model
repeatCount_ = attr "repeatCount"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatDur>
repeatDur_ ::  MisoString -> Attribute model
repeatDur_ = attr "repeatDur"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/requiredExtensions>
requiredExtensions_ ::  MisoString -> Attribute model
requiredExtensions_ = attr "requiredExtensions"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/requiredFeatures>
requiredFeatures_ ::  MisoString -> Attribute model
requiredFeatures_ = attr "requiredFeatures"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/restart>
restart_ ::  MisoString -> Attribute model
restart_ = attr "restart"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/result>
result_ ::  MisoString -> Attribute model
result_ = attr "result"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rotate>
rotate_ ::  MisoString -> Attribute model
rotate_ = attr "rotate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/rx>
rx_ ::  MisoString -> Attribute model
rx_ = attr "rx"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/ry>
ry_ ::  MisoString -> Attribute model
ry_ = attr "ry"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/scale>
scale_ ::  MisoString -> Attribute model
scale_ = attr "scale"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/seed>
seed_ ::  MisoString -> Attribute model
seed_ = attr "seed"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/slope>
slope_ ::  MisoString -> Attribute model
slope_ = attr "slope"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spacing>
spacing_ ::  MisoString -> Attribute model
spacing_ = attr "spacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularConstant>
specularConstant_ ::  MisoString -> Attribute model
specularConstant_ = attr "specularConstant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/specularExponent>
specularExponent_ ::  MisoString -> Attribute model
specularExponent_ = attr "specularExponent"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/speed>
speed_ ::  MisoString -> Attribute model
speed_ = attr "speed"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/spreadMethod>
spreadMethod_ ::  MisoString -> Attribute model
spreadMethod_ = attr "spreadMethod"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/startOffset>
startOffset_ ::  MisoString -> Attribute model
startOffset_ = attr "startOffset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stdDeviation>
stdDeviation_ ::  MisoString -> Attribute model
stdDeviation_ = attr "stdDeviation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stemh>
stemh_ ::  MisoString -> Attribute model
stemh_ = attr "stemh"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stemv>
stemv_ ::  MisoString -> Attribute model
stemv_ = attr "stemv"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stitchTiles>
stitchTiles_ ::  MisoString -> Attribute model
stitchTiles_ = attr "stitchTiles"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strikethroughPosition>
strikethroughPosition_ ::  MisoString -> Attribute model
strikethroughPosition_ = attr "strikethroughPosition"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strikethroughThickness>
strikethroughThickness_ ::  MisoString -> Attribute model
strikethroughThickness_ = attr "strikethroughThickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/string>
string_ ::  MisoString -> Attribute model
string_ = attr "string"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/style>
style_ ::  MisoString -> Attribute model
style_ = attr "style"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/surfaceScale>
surfaceScale_ ::  MisoString -> Attribute model
surfaceScale_ = attr "surfaceScale"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/systemLanguage>
systemLanguage_ ::  MisoString -> Attribute model
systemLanguage_ = attr "systemLanguage"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/tableValues>
tableValues_ ::  MisoString -> Attribute model
tableValues_ = attr "tableValues"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/target>
target_ ::  MisoString -> Attribute model
target_ = attr "target"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetX>
targetX_ ::  MisoString -> Attribute model
targetX_ = attr "targetX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/targetY>
targetY_ ::  MisoString -> Attribute model
targetY_ = attr "targetY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textLength>
textLength_ ::  MisoString -> Attribute model
textLength_ = attr "textLength"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/title>
title_ ::  MisoString -> Attribute model
title_ = attr "title"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to>
to_ ::  MisoString -> Attribute model
to_ = attr "to"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
transform_ ::  MisoString -> Attribute model
transform_ = attr "transform"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/type>
type_' ::  MisoString -> Attribute model
type_' = attr "type"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/u1>
u1_ ::  MisoString -> Attribute model
u1_ = attr "u1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/u2>
u2_ ::  MisoString -> Attribute model
u2_ = attr "u2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/underlinePosition>
underlinePosition_ ::  MisoString -> Attribute model
underlinePosition_ = attr "underlinePosition"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/underlineThickness>
underlineThickness_ ::  MisoString -> Attribute model
underlineThickness_ = attr "underlineThickness"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicode>
unicode_ ::  MisoString -> Attribute model
unicode_ = attr "unicode"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicodeRange>
unicodeRange_ ::  MisoString -> Attribute model
unicodeRange_ = attr "unicodeRange"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unitsPerEm>
unitsPerEm_ ::  MisoString -> Attribute model
unitsPerEm_ = attr "unitsPerEm"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vAlphabetic>
vAlphabetic_ ::  MisoString -> Attribute model
vAlphabetic_ = attr "vAlphabetic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vHanging>
vHanging_ ::  MisoString -> Attribute model
vHanging_ = attr "vHanging"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vIdeographic>
vIdeographic_ ::  MisoString -> Attribute model
vIdeographic_ = attr "vIdeographic"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vMathematical>
vMathematical_ ::  MisoString -> Attribute model
vMathematical_ = attr "vMathematical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/values>
values_ ::  MisoString -> Attribute model
values_ = attr "values"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/version>
version_ ::  MisoString -> Attribute model
version_ = attr "version"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vertAdvY>
vertAdvY_ ::  MisoString -> Attribute model
vertAdvY_ = attr "vertAdvY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vertOriginX>
vertOriginX_ ::  MisoString -> Attribute model
vertOriginX_ = attr "vertOriginX"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/vertOriginY>
vertOriginY_ ::  MisoString -> Attribute model
vertOriginY_ = attr "vertOriginY"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewBox>
viewBox_ ::  MisoString -> Attribute model
viewBox_ = attr "viewBox"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/viewTarget>
viewTarget_ ::  MisoString -> Attribute model
viewTarget_ = attr "viewTarget"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/width>
width_ ::  MisoString -> Attribute model
width_ = attr "width"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/widths>
widths_ ::  MisoString -> Attribute model
widths_ = attr "widths"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x>
x_ ::  MisoString -> Attribute model
x_ = attr "x"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xHeight>
xHeight_ ::  MisoString -> Attribute model
xHeight_ = attr "xHeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x1>
x1_ ::  MisoString -> Attribute model
x1_ = attr "x1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/x2>
x2_ ::  MisoString -> Attribute model
x2_ = attr "x2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xChannelSelector>
xChannelSelector_ ::  MisoString -> Attribute model
xChannelSelector_ = attr "xChannelSelector"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkActuate>
xlinkActuate_ ::  MisoString -> Attribute model
xlinkActuate_ = attr "xlinkActuate"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkArcrole>
xlinkArcrole_ ::  MisoString -> Attribute model
xlinkArcrole_ = attr "xlinkArcrole"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkHref>
xlinkHref_ ::  MisoString -> Attribute model
xlinkHref_ = attr "xlinkHref"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkRole>
xlinkRole_ ::  MisoString -> Attribute model
xlinkRole_ = attr "xlinkRole"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkShow>
xlinkShow_ ::  MisoString -> Attribute model
xlinkShow_ = attr "xlinkShow"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkTitle>
xlinkTitle_ ::  MisoString -> Attribute model
xlinkTitle_ = attr "xlinkTitle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xlinkType>
xlinkType_ ::  MisoString -> Attribute model
xlinkType_ = attr "xlinkType"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlBase>
xmlBase_ ::  MisoString -> Attribute model
xmlBase_ = attr "xmlBase"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlLang>
xmlLang_ ::  MisoString -> Attribute model
xmlLang_ = attr "xmlLang"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/xmlSpace>
xmlSpace_ ::  MisoString -> Attribute model
xmlSpace_ = attr "xmlSpace"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y>
y_ ::  MisoString -> Attribute model
y_ = attr "y"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y1>
y1_ ::  MisoString -> Attribute model
y1_ = attr "y1"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/y2>
y2_ ::  MisoString -> Attribute model
y2_ = attr "y2"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/yChannelSelector>
yChannelSelector_ ::  MisoString -> Attribute model
yChannelSelector_ = attr "yChannelSelector"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/z>
z_ ::  MisoString -> Attribute model
z_ = attr "z"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/zoomAndPan>
zoomAndPan_ ::  MisoString -> Attribute model
zoomAndPan_ = attr "zoomAndPan"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignmentBaseline>
alignmentBaseline_ ::  MisoString -> Attribute model
alignmentBaseline_ = attr "alignmentBaseline"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/baselineShift>
baselineShift_ ::  MisoString -> Attribute model
baselineShift_ = attr "baselineShift"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipPath>
clipPath_ ::  MisoString -> Attribute model
clipPath_ = attr "clipPath"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clipRule>
clipRule_ ::  MisoString -> Attribute model
clipRule_ = attr "clipRule"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip>
clip_ ::  MisoString -> Attribute model
clip_ = attr "clip"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorInterpolationFilters>
colorInterpolationFilters_ ::  MisoString -> Attribute model
colorInterpolationFilters_ = attr "colorInterpolationFilters"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorInterpolation>
colorInterpolation_ ::  MisoString -> Attribute model
colorInterpolation_ = attr "colorInterpolation"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorProfile>
colorProfile_ ::  MisoString -> Attribute model
colorProfile_ = attr "colorProfile"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/colorRendering>
colorRendering_ ::  MisoString -> Attribute model
colorRendering_ = attr "colorRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color>
color_ ::  MisoString -> Attribute model
color_ = attr "color"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/cursor>
cursor_ ::  MisoString -> Attribute model
cursor_ = attr "cursor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/direction>
direction_ ::  MisoString -> Attribute model
direction_ = attr "direction"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/display>
display_ ::  MisoString -> Attribute model
display_ = attr "display"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dominantBaseline>
dominantBaseline_ ::  MisoString -> Attribute model
dominantBaseline_ = attr "dominantBaseline"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/enableBackground>
enableBackground_ ::  MisoString -> Attribute model
enableBackground_ = attr "enableBackground"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fillOpacity>
fillOpacity_ ::  MisoString -> Attribute model
fillOpacity_ = attr "fillOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fillRule>
fillRule_ ::  MisoString -> Attribute model
fillRule_ = attr "fillRule"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
fill_ ::  MisoString -> Attribute model
fill_ = attr "fill"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/filter>
filter_ ::  MisoString -> Attribute model
filter_ = attr "filter"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/floodColor>
floodColor_ ::  MisoString -> Attribute model
floodColor_ = attr "floodColor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/floodOpacity>
floodOpacity_ ::  MisoString -> Attribute model
floodOpacity_ = attr "floodOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontFamily>
fontFamily_ ::  MisoString -> Attribute model
fontFamily_ = attr "fontFamily"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontSizeAdjust>
fontSizeAdjust_ ::  MisoString -> Attribute model
fontSizeAdjust_ = attr "fontSizeAdjust"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontSize>
fontSize_ ::  MisoString -> Attribute model
fontSize_ = attr "fontSize"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontStretch>
fontStretch_ ::  MisoString -> Attribute model
fontStretch_ = attr "fontStretch"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontStyle>
fontStyle_ ::  MisoString -> Attribute model
fontStyle_ = attr "fontStyle"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontVariant>
fontVariant_ ::  MisoString -> Attribute model
fontVariant_ = attr "fontVariant"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fontWeight>
fontWeight_ ::  MisoString -> Attribute model
fontWeight_ = attr "fontWeight"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphOrientationHorizontal>
glyphOrientationHorizontal_ ::  MisoString -> Attribute model
glyphOrientationHorizontal_ = attr "glyphOrientationHorizontal"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/glyphOrientationVertical>
glyphOrientationVertical_ ::  MisoString -> Attribute model
glyphOrientationVertical_ = attr "glyphOrientationVertical"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/imageRendering>
imageRendering_ ::  MisoString -> Attribute model
imageRendering_ = attr "imageRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/kerning>
kerning_ ::  MisoString -> Attribute model
kerning_ = attr "kerning"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/letterSpacing>
letterSpacing_ ::  MisoString -> Attribute model
letterSpacing_ = attr "letterSpacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/lightingColor>
lightingColor_ ::  MisoString -> Attribute model
lightingColor_ = attr "lightingColor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerEnd>
markerEnd_ ::  MisoString -> Attribute model
markerEnd_ = attr "markerEnd"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerMid>
markerMid_ ::  MisoString -> Attribute model
markerMid_ = attr "markerMid"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/markerStart>
markerStart_ ::  MisoString -> Attribute model
markerStart_ = attr "markerStart"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/mask>
mask_ ::  MisoString -> Attribute model
mask_ = attr "mask"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/opacity>
opacity_ ::  MisoString -> Attribute model
opacity_ = attr "opacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/overflow>
overflow_ ::  MisoString -> Attribute model
overflow_ = attr "overflow"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/pointerEvents>
pointerEvents_ ::  MisoString -> Attribute model
pointerEvents_ = attr "pointerEvents"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/shapeRendering>
shapeRendering_ ::  MisoString -> Attribute model
shapeRendering_ = attr "shapeRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stopColor>
stopColor_ ::  MisoString -> Attribute model
stopColor_ = attr "stopColor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stopOpacity>
stopOpacity_ ::  MisoString -> Attribute model
stopOpacity_ = attr "stopOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeDasharray>
strokeDasharray_ ::  MisoString -> Attribute model
strokeDasharray_ = attr "strokeDasharray"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeDashoffset>
strokeDashoffset_ ::  MisoString -> Attribute model
strokeDashoffset_ = attr "strokeDashoffset"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeLinecap>
strokeLinecap_ ::  MisoString -> Attribute model
strokeLinecap_ = attr "strokeLinecap"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeLinejoin>
strokeLinejoin_ ::  MisoString -> Attribute model
strokeLinejoin_ = attr "strokeLinejoin"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeMiterlimit>
strokeMiterlimit_ ::  MisoString -> Attribute model
strokeMiterlimit_ = attr "strokeMiterlimit"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeOpacity>
strokeOpacity_ ::  MisoString -> Attribute model
strokeOpacity_ = attr "strokeOpacity"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/strokeWidth>
strokeWidth_ ::  MisoString -> Attribute model
strokeWidth_ = attr "strokeWidth"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke>
stroke_ ::  MisoString -> Attribute model
stroke_ = attr "stroke"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textAnchor>
textAnchor_ ::  MisoString -> Attribute model
textAnchor_ = attr "textAnchor"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textDecoration>
textDecoration_ ::  MisoString -> Attribute model
textDecoration_ = attr "textDecoration"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/textRendering>
textRendering_ ::  MisoString -> Attribute model
textRendering_ = attr "textRendering"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/unicodeBidi>
unicodeBidi_ ::  MisoString -> Attribute model
unicodeBidi_ = attr "unicodeBidi"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/visibility>
visibility_ ::  MisoString -> Attribute model
visibility_ = attr "visibility"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/wordSpacing>
wordSpacing_ ::  MisoString -> Attribute model
wordSpacing_ = attr "wordSpacing"
-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/writingMode>
writingMode_ ::  MisoString -> Attribute model
writingMode_ = attr "writingMode"
