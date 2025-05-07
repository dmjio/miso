-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Style
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Style
  ( -- *** Types
    Style
  , Styles
  , StyleSheet
    -- *** Smart Constructor
  , style_
  , styleInline_
  , sheet_
  , (=:)
    -- *** Render
  , renderStyleSheet
    -- *** Combinators
  , alignContent
  , alignItems
  , alignSelf
  , animationDelay
  , animationDirection
  , animationDuration
  , animationFillMode
  , animationIterationCount
  , animation
  , animationName
  , animationPlayState
  , animationTimingFunction
  , aspectRatio
  , backgroundClip
  , backgroundColor
  , backgroundImage
  , background
  , backgroundOrigin
  , backgroundPosition
  , backgroundRepeat
  , backgroundSize
  , borderBottomColor
  , borderBottomLeftRadius
  , borderBottom
  , borderBottomRightRadius
  , borderBottomStyle
  , borderBottomWidth
  , borderColor
  , borderEndEndRadius
  , borderEndStartRadius
  , borderInlineEndColor
  , borderInlineEndStyle
  , borderInlineEndWidth
  , borderInlineStartColor
  , borderInlineStartStyle
  , borderInlineStartWidth
  , borderLeftColor
  , borderLeft
  , borderLeftStyle
  , borderLeftWidth
  , border
  , borderRadius
  , borderRightColor
  , borderRight
  , borderRightStyle
  , borderRightWidth
  , borderStartEndRadius
  , borderStartStartRadius
  , borderStyle
  , borderTopColor
  , borderTopLeftRadius
  , borderTop
  , borderTopRightRadius
  , borderTopStyle
  , borderTopWidth
  , borderWidth
  , bottom
  , boxShadow
  , boxSizing
  , clipPath
  , color
  , columnGap
  , cssVariable
  , direction
  , display
  , filter
  , flexBasis
  , flexDirection
  , flexFlow
  , flexGrow
  , flex
  , flexShrink
  , flexWrap
  , fontFamily
  , fontSize
  , fontStyle
  , fontWeight
  , gap
  , gridAutoColumns
  , gridAutoFlow
  , gridAutoRows
  , gridColumnEnd
  , gridColumnSpan
  , gridColumnStart
  , gridRowEnd
  , gridRowSpan
  , gridRowStart
  , gridTemplateColumns
  , gridTemplateRows
  , height
  , imageRendering
  , insetInlineEnd
  , insetInlineStart
  , justifyContent
  , justifyItems
  , justifySelf
  , left
  , letterSpacing
  , linearCrossGravity
  , linearDirection
  , linearGravity
  , linearLayoutGravity
  , linearWeight
  , linearWeightSum
  , lineHeight
  , marginBottom
  , marginInlineEnd
  , marginInlineStart
  , marginLeft
  , margin
  , marginRight
  , marginTop
  , maskImage
  , mask
  , maxHeight
  , maxWidth
  , minHeight
  , minWidth
  , opacity
  , order
  , overflow
  , overflowX
  , overflowY
  , paddingBottom
  , paddingInlineEnd
  , paddingInlineStart
  , paddingLeft
  , padding
  , paddingRight
  , paddingTop
  , perspective
  , position
  , relativeAlignBottom
  , relativeAlignInlineEnd
  , relativeAlignInlineStart
  , relativeAlignLeft
  , relativeAlignRight
  , relativeAlignTop
  , relativeBottomOf
  , relativeCenter
  , relativeId
  , relativeInlineEndOf
  , relativeInlineStartOf
  , relativeLayoutOnce
  , relativeLeftOf
  , relativeRightOf
  , relativeTopOf
  , right
  , rowGap
  , textAlign
  , textDecoration
  , textIndent
  , textOverflow
  , textShadow
  , textStrokeColor
  , textStroke
  , textStrokeWidth
  , top
  , transform
  , transformOrigin
  , transitionDelay
  , transitionDuration
  , transition
  , transitionProperty
  , transitionTimingFunction
  , verticalAlign
  , visibility
  , whiteSpace
  , width
  , wordBreak
  , xAutoFontSize
  , xAutoFontSizePresetSizes
  , xHandleColor
  , xHandleSize
  , zIndex
  -- *** Colors
  , module Miso.Style.Color
  ) where
-----------------------------------------------------------------------------
import           Data.Map (Map)
import qualified Data.Map as M
import           Miso.String (MisoString)
import qualified Miso.String as MS
import           Miso.Style.Color
import           Miso.Property
import           Miso.Types (Attribute)
import qualified Miso.Types as MT
-----------------------------------------------------------------------------
import           Prelude hiding (filter)
-----------------------------------------------------------------------------
-- | Smart constructor for Attributes. This function is helpful when 
-- constructing 'Style'.
--
-- Example shown below.
--
-- @
-- div_ [ style_  [ "background" =: "red" ] []
-- @
--
(=:) :: k -> v -> (k, v)
k =: v = (k,v)
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | 'Style'
--
-- Type for a CSS 'Style'
--
type Style = (MisoString, MisoString)
-----------------------------------------------------------------------------
-- | 'Styles'
--
-- Type for a @Map@ of CSS 'Style'. Used with @StyleSheet@.
--
type Styles = Map MisoString MisoString
-----------------------------------------------------------------------------
-- | 'StyleSheet'
--
-- Type for a CSS style on native
--
newtype StyleSheet = StyleSheet { getStyleSheet :: Map MisoString Styles }
-----------------------------------------------------------------------------
sheet_ :: [(MisoString, Styles)] -> StyleSheet
sheet_ = StyleSheet . M.fromList
-----------------------------------------------------------------------------
-- | @style_@ is an attribute that will set the @style@
-- attribute of the associated DOM node to @attrs@.
--
-- @style@ attributes not contained in @attrs@ will be deleted.
--
-- > import qualified Data.Map as M
-- > div_ [ style_ [ backgroundColor "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: [Style] -> Attribute action
style_ = MT.Styles . M.fromList
-----------------------------------------------------------------------------
-- | Set "style" property
--
-- > view m = div_ [ styleInline_ "background-color:red;color:blue;" ] [ "foo" ]
--
-- https://developer.mozilla.org/en-US/docs/Web/CSS
styleInline_ ::  MisoString -> Attribute action
styleInline_ = textProp "style"
-----------------------------------------------------------------------------
-- | 'renderStyleSheet'
--
-- Renders a 'StyleSheet' to a 'MisoString'
--
-- @
-- testSheet :: StyleSheet
-- testSheet =
--   sheet_
--   [ ".name" =:
--       style_
--       [ backgroundColor "red"
--       , alignContent "top"
--       ]
--   , "#container" =:
--       style_
--       [ backgroundColor "blue"
--       , alignContent "center"
--       ]
--   ]
-- @
--
renderStyles :: Styles -> MisoString
renderStyles m = MS.unlines
  [ mconcat [ spaced, k, ":", v, ";" ]
  | (k,v) <- M.toList m
  , let spaced = "  "
  ]
-----------------------------------------------------------------------------  
renderStyleSheet :: StyleSheet -> MisoString
renderStyleSheet styleSheet = mconcat
  [ MS.unlines
    [ selector
    , "{"
    , renderStyles styles <> "}"
    ]
  | (selector, styles) <- M.toList (getStyleSheet styleSheet)
  ]
-----------------------------------------------------------------------------
-- 'alignContent'
--
-- > styles_ [ alignContent .= "value" ]
--
alignContent :: MisoString -> Style
alignContent x = "align-content" =: x
-----------------------------------------------------------------------------
-- 'alignItems'
--
-- > styles_ [ alignItems .= "value" ]
--
alignItems :: MisoString -> Style
alignItems x = "align-items" =: x
-----------------------------------------------------------------------------
-- 'alignSelf'
--
-- > styles_ [ alignSelf .= "value" ]
--
alignSelf :: MisoString -> Style
alignSelf x = "align-self" =: x
-----------------------------------------------------------------------------
-- 'animationDelay'
--
-- > styles_ [ animationDelay .= "value" ]
--
animationDelay :: MisoString -> Style
animationDelay x = "animation-delay" =: x
-----------------------------------------------------------------------------
-- 'animationDirection'
--
-- > styles_ [ animationDirection .= "value" ]
--
animationDirection :: MisoString -> Style
animationDirection x = "animation-direction" =: x
-----------------------------------------------------------------------------
-- 'animationDuration'
--
-- > styles_ [ animationDuration .= "value" ]
--
animationDuration :: MisoString -> Style
animationDuration x = "animation-duration" =: x
-----------------------------------------------------------------------------
-- 'animationFillMode'
--
-- > styles_ [ animationFillMode .= "value" ]
--
animationFillMode :: MisoString -> Style
animationFillMode x = "animation-fill-mode" =: x
-----------------------------------------------------------------------------
-- 'animationIterationCount'
--
-- > styles_ [ animationIterationCount .= "value" ]
--
animationIterationCount :: MisoString -> Style
animationIterationCount x = "animation-iteration-count" =: x
-----------------------------------------------------------------------------
-- 'animation'
--
-- > styles_ [ animation .= "value" ]
--
animation :: MisoString -> Style
animation x = "animation" =: x
-----------------------------------------------------------------------------
-- 'animationName'
--
-- > styles_ [ animationName .= "value" ]
--
animationName :: MisoString -> Style
animationName x = "animation-name" =: x
-----------------------------------------------------------------------------
-- 'animationPlayState'
--
-- > styles_ [ animationPlayState .= "value" ]
--
animationPlayState :: MisoString -> Style
animationPlayState x = "animation-play-state" =: x
-----------------------------------------------------------------------------
-- 'animationTimingFunction'
--
-- > styles_ [ animationTimingFunction .= "value" ]
--
animationTimingFunction :: MisoString -> Style
animationTimingFunction x = "animation-timing-function" =: x
-----------------------------------------------------------------------------
-- 'aspectRatio'
--
-- > styles_ [ aspectRatio .= "value" ]
--
aspectRatio :: MisoString -> Style
aspectRatio x = "aspect-ratio" =: x
-----------------------------------------------------------------------------
-- 'backgroundClip'
--
-- > styles_ [ backgroundClip .= "value" ]
--
backgroundClip :: MisoString -> Style
backgroundClip x = "background-clip" =: x
-----------------------------------------------------------------------------
-- 'backgroundColor'
--
-- > styles_ [ backgroundColor .= "value" ]
--
backgroundColor :: Color -> Style
backgroundColor x = "background-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'backgroundImage'
--
-- > styles_ [ backgroundImage .= "value" ]
--
backgroundImage :: MisoString -> Style
backgroundImage x = "background-image" =: x
-----------------------------------------------------------------------------
-- 'background'
--
-- > styles_ [ background .= "value" ]
--
background :: MisoString -> Style
background x = "background" =: x
-----------------------------------------------------------------------------
-- 'backgroundOrigin'
--
-- > styles_ [ backgroundOrigin .= "value" ]
--
backgroundOrigin :: MisoString -> Style
backgroundOrigin x = "background-origin" =: x
-----------------------------------------------------------------------------
-- 'backgroundPosition'
--
-- > styles_ [ backgroundPosition .= "value" ]
--
backgroundPosition :: MisoString -> Style
backgroundPosition x = "background-position" =: x
-----------------------------------------------------------------------------
-- 'backgroundRepeat'
--
-- > styles_ [ backgroundRepeat .= "value" ]
--
backgroundRepeat :: MisoString -> Style
backgroundRepeat x = "background-repeat" =: x
-----------------------------------------------------------------------------
-- 'backgroundSize'
--
-- > styles_ [ backgroundSize .= "value" ]
--
backgroundSize :: MisoString -> Style
backgroundSize x = "background-size" =: x
-----------------------------------------------------------------------------
-- 'borderBottomColor'
--
-- > styles_ [ borderBottomColor .= "value" ]
--
borderBottomColor :: Color -> Style
borderBottomColor x = "border-bottom-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderBottomLeftRadius'
--
-- > styles_ [ borderBottomLeftRadius .= "value" ]
--
borderBottomLeftRadius :: MisoString -> Style
borderBottomLeftRadius x = "border-bottom-left-radius" =: x
-----------------------------------------------------------------------------
-- 'borderBottom'
--
-- > styles_ [ borderBottom .= "value" ]
--
borderBottom :: MisoString -> Style
borderBottom x = "border-bottom" =: x
-----------------------------------------------------------------------------
-- 'borderBottomRightRadius'
--
-- > styles_ [ borderBottomRightRadius .= "value" ]
--
borderBottomRightRadius :: MisoString -> Style
borderBottomRightRadius x = "border-bottom-right-radius" =: x
-----------------------------------------------------------------------------
-- 'borderBottomStyle'
--
-- > styles_ [ borderBottomStyle .= "value" ]
--
borderBottomStyle :: MisoString -> Style
borderBottomStyle x = "border-bottom-style" =: x
-----------------------------------------------------------------------------
-- 'borderBottomWidth'
--
-- > styles_ [ borderBottomWidth .= "value" ]
--
borderBottomWidth :: MisoString -> Style
borderBottomWidth x = "border-bottom-width" =: x
-----------------------------------------------------------------------------
-- 'borderColor'
--
-- > styles_ [ borderColor .= "value" ]
--
borderColor :: Color -> Style
borderColor x = "border-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderEndEndRadius'
--
-- > styles_ [ borderEndEndRadius .= "value" ]
--
borderEndEndRadius :: MisoString -> Style
borderEndEndRadius x = "border-end-end-radius" =: x
-----------------------------------------------------------------------------
-- 'borderEndStartRadius'
--
-- > styles_ [ borderEndStartRadius .= "value" ]
--
borderEndStartRadius :: MisoString -> Style
borderEndStartRadius x = "border-end-start-radius" =: x
-----------------------------------------------------------------------------
-- 'borderInlineEndColor'
--
-- > styles_ [ borderInlineEndColor .= "value" ]
--
borderInlineEndColor :: Color -> Style
borderInlineEndColor x = "border-inline-end-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderInlineEndStyle'
--
-- > styles_ [ borderInlineEndStyle .= "value" ]
--
borderInlineEndStyle :: MisoString -> Style
borderInlineEndStyle x = "border-inline-end-style" =: x
-----------------------------------------------------------------------------
-- 'borderInlineEndWidth'
--
-- > styles_ [ borderInlineEndWidth .= "value" ]
--
borderInlineEndWidth :: MisoString -> Style
borderInlineEndWidth x = "border-inline-end-width" =: x
-----------------------------------------------------------------------------
-- 'borderInlineStartColor'
--
-- > styles_ [ borderInlineStartColor .= "value" ]
--
borderInlineStartColor :: Color -> Style
borderInlineStartColor x = "border-inline-start-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderInlineStartStyle'
--
-- > styles_ [ borderInlineStartStyle .= "value" ]
--
borderInlineStartStyle :: MisoString -> Style
borderInlineStartStyle x = "border-inline-start-style" =: x
-----------------------------------------------------------------------------
-- 'borderInlineStartWidth'
--
-- > styles_ [ borderInlineStartWidth .= "value" ]
--
borderInlineStartWidth :: MisoString -> Style
borderInlineStartWidth x = "border-inline-start-width" =: x
-----------------------------------------------------------------------------
-- 'borderLeftColor'
--
-- > styles_ [ borderLeftColor .= "value" ]
--
borderLeftColor :: Color -> Style
borderLeftColor x = "border-left-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderLeft'
--
-- > styles_ [ borderLeft .= "value" ]
--
borderLeft :: MisoString -> Style
borderLeft x = "border-left" =: x
-----------------------------------------------------------------------------
-- 'borderLeftStyle'
--
-- > styles_ [ borderLeftStyle .= "value" ]
--
borderLeftStyle :: MisoString -> Style
borderLeftStyle x = "border-left-style" =: x
-----------------------------------------------------------------------------
-- 'borderLeftWidth'
--
-- > styles_ [ borderLeftWidth .= "value" ]
--
borderLeftWidth :: MisoString -> Style
borderLeftWidth x = "border-left-width" =: x
-----------------------------------------------------------------------------
-- 'border'
--
-- > styles_ [ border .= "value" ]
--
border :: MisoString -> Style
border x = "border" =: x
-----------------------------------------------------------------------------
-- 'borderRadius'
--
-- > styles_ [ borderRadius .= "value" ]
--
borderRadius :: MisoString -> Style
borderRadius x = "border-radius" =: x
-----------------------------------------------------------------------------
-- 'borderRightColor'
--
-- > styles_ [ borderRightColor .= "value" ]
--
borderRightColor :: Color -> Style
borderRightColor x = "border-right-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderRight'
--
-- > styles_ [ borderRight .= "value" ]
--
borderRight :: MisoString -> Style
borderRight x = "border-right" =: x
-----------------------------------------------------------------------------
-- 'borderRightStyle'
--
-- > styles_ [ borderRightStyle .= "value" ]
--
borderRightStyle :: MisoString -> Style
borderRightStyle x = "border-right-style" =: x
-----------------------------------------------------------------------------
-- 'borderRightWidth'
--
-- > styles_ [ borderRightWidth .= "value" ]
--
borderRightWidth :: MisoString -> Style
borderRightWidth x = "border-right-width" =: x
-----------------------------------------------------------------------------
-- 'borderStartEndRadius'
--
-- > styles_ [ borderStartEndRadius .= "value" ]
--
borderStartEndRadius :: MisoString -> Style
borderStartEndRadius x = "border-start-end-radius" =: x
-----------------------------------------------------------------------------
-- 'borderStartStartRadius'
--
-- > styles_ [ borderStartStartRadius .= "value" ]
--
borderStartStartRadius :: MisoString -> Style
borderStartStartRadius x = "border-start-start-radius" =: x
-----------------------------------------------------------------------------
-- 'borderStyle'
--
-- > styles_ [ borderStyle .= "value" ]
--
borderStyle :: MisoString -> Style
borderStyle x = "border-style" =: x
-----------------------------------------------------------------------------
-- 'borderTopColor'
--
-- > styles_ [ borderTopColor .= "value" ]
--
borderTopColor :: Color -> Style
borderTopColor x = "border-top-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'borderTopLeftRadius'
--
-- > styles_ [ borderTopLeftRadius .= "value" ]
--
borderTopLeftRadius :: MisoString -> Style
borderTopLeftRadius x = "border-top-left-radius" =: x
-----------------------------------------------------------------------------
-- 'borderTop'
--
-- > styles_ [ borderTop .= "value" ]
--
borderTop :: MisoString -> Style
borderTop x = "border-top" =: x
-----------------------------------------------------------------------------
-- 'borderTopRightRadius'
--
-- > styles_ [ borderTopRightRadius .= "value" ]
--
borderTopRightRadius :: MisoString -> Style
borderTopRightRadius x = "border-top-right-radius" =: x
-----------------------------------------------------------------------------
-- 'borderTopStyle'
--
-- > styles_ [ borderTopStyle .= "value" ]
--
borderTopStyle :: MisoString -> Style
borderTopStyle x = "border-top-style" =: x
-----------------------------------------------------------------------------
-- 'borderTopWidth'
--
-- > styles_ [ borderTopWidth .= "value" ]
--
borderTopWidth :: MisoString -> Style
borderTopWidth x = "border-top-width" =: x
-----------------------------------------------------------------------------
-- 'borderWidth'
--
-- > styles_ [ borderWidth .= "value" ]
--
borderWidth :: MisoString -> Style
borderWidth x = "border-width" =: x
-----------------------------------------------------------------------------
-- 'bottom'
--
-- > styles_ [ bottom .= "value" ]
--
bottom :: MisoString -> Style
bottom x = "bottom" =: x
-----------------------------------------------------------------------------
-- 'boxShadow'
--
-- > styles_ [ boxShadow .= "value" ]
--
boxShadow :: MisoString -> Style
boxShadow x = "box-shadow" =: x
-----------------------------------------------------------------------------
-- 'boxSizing'
--
-- > styles_ [ boxSizing .= "value" ]
--
boxSizing :: MisoString -> Style
boxSizing x = "box-sizing" =: x
-----------------------------------------------------------------------------
-- 'clipPath'
--
-- > styles_ [ clipPath .= "value" ]
--
clipPath :: MisoString -> Style
clipPath x = "clip-path" =: x
-----------------------------------------------------------------------------
-- 'color'
--
-- > styles_ [ color .= "value" ]
--
color :: Color -> Style
color x = "color" =: renderColor x
-----------------------------------------------------------------------------
-- 'columnGap'
--
-- > styles_ [ columnGap .= "value" ]
--
columnGap :: MisoString -> Style
columnGap x = "column-gap" =: x
-----------------------------------------------------------------------------
-- 'cssVariable'
--
-- > styles_ [ cssVariable .= "value" ]
--
cssVariable :: MisoString -> Style
cssVariable x = "css-variable" =: x
-----------------------------------------------------------------------------
-- 'direction'
--
-- > styles_ [ direction .= "value" ]
--
direction :: MisoString -> Style
direction x = "direction" =: x
-----------------------------------------------------------------------------
-- 'display'
--
-- > styles_ [ display .= "value" ]
--
display :: MisoString -> Style
display x = "display" =: x
-----------------------------------------------------------------------------
-- 'filter'
--
-- > styles_ [ filter .= "value" ]
--
filter :: MisoString -> Style
filter x = "filter" =: x
-----------------------------------------------------------------------------
-- 'flexBasis'
--
-- > styles_ [ flexBasis .= "value" ]
--
flexBasis :: MisoString -> Style
flexBasis x = "flex-basis" =: x
-----------------------------------------------------------------------------
-- 'flexDirection'
--
-- > styles_ [ flexDirection .= "value" ]
--
flexDirection :: MisoString -> Style
flexDirection x = "flex-direction" =: x
-----------------------------------------------------------------------------
-- 'flexFlow'
--
-- > styles_ [ flexFlow .= "value" ]
--
flexFlow :: MisoString -> Style
flexFlow x = "flex-flow" =: x
-----------------------------------------------------------------------------
-- 'flexGrow'
--
-- > styles_ [ flexGrow .= "value" ]
--
flexGrow :: MisoString -> Style
flexGrow x = "flex-grow" =: x
-----------------------------------------------------------------------------
-- 'flex'
--
-- > styles_ [ flex .= "value" ]
--
flex :: MisoString -> Style
flex x = "flex" =: x
-----------------------------------------------------------------------------
-- 'flexShrink'
--
-- > styles_ [ flexShrink .= "value" ]
--
flexShrink :: MisoString -> Style
flexShrink x = "flex-shrink" =: x
-----------------------------------------------------------------------------
-- 'flexWrap'
--
-- > styles_ [ flexWrap .= "value" ]
--
flexWrap :: MisoString -> Style
flexWrap x = "flex-wrap" =: x
-----------------------------------------------------------------------------
-- 'fontFamily'
--
-- > styles_ [ fontFamily .= "value" ]
--
fontFamily :: MisoString -> Style
fontFamily x = "font-family" =: x
-----------------------------------------------------------------------------
-- 'fontSize'
--
-- > styles_ [ fontSize .= "value" ]
--
fontSize :: MisoString -> Style
fontSize x = "font-size" =: x
-----------------------------------------------------------------------------
-- 'fontStyle'
--
-- > styles_ [ fontStyle .= "value" ]
--
fontStyle :: MisoString -> Style
fontStyle x = "font-style" =: x
-----------------------------------------------------------------------------
-- 'fontWeight'
--
-- > styles_ [ fontWeight .= "value" ]
--
fontWeight :: MisoString -> Style
fontWeight x = "font-weight" =: x
-----------------------------------------------------------------------------
-- 'gap'
--
-- > styles_ [ gap .= "value" ]
--
gap :: MisoString -> Style
gap x = "gap" =: x
-----------------------------------------------------------------------------
-- 'gridAutoColumns'
--
-- > styles_ [ gridAutoColumns .= "value" ]
--
gridAutoColumns :: MisoString -> Style
gridAutoColumns x = "grid-auto-columns" =: x
-----------------------------------------------------------------------------
-- 'gridAutoFlow'
--
-- > styles_ [ gridAutoFlow .= "value" ]
--
gridAutoFlow :: MisoString -> Style
gridAutoFlow x = "grid-auto-flow" =: x
-----------------------------------------------------------------------------
-- 'gridAutoRows'
--
-- > styles_ [ gridAutoRows .= "value" ]
--
gridAutoRows :: MisoString -> Style
gridAutoRows x = "grid-auto-rows" =: x
-----------------------------------------------------------------------------
-- 'gridColumnEnd'
--
-- > styles_ [ gridColumnEnd .= "value" ]
--
gridColumnEnd :: MisoString -> Style
gridColumnEnd x = "grid-column-end" =: x
-----------------------------------------------------------------------------
-- 'gridColumnSpan'
--
-- > styles_ [ gridColumnSpan .= "value" ]
--
gridColumnSpan :: MisoString -> Style
gridColumnSpan x = "grid-column-span" =: x
-----------------------------------------------------------------------------
-- 'gridColumnStart'
--
-- > styles_ [ gridColumnStart .= "value" ]
--
gridColumnStart :: MisoString -> Style
gridColumnStart x = "grid-column-start" =: x
-----------------------------------------------------------------------------
-- 'gridRowEnd'
--
-- > styles_ [ gridRowEnd .= "value" ]
--
gridRowEnd :: MisoString -> Style
gridRowEnd x = "grid-row-end" =: x
-----------------------------------------------------------------------------
-- 'gridRowSpan'
--
-- > styles_ [ gridRowSpan .= "value" ]
--
gridRowSpan :: MisoString -> Style
gridRowSpan x = "grid-row-span" =: x
-----------------------------------------------------------------------------
-- 'gridRowStart'
--
-- > styles_ [ gridRowStart .= "value" ]
--
gridRowStart :: MisoString -> Style
gridRowStart x = "grid-row-start" =: x
-----------------------------------------------------------------------------
-- 'gridTemplateColumns'
--
-- > styles_ [ gridTemplateColumns .= "value" ]
--
gridTemplateColumns :: MisoString -> Style
gridTemplateColumns x = "grid-template-columns" =: x
-----------------------------------------------------------------------------
-- 'gridTemplateRows'
--
-- > styles_ [ gridTemplateRows .= "value" ]
--
gridTemplateRows :: MisoString -> Style
gridTemplateRows x = "grid-template-rows" =: x
-----------------------------------------------------------------------------
-- 'height'
--
-- > styles_ [ height .= "value" ]
--
height :: MisoString -> Style
height x = "height" =: x
-----------------------------------------------------------------------------
-- 'imageRendering'
--
-- > styles_ [ imageRendering .= "value" ]
--
imageRendering :: MisoString -> Style
imageRendering x = "image-rendering" =: x
-----------------------------------------------------------------------------
-- 'insetInlineEnd'
--
-- > styles_ [ insetInlineEnd .= "value" ]
--
insetInlineEnd :: MisoString -> Style
insetInlineEnd x = "inset-inline-end" =: x
-----------------------------------------------------------------------------
-- 'insetInlineStart'
--
-- > styles_ [ insetInlineStart .= "value" ]
--
insetInlineStart :: MisoString -> Style
insetInlineStart x = "inset-inline-start" =: x
-----------------------------------------------------------------------------
-- 'justifyContent'
--
-- > styles_ [ justifyContent .= "value" ]
--
justifyContent :: MisoString -> Style
justifyContent x = "justify-content" =: x
-----------------------------------------------------------------------------
-- 'justifyItems'
--
-- > styles_ [ justifyItems .= "value" ]
--
justifyItems :: MisoString -> Style
justifyItems x = "justify-items" =: x
-----------------------------------------------------------------------------
-- 'justifySelf'
--
-- > styles_ [ justifySelf .= "value" ]
--
justifySelf :: MisoString -> Style
justifySelf x = "justify-self" =: x
-----------------------------------------------------------------------------
-- 'left'
--
-- > styles_ [ left .= "value" ]
--
left :: MisoString -> Style
left x = "left" =: x
-----------------------------------------------------------------------------
-- 'letterSpacing'
--
-- > styles_ [ letterSpacing .= "value" ]
--
letterSpacing :: MisoString -> Style
letterSpacing x = "letter-spacing" =: x
-----------------------------------------------------------------------------
-- 'linearCrossGravity'
--
-- > styles_ [ linearCrossGravity .= "value" ]
--
linearCrossGravity :: MisoString -> Style
linearCrossGravity x = "linear-cross-gravity" =: x
-----------------------------------------------------------------------------
-- 'linearDirection'
--
-- > styles_ [ linearDirection .= "value" ]
--
linearDirection :: MisoString -> Style
linearDirection x = "linear-direction" =: x
-----------------------------------------------------------------------------
-- 'linearGravity'
--
-- > styles_ [ linearGravity .= "value" ]
--
linearGravity :: MisoString -> Style
linearGravity x = "linear-gravity" =: x
-----------------------------------------------------------------------------
-- 'linearLayoutGravity'
--
-- > styles_ [ linearLayoutGravity .= "value" ]
--
linearLayoutGravity :: MisoString -> Style
linearLayoutGravity x = "linear-layout-gravity" =: x
-----------------------------------------------------------------------------
-- 'linearWeight'
--
-- > styles_ [ linearWeight .= "value" ]
--
linearWeight :: MisoString -> Style
linearWeight x = "linear-weight" =: x
-----------------------------------------------------------------------------
-- 'linearWeightSum'
--
-- > styles_ [ linearWeightSum .= "value" ]
--
linearWeightSum :: MisoString -> Style
linearWeightSum x = "linear-weight-sum" =: x
-----------------------------------------------------------------------------
-- 'lineHeight'
--
-- > styles_ [ lineHeight .= "value" ]
--
lineHeight :: MisoString -> Style
lineHeight x = "line-height" =: x
-----------------------------------------------------------------------------
-- 'marginBottom'
--
-- > styles_ [ marginBottom .= "value" ]
--
marginBottom :: MisoString -> Style
marginBottom x = "margin-bottom" =: x
-----------------------------------------------------------------------------
-- 'marginInlineEnd'
--
-- > styles_ [ marginInlineEnd .= "value" ]
--
marginInlineEnd :: MisoString -> Style
marginInlineEnd x = "margin-inline-end" =: x
-----------------------------------------------------------------------------
-- 'marginInlineStart'
--
-- > styles_ [ marginInlineStart .= "value" ]
--
marginInlineStart :: MisoString -> Style
marginInlineStart x = "margin-inline-start" =: x
-----------------------------------------------------------------------------
-- 'marginLeft'
--
-- > styles_ [ marginLeft .= "value" ]
--
marginLeft :: MisoString -> Style
marginLeft x = "margin-left" =: x
-----------------------------------------------------------------------------
-- 'margin'
--
-- > styles_ [ margin .= "value" ]
--
margin :: MisoString -> Style
margin x = "margin" =: x
-----------------------------------------------------------------------------
-- 'marginRight'
--
-- > styles_ [ marginRight .= "value" ]
--
marginRight :: MisoString -> Style
marginRight x = "margin-right" =: x
-----------------------------------------------------------------------------
-- 'marginTop'
--
-- > styles_ [ marginTop .= "value" ]
--
marginTop :: MisoString -> Style
marginTop x = "margin-top" =: x
-----------------------------------------------------------------------------
-- 'maskImage'
--
-- > styles_ [ maskImage .= "value" ]
--
maskImage :: MisoString -> Style
maskImage x = "mask-image" =: x
-----------------------------------------------------------------------------
-- 'mask'
--
-- > styles_ [ mask .= "value" ]
--
mask :: MisoString -> Style
mask x = "mask" =: x
-----------------------------------------------------------------------------
-- 'maxHeight'
--
-- > styles_ [ maxHeight .= "value" ]
--
maxHeight :: MisoString -> Style
maxHeight x = "max-height" =: x
-----------------------------------------------------------------------------
-- 'maxWidth'
--
-- > styles_ [ maxWidth .= "value" ]
--
maxWidth :: MisoString -> Style
maxWidth x = "max-width" =: x
-----------------------------------------------------------------------------
-- 'minHeight'
--
-- > styles_ [ minHeight .= "value" ]
--
minHeight :: MisoString -> Style
minHeight x = "min-height" =: x
-----------------------------------------------------------------------------
-- 'minWidth'
--
-- > styles_ [ minWidth .= "value" ]
--
minWidth :: MisoString -> Style
minWidth x = "min-width" =: x
-----------------------------------------------------------------------------
-- 'opacity'
--
-- > styles_ [ opacity .= "value" ]
--
opacity :: MisoString -> Style
opacity x = "opacity" =: x
-----------------------------------------------------------------------------
-- 'order'
--
-- > styles_ [ order .= "value" ]
--
order :: MisoString -> Style
order x = "order" =: x
-----------------------------------------------------------------------------
-- 'overflow'
--
-- > styles_ [ overflow .= "value" ]
--
overflow :: MisoString -> Style
overflow x = "overflow" =: x
-----------------------------------------------------------------------------
-- 'overflowX'
--
-- > styles_ [ overflowX .= "value" ]
--
overflowX :: MisoString -> Style
overflowX x = "overflow-x" =: x
-----------------------------------------------------------------------------
-- 'overflowY'
--
-- > styles_ [ overflowY .= "value" ]
--
overflowY :: MisoString -> Style
overflowY x = "overflow-y" =: x
-----------------------------------------------------------------------------
-- 'paddingBottom'
--
-- > styles_ [ paddingBottom .= "value" ]
--
paddingBottom :: MisoString -> Style
paddingBottom x = "padding-bottom" =: x
-----------------------------------------------------------------------------
-- 'paddingInlineEnd'
--
-- > styles_ [ paddingInlineEnd .= "value" ]
--
paddingInlineEnd :: MisoString -> Style
paddingInlineEnd x = "padding-inline-end" =: x
-----------------------------------------------------------------------------
-- 'paddingInlineStart'
--
-- > styles_ [ paddingInlineStart .= "value" ]
--
paddingInlineStart :: MisoString -> Style
paddingInlineStart x = "padding-inline-start" =: x
-----------------------------------------------------------------------------
-- 'paddingLeft'
--
-- > styles_ [ paddingLeft .= "value" ]
--
paddingLeft :: MisoString -> Style
paddingLeft x = "padding-left" =: x
-----------------------------------------------------------------------------
-- 'padding'
--
-- > styles_ [ padding .= "value" ]
--
padding :: MisoString -> Style
padding x = "padding" =: x
-----------------------------------------------------------------------------
-- 'paddingRight'
--
-- > styles_ [ paddingRight .= "value" ]
--
paddingRight :: MisoString -> Style
paddingRight x = "padding-right" =: x
-----------------------------------------------------------------------------
-- 'paddingTop'
--
-- > styles_ [ paddingTop .= "value" ]
--
paddingTop :: MisoString -> Style
paddingTop x = "padding-top" =: x
-----------------------------------------------------------------------------
-- 'perspective'
--
-- > styles_ [ perspective .= "value" ]
--
perspective :: MisoString -> Style
perspective x = "perspective" =: x
-----------------------------------------------------------------------------
-- 'position'
--
-- > styles_ [ position .= "value" ]
--
position :: MisoString -> Style
position x = "position" =: x
-----------------------------------------------------------------------------
-- 'relativeAlignBottom'
--
-- > styles_ [ relativeAlignBottom .= "value" ]
--
relativeAlignBottom :: MisoString -> Style
relativeAlignBottom x = "relative-align-bottom" =: x
-----------------------------------------------------------------------------
-- 'relativeAlignInlineEnd'
--
-- > styles_ [ relativeAlignInlineEnd .= "value" ]
--
relativeAlignInlineEnd :: MisoString -> Style
relativeAlignInlineEnd x = "relative-align-inline-end" =: x
-----------------------------------------------------------------------------
-- 'relativeAlignInlineStart'
--
-- > styles_ [ relativeAlignInlineStart .= "value" ]
--
relativeAlignInlineStart :: MisoString -> Style
relativeAlignInlineStart x = "relative-align-inline-start" =: x
-----------------------------------------------------------------------------
-- 'relativeAlignLeft'
--
-- > styles_ [ relativeAlignLeft .= "value" ]
--
relativeAlignLeft :: MisoString -> Style
relativeAlignLeft x = "relative-align-left" =: x
-----------------------------------------------------------------------------
-- 'relativeAlignRight'
--
-- > styles_ [ relativeAlignRight .= "value" ]
--
relativeAlignRight :: MisoString -> Style
relativeAlignRight x = "relative-align-right" =: x
-----------------------------------------------------------------------------
-- 'relativeAlignTop'
--
-- > styles_ [ relativeAlignTop .= "value" ]
--
relativeAlignTop :: MisoString -> Style
relativeAlignTop x = "relative-align-top" =: x
-----------------------------------------------------------------------------
-- 'relativeBottomOf'
--
-- > styles_ [ relativeBottomOf .= "value" ]
--
relativeBottomOf :: MisoString -> Style
relativeBottomOf x = "relative-bottom-of" =: x
-----------------------------------------------------------------------------
-- 'relativeCenter'
--
-- > styles_ [ relativeCenter .= "value" ]
--
relativeCenter :: MisoString -> Style
relativeCenter x = "relative-center" =: x
-----------------------------------------------------------------------------
-- 'relativeId'
--
-- > styles_ [ relativeId .= "value" ]
--
relativeId :: MisoString -> Style
relativeId x = "relative-id" =: x
-----------------------------------------------------------------------------
-- 'relativeInlineEndOf'
--
-- > styles_ [ relativeInlineEndOf .= "value" ]
--
relativeInlineEndOf :: MisoString -> Style
relativeInlineEndOf x = "relative-inline-end-of" =: x
-----------------------------------------------------------------------------
-- 'relativeInlineStartOf'
--
-- > styles_ [ relativeInlineStartOf .= "value" ]
--
relativeInlineStartOf :: MisoString -> Style
relativeInlineStartOf x = "relative-inline-start-of" =: x
-----------------------------------------------------------------------------
-- 'relativeLayoutOnce'
--
-- > styles_ [ relativeLayoutOnce .= "value" ]
--
relativeLayoutOnce :: MisoString -> Style
relativeLayoutOnce x = "relative-layout-once" =: x
-----------------------------------------------------------------------------
-- 'relativeLeftOf'
--
-- > styles_ [ relativeLeftOf .= "value" ]
--
relativeLeftOf :: MisoString -> Style
relativeLeftOf x = "relative-left-of" =: x
-----------------------------------------------------------------------------
-- 'relativeRightOf'
--
-- > styles_ [ relativeRightOf .= "value" ]
--
relativeRightOf :: MisoString -> Style
relativeRightOf x = "relative-right-of" =: x
-----------------------------------------------------------------------------
-- 'relativeTopOf'
--
-- > styles_ [ relativeTopOf .= "value" ]
--
relativeTopOf :: MisoString -> Style
relativeTopOf x = "relative-top-of" =: x
-----------------------------------------------------------------------------
-- 'right'
--
-- > styles_ [ right .= "value" ]
--
right :: MisoString -> Style
right x = "right" =: x
-----------------------------------------------------------------------------
-- 'rowGap'
--
-- > styles_ [ rowGap .= "value" ]
--
rowGap :: MisoString -> Style
rowGap x = "row-gap" =: x
-----------------------------------------------------------------------------
-- 'textAlign'
--
-- > styles_ [ textAlign .= "value" ]
--
textAlign :: MisoString -> Style
textAlign x = "text-align" =: x
-----------------------------------------------------------------------------
-- 'textDecoration'
--
-- > styles_ [ textDecoration .= "value" ]
--
textDecoration :: MisoString -> Style
textDecoration x = "text-decoration" =: x
-----------------------------------------------------------------------------
-- 'textIndent'
--
-- > styles_ [ textIndent .= "value" ]
--
textIndent :: MisoString -> Style
textIndent x = "text-indent" =: x
-----------------------------------------------------------------------------
-- 'textOverflow'
--
-- > styles_ [ textOverflow .= "value" ]
--
textOverflow :: MisoString -> Style
textOverflow x = "text-overflow" =: x
-----------------------------------------------------------------------------
-- 'textShadow'
--
-- > styles_ [ textShadow .= "value" ]
--
textShadow :: MisoString -> Style
textShadow x = "text-shadow" =: x
-----------------------------------------------------------------------------
-- 'textStrokeColor'
--
-- > styles_ [ textStrokeColor .= "value" ]
--
textStrokeColor :: Color -> Style
textStrokeColor x = "text-stroke-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'textStroke'
--
-- > styles_ [ textStroke .= "value" ]
--
textStroke :: MisoString -> Style
textStroke x = "text-stroke" =: x
-----------------------------------------------------------------------------
-- 'textStrokeWidth'
--
-- > styles_ [ textStrokeWidth .= "value" ]
--
textStrokeWidth :: MisoString -> Style
textStrokeWidth x = "text-stroke-width" =: x
-----------------------------------------------------------------------------
-- 'top'
--
-- > styles_ [ top .= "value" ]
--
top :: MisoString -> Style
top x = "top" =: x
-----------------------------------------------------------------------------
-- 'transform'
--
-- > styles_ [ transform .= "value" ]
--
transform :: MisoString -> Style
transform x = "transform" =: x
-----------------------------------------------------------------------------
-- 'transformOrigin'
--
-- > styles_ [ transformOrigin .= "value" ]
--
transformOrigin :: MisoString -> Style
transformOrigin x = "transform-origin" =: x
-----------------------------------------------------------------------------
-- 'transitionDelay'
--
-- > styles_ [ transitionDelay .= "value" ]
--
transitionDelay :: MisoString -> Style
transitionDelay x = "transition-delay" =: x
-----------------------------------------------------------------------------
-- 'transitionDuration'
--
-- > styles_ [ transitionDuration .= "value" ]
--
transitionDuration :: MisoString -> Style
transitionDuration x = "transition-duration" =: x
-----------------------------------------------------------------------------
-- 'transition'
--
-- > styles_ [ transition .= "value" ]
--
transition :: MisoString -> Style
transition x = "transition" =: x
-----------------------------------------------------------------------------
-- 'transitionProperty'
--
-- > styles_ [ transitionProperty .= "value" ]
--
transitionProperty :: MisoString -> Style
transitionProperty x = "transition-property" =: x
-----------------------------------------------------------------------------
-- 'transitionTimingFunction'
--
-- > styles_ [ transitionTimingFunction .= "value" ]
--
transitionTimingFunction :: MisoString -> Style
transitionTimingFunction x = "transition-timing-function" =: x
-----------------------------------------------------------------------------
-- 'verticalAlign'
--
-- > styles_ [ verticalAlign .= "value" ]
--
verticalAlign :: MisoString -> Style
verticalAlign x = "vertical-align" =: x
-----------------------------------------------------------------------------
-- 'visibility'
--
-- > styles_ [ visibility .= "value" ]
--
visibility :: MisoString -> Style
visibility x = "visibility" =: x
-----------------------------------------------------------------------------
-- 'whiteSpace'
--
-- > styles_ [ whiteSpace .= "value" ]
--
whiteSpace :: MisoString -> Style
whiteSpace x = "white-space" =: x
-----------------------------------------------------------------------------
-- 'width'
--
-- > styles_ [ width .= "value" ]
--
width :: MisoString -> Style
width x = "width" =: x
-----------------------------------------------------------------------------
-- 'wordBreak'
--
-- > styles_ [ wordBreak .= "value" ]
--
wordBreak :: MisoString -> Style
wordBreak x = "word-break" =: x
-----------------------------------------------------------------------------
-- 'xAutoFontSize'
--
-- > styles_ [ xAutoFontSize .= "value" ]
--
xAutoFontSize :: MisoString -> Style
xAutoFontSize x = "-x-auto-font-size" =: x
-----------------------------------------------------------------------------
-- 'xAutoFontSizePresetSizes'
--
-- > styles_ [ xAutoFontSizePresetSizes .= "value" ]
--
xAutoFontSizePresetSizes :: MisoString -> Style
xAutoFontSizePresetSizes x = "-x-auto-font-size-preset-sizes" =: x
-----------------------------------------------------------------------------
-- 'xHandleColor'
--
-- > styles_ [ xHandleColor .= "value" ]
--
xHandleColor :: Color -> Style
xHandleColor x = "-x-handle-color" =: renderColor x
-----------------------------------------------------------------------------
-- 'xHandleSize'
--
-- > styles_ [ xHandleSize .= "value" ]
--
xHandleSize :: MisoString -> Style
xHandleSize x = "-x-handle-size" =: x
-----------------------------------------------------------------------------
-- 'zIndex'
--
-- > styles_ [ zIndex .= "value" ]
--
zIndex :: MisoString -> Style
zIndex x = "z-index" =: x
-----------------------------------------------------------------------------
