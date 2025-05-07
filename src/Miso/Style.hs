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
--
-- > styles_ [ alignContent =: "value" ]
--
alignContent :: MisoString -> Style
alignContent x = "align-content" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ alignItems =: "value" ]
--
alignItems :: MisoString -> Style
alignItems x = "align-items" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ alignSelf =: "value" ]
--
alignSelf :: MisoString -> Style
alignSelf x = "align-self" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationDelay =: "value" ]
--
animationDelay :: MisoString -> Style
animationDelay x = "animation-delay" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationDirection =: "value" ]
--
animationDirection :: MisoString -> Style
animationDirection x = "animation-direction" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationDuration =: "value" ]
--
animationDuration :: MisoString -> Style
animationDuration x = "animation-duration" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationFillMode =: "value" ]
--
animationFillMode :: MisoString -> Style
animationFillMode x = "animation-fill-mode" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationIterationCount =: "value" ]
--
animationIterationCount :: MisoString -> Style
animationIterationCount x = "animation-iteration-count" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animation =: "value" ]
--
animation :: MisoString -> Style
animation x = "animation" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationName =: "value" ]
--
animationName :: MisoString -> Style
animationName x = "animation-name" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationPlayState =: "value" ]
--
animationPlayState :: MisoString -> Style
animationPlayState x = "animation-play-state" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ animationTimingFunction =: "value" ]
--
animationTimingFunction :: MisoString -> Style
animationTimingFunction x = "animation-timing-function" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ aspectRatio =: "value" ]
--
aspectRatio :: MisoString -> Style
aspectRatio x = "aspect-ratio" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundClip =: "value" ]
--
backgroundClip :: MisoString -> Style
backgroundClip x = "background-clip" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundColor =: "value" ]
--
backgroundColor :: Color -> Style
backgroundColor x = "background-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundImage =: "value" ]
--
backgroundImage :: MisoString -> Style
backgroundImage x = "background-image" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ background =: "value" ]
--
background :: MisoString -> Style
background x = "background" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundOrigin =: "value" ]
--
backgroundOrigin :: MisoString -> Style
backgroundOrigin x = "background-origin" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundPosition =: "value" ]
--
backgroundPosition :: MisoString -> Style
backgroundPosition x = "background-position" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundRepeat =: "value" ]
--
backgroundRepeat :: MisoString -> Style
backgroundRepeat x = "background-repeat" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ backgroundSize =: "value" ]
--
backgroundSize :: MisoString -> Style
backgroundSize x = "background-size" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderBottomColor =: "value" ]
--
borderBottomColor :: Color -> Style
borderBottomColor x = "border-bottom-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderBottomLeftRadius =: "value" ]
--
borderBottomLeftRadius :: MisoString -> Style
borderBottomLeftRadius x = "border-bottom-left-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderBottom =: "value" ]
--
borderBottom :: MisoString -> Style
borderBottom x = "border-bottom" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderBottomRightRadius =: "value" ]
--
borderBottomRightRadius :: MisoString -> Style
borderBottomRightRadius x = "border-bottom-right-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderBottomStyle =: "value" ]
--
borderBottomStyle :: MisoString -> Style
borderBottomStyle x = "border-bottom-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderBottomWidth =: "value" ]
--
borderBottomWidth :: MisoString -> Style
borderBottomWidth x = "border-bottom-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderColor =: "value" ]
--
borderColor :: Color -> Style
borderColor x = "border-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderEndEndRadius =: "value" ]
--
borderEndEndRadius :: MisoString -> Style
borderEndEndRadius x = "border-end-end-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderEndStartRadius =: "value" ]
--
borderEndStartRadius :: MisoString -> Style
borderEndStartRadius x = "border-end-start-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderInlineEndColor =: "value" ]
--
borderInlineEndColor :: Color -> Style
borderInlineEndColor x = "border-inline-end-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderInlineEndStyle =: "value" ]
--
borderInlineEndStyle :: MisoString -> Style
borderInlineEndStyle x = "border-inline-end-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderInlineEndWidth =: "value" ]
--
borderInlineEndWidth :: MisoString -> Style
borderInlineEndWidth x = "border-inline-end-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderInlineStartColor =: "value" ]
--
borderInlineStartColor :: Color -> Style
borderInlineStartColor x = "border-inline-start-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderInlineStartStyle =: "value" ]
--
borderInlineStartStyle :: MisoString -> Style
borderInlineStartStyle x = "border-inline-start-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderInlineStartWidth =: "value" ]
--
borderInlineStartWidth :: MisoString -> Style
borderInlineStartWidth x = "border-inline-start-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderLeftColor =: "value" ]
--
borderLeftColor :: Color -> Style
borderLeftColor x = "border-left-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderLeft =: "value" ]
--
borderLeft :: MisoString -> Style
borderLeft x = "border-left" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderLeftStyle =: "value" ]
--
borderLeftStyle :: MisoString -> Style
borderLeftStyle x = "border-left-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderLeftWidth =: "value" ]
--
borderLeftWidth :: MisoString -> Style
borderLeftWidth x = "border-left-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ border =: "value" ]
--
border :: MisoString -> Style
border x = "border" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderRadius =: "value" ]
--
borderRadius :: MisoString -> Style
borderRadius x = "border-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderRightColor =: "value" ]
--
borderRightColor :: Color -> Style
borderRightColor x = "border-right-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderRight =: "value" ]
--
borderRight :: MisoString -> Style
borderRight x = "border-right" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderRightStyle =: "value" ]
--
borderRightStyle :: MisoString -> Style
borderRightStyle x = "border-right-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderRightWidth =: "value" ]
--
borderRightWidth :: MisoString -> Style
borderRightWidth x = "border-right-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderStartEndRadius =: "value" ]
--
borderStartEndRadius :: MisoString -> Style
borderStartEndRadius x = "border-start-end-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderStartStartRadius =: "value" ]
--
borderStartStartRadius :: MisoString -> Style
borderStartStartRadius x = "border-start-start-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderStyle =: "value" ]
--
borderStyle :: MisoString -> Style
borderStyle x = "border-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderTopColor =: "value" ]
--
borderTopColor :: Color -> Style
borderTopColor x = "border-top-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderTopLeftRadius =: "value" ]
--
borderTopLeftRadius :: MisoString -> Style
borderTopLeftRadius x = "border-top-left-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderTop =: "value" ]
--
borderTop :: MisoString -> Style
borderTop x = "border-top" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderTopRightRadius =: "value" ]
--
borderTopRightRadius :: MisoString -> Style
borderTopRightRadius x = "border-top-right-radius" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderTopStyle =: "value" ]
--
borderTopStyle :: MisoString -> Style
borderTopStyle x = "border-top-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderTopWidth =: "value" ]
--
borderTopWidth :: MisoString -> Style
borderTopWidth x = "border-top-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ borderWidth =: "value" ]
--
borderWidth :: MisoString -> Style
borderWidth x = "border-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ bottom =: "value" ]
--
bottom :: MisoString -> Style
bottom x = "bottom" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ boxShadow =: "value" ]
--
boxShadow :: MisoString -> Style
boxShadow x = "box-shadow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ boxSizing =: "value" ]
--
boxSizing :: MisoString -> Style
boxSizing x = "box-sizing" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ clipPath =: "value" ]
--
clipPath :: MisoString -> Style
clipPath x = "clip-path" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ color =: "value" ]
--
color :: Color -> Style
color x = "color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ columnGap =: "value" ]
--
columnGap :: MisoString -> Style
columnGap x = "column-gap" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ cssVariable =: "value" ]
--
cssVariable :: MisoString -> Style
cssVariable x = "css-variable" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ direction =: "value" ]
--
direction :: MisoString -> Style
direction x = "direction" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ display =: "value" ]
--
display :: MisoString -> Style
display x = "display" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ filter =: "value" ]
--
filter :: MisoString -> Style
filter x = "filter" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flexBasis =: "value" ]
--
flexBasis :: MisoString -> Style
flexBasis x = "flex-basis" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flexDirection =: "value" ]
--
flexDirection :: MisoString -> Style
flexDirection x = "flex-direction" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flexFlow =: "value" ]
--
flexFlow :: MisoString -> Style
flexFlow x = "flex-flow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flexGrow =: "value" ]
--
flexGrow :: MisoString -> Style
flexGrow x = "flex-grow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flex =: "value" ]
--
flex :: MisoString -> Style
flex x = "flex" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flexShrink =: "value" ]
--
flexShrink :: MisoString -> Style
flexShrink x = "flex-shrink" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ flexWrap =: "value" ]
--
flexWrap :: MisoString -> Style
flexWrap x = "flex-wrap" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ fontFamily =: "value" ]
--
fontFamily :: MisoString -> Style
fontFamily x = "font-family" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ fontSize =: "value" ]
--
fontSize :: MisoString -> Style
fontSize x = "font-size" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ fontStyle =: "value" ]
--
fontStyle :: MisoString -> Style
fontStyle x = "font-style" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ fontWeight =: "value" ]
--
fontWeight :: MisoString -> Style
fontWeight x = "font-weight" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gap =: "value" ]
--
gap :: MisoString -> Style
gap x = "gap" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridAutoColumns =: "value" ]
--
gridAutoColumns :: MisoString -> Style
gridAutoColumns x = "grid-auto-columns" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridAutoFlow =: "value" ]
--
gridAutoFlow :: MisoString -> Style
gridAutoFlow x = "grid-auto-flow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridAutoRows =: "value" ]
--
gridAutoRows :: MisoString -> Style
gridAutoRows x = "grid-auto-rows" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridColumnEnd =: "value" ]
--
gridColumnEnd :: MisoString -> Style
gridColumnEnd x = "grid-column-end" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridColumnSpan =: "value" ]
--
gridColumnSpan :: MisoString -> Style
gridColumnSpan x = "grid-column-span" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridColumnStart =: "value" ]
--
gridColumnStart :: MisoString -> Style
gridColumnStart x = "grid-column-start" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridRowEnd =: "value" ]
--
gridRowEnd :: MisoString -> Style
gridRowEnd x = "grid-row-end" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridRowSpan =: "value" ]
--
gridRowSpan :: MisoString -> Style
gridRowSpan x = "grid-row-span" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridRowStart =: "value" ]
--
gridRowStart :: MisoString -> Style
gridRowStart x = "grid-row-start" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridTemplateColumns =: "value" ]
--
gridTemplateColumns :: MisoString -> Style
gridTemplateColumns x = "grid-template-columns" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ gridTemplateRows =: "value" ]
--
gridTemplateRows :: MisoString -> Style
gridTemplateRows x = "grid-template-rows" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ height =: "value" ]
--
height :: MisoString -> Style
height x = "height" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ imageRendering =: "value" ]
--
imageRendering :: MisoString -> Style
imageRendering x = "image-rendering" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ insetInlineEnd =: "value" ]
--
insetInlineEnd :: MisoString -> Style
insetInlineEnd x = "inset-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ insetInlineStart =: "value" ]
--
insetInlineStart :: MisoString -> Style
insetInlineStart x = "inset-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ justifyContent =: "value" ]
--
justifyContent :: MisoString -> Style
justifyContent x = "justify-content" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ justifyItems =: "value" ]
--
justifyItems :: MisoString -> Style
justifyItems x = "justify-items" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ justifySelf =: "value" ]
--
justifySelf :: MisoString -> Style
justifySelf x = "justify-self" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ left =: "value" ]
--
left :: MisoString -> Style
left x = "left" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ letterSpacing =: "value" ]
--
letterSpacing :: MisoString -> Style
letterSpacing x = "letter-spacing" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ linearCrossGravity =: "value" ]
--
linearCrossGravity :: MisoString -> Style
linearCrossGravity x = "linear-cross-gravity" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ linearDirection =: "value" ]
--
linearDirection :: MisoString -> Style
linearDirection x = "linear-direction" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ linearGravity =: "value" ]
--
linearGravity :: MisoString -> Style
linearGravity x = "linear-gravity" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ linearLayoutGravity =: "value" ]
--
linearLayoutGravity :: MisoString -> Style
linearLayoutGravity x = "linear-layout-gravity" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ linearWeight =: "value" ]
--
linearWeight :: MisoString -> Style
linearWeight x = "linear-weight" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ linearWeightSum =: "value" ]
--
linearWeightSum :: MisoString -> Style
linearWeightSum x = "linear-weight-sum" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ lineHeight =: "value" ]
--
lineHeight :: MisoString -> Style
lineHeight x = "line-height" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ marginBottom =: "value" ]
--
marginBottom :: MisoString -> Style
marginBottom x = "margin-bottom" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ marginInlineEnd =: "value" ]
--
marginInlineEnd :: MisoString -> Style
marginInlineEnd x = "margin-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ marginInlineStart =: "value" ]
--
marginInlineStart :: MisoString -> Style
marginInlineStart x = "margin-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ marginLeft =: "value" ]
--
marginLeft :: MisoString -> Style
marginLeft x = "margin-left" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ margin =: "value" ]
--
margin :: MisoString -> Style
margin x = "margin" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ marginRight =: "value" ]
--
marginRight :: MisoString -> Style
marginRight x = "margin-right" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ marginTop =: "value" ]
--
marginTop :: MisoString -> Style
marginTop x = "margin-top" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ maskImage =: "value" ]
--
maskImage :: MisoString -> Style
maskImage x = "mask-image" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ mask =: "value" ]
--
mask :: MisoString -> Style
mask x = "mask" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ maxHeight =: "value" ]
--
maxHeight :: MisoString -> Style
maxHeight x = "max-height" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ maxWidth =: "value" ]
--
maxWidth :: MisoString -> Style
maxWidth x = "max-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ minHeight =: "value" ]
--
minHeight :: MisoString -> Style
minHeight x = "min-height" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ minWidth =: "value" ]
--
minWidth :: MisoString -> Style
minWidth x = "min-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ opacity =: "value" ]
--
opacity :: MisoString -> Style
opacity x = "opacity" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ order =: "value" ]
--
order :: MisoString -> Style
order x = "order" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ overflow =: "value" ]
--
overflow :: MisoString -> Style
overflow x = "overflow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ overflowX =: "value" ]
--
overflowX :: MisoString -> Style
overflowX x = "overflow-x" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ overflowY =: "value" ]
--
overflowY :: MisoString -> Style
overflowY x = "overflow-y" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ paddingBottom =: "value" ]
--
paddingBottom :: MisoString -> Style
paddingBottom x = "padding-bottom" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ paddingInlineEnd =: "value" ]
--
paddingInlineEnd :: MisoString -> Style
paddingInlineEnd x = "padding-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ paddingInlineStart =: "value" ]
--
paddingInlineStart :: MisoString -> Style
paddingInlineStart x = "padding-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ paddingLeft =: "value" ]
--
paddingLeft :: MisoString -> Style
paddingLeft x = "padding-left" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ padding =: "value" ]
--
padding :: MisoString -> Style
padding x = "padding" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ paddingRight =: "value" ]
--
paddingRight :: MisoString -> Style
paddingRight x = "padding-right" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ paddingTop =: "value" ]
--
paddingTop :: MisoString -> Style
paddingTop x = "padding-top" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ perspective =: "value" ]
--
perspective :: MisoString -> Style
perspective x = "perspective" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ position =: "value" ]
--
position :: MisoString -> Style
position x = "position" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeAlignBottom =: "value" ]
--
relativeAlignBottom :: MisoString -> Style
relativeAlignBottom x = "relative-align-bottom" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeAlignInlineEnd =: "value" ]
--
relativeAlignInlineEnd :: MisoString -> Style
relativeAlignInlineEnd x = "relative-align-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeAlignInlineStart =: "value" ]
--
relativeAlignInlineStart :: MisoString -> Style
relativeAlignInlineStart x = "relative-align-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeAlignLeft =: "value" ]
--
relativeAlignLeft :: MisoString -> Style
relativeAlignLeft x = "relative-align-left" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeAlignRight =: "value" ]
--
relativeAlignRight :: MisoString -> Style
relativeAlignRight x = "relative-align-right" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeAlignTop =: "value" ]
--
relativeAlignTop :: MisoString -> Style
relativeAlignTop x = "relative-align-top" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeBottomOf =: "value" ]
--
relativeBottomOf :: MisoString -> Style
relativeBottomOf x = "relative-bottom-of" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeCenter =: "value" ]
--
relativeCenter :: MisoString -> Style
relativeCenter x = "relative-center" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeId =: "value" ]
--
relativeId :: MisoString -> Style
relativeId x = "relative-id" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeInlineEndOf =: "value" ]
--
relativeInlineEndOf :: MisoString -> Style
relativeInlineEndOf x = "relative-inline-end-of" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeInlineStartOf =: "value" ]
--
relativeInlineStartOf :: MisoString -> Style
relativeInlineStartOf x = "relative-inline-start-of" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeLayoutOnce =: "value" ]
--
relativeLayoutOnce :: MisoString -> Style
relativeLayoutOnce x = "relative-layout-once" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeLeftOf =: "value" ]
--
relativeLeftOf :: MisoString -> Style
relativeLeftOf x = "relative-left-of" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeRightOf =: "value" ]
--
relativeRightOf :: MisoString -> Style
relativeRightOf x = "relative-right-of" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ relativeTopOf =: "value" ]
--
relativeTopOf :: MisoString -> Style
relativeTopOf x = "relative-top-of" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ right =: "value" ]
--
right :: MisoString -> Style
right x = "right" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ rowGap =: "value" ]
--
rowGap :: MisoString -> Style
rowGap x = "row-gap" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textAlign =: "value" ]
--
textAlign :: MisoString -> Style
textAlign x = "text-align" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textDecoration =: "value" ]
--
textDecoration :: MisoString -> Style
textDecoration x = "text-decoration" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textIndent =: "value" ]
--
textIndent :: MisoString -> Style
textIndent x = "text-indent" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textOverflow =: "value" ]
--
textOverflow :: MisoString -> Style
textOverflow x = "text-overflow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textShadow =: "value" ]
--
textShadow :: MisoString -> Style
textShadow x = "text-shadow" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textStrokeColor =: "value" ]
--
textStrokeColor :: Color -> Style
textStrokeColor x = "text-stroke-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ textStroke =: "value" ]
--
textStroke :: MisoString -> Style
textStroke x = "text-stroke" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ textStrokeWidth =: "value" ]
--
textStrokeWidth :: MisoString -> Style
textStrokeWidth x = "text-stroke-width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ top =: "value" ]
--
top :: MisoString -> Style
top x = "top" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transform =: "value" ]
--
transform :: MisoString -> Style
transform x = "transform" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transformOrigin =: "value" ]
--
transformOrigin :: MisoString -> Style
transformOrigin x = "transform-origin" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transitionDelay =: "value" ]
--
transitionDelay :: MisoString -> Style
transitionDelay x = "transition-delay" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transitionDuration =: "value" ]
--
transitionDuration :: MisoString -> Style
transitionDuration x = "transition-duration" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transition =: "value" ]
--
transition :: MisoString -> Style
transition x = "transition" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transitionProperty =: "value" ]
--
transitionProperty :: MisoString -> Style
transitionProperty x = "transition-property" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ transitionTimingFunction =: "value" ]
--
transitionTimingFunction :: MisoString -> Style
transitionTimingFunction x = "transition-timing-function" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ verticalAlign =: "value" ]
--
verticalAlign :: MisoString -> Style
verticalAlign x = "vertical-align" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ visibility =: "value" ]
--
visibility :: MisoString -> Style
visibility x = "visibility" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ whiteSpace =: "value" ]
--
whiteSpace :: MisoString -> Style
whiteSpace x = "white-space" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ width =: "value" ]
--
width :: MisoString -> Style
width x = "width" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ wordBreak =: "value" ]
--
wordBreak :: MisoString -> Style
wordBreak x = "word-break" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ xAutoFontSize =: "value" ]
--
xAutoFontSize :: MisoString -> Style
xAutoFontSize x = "-x-auto-font-size" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ xAutoFontSizePresetSizes =: "value" ]
--
xAutoFontSizePresetSizes :: MisoString -> Style
xAutoFontSizePresetSizes x = "-x-auto-font-size-preset-sizes" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ xHandleColor =: "value" ]
--
xHandleColor :: Color -> Style
xHandleColor x = "-x-handle-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > styles_ [ xHandleSize =: "value" ]
--
xHandleSize :: MisoString -> Style
xHandleSize x = "-x-handle-size" =: x
-----------------------------------------------------------------------------
--
-- > styles_ [ zIndex =: "value" ]
--
zIndex :: MisoString -> Style
zIndex x = "z-index" =: x
-----------------------------------------------------------------------------