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
-- div_ [ style_  [ "background" =: "red" ] ] []
-- @
--
(=:) :: k -> v -> (k, v)
k =: v = (k,v)
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Type for a CSS 'Style'
--
type Style = (MisoString, MisoString)
-----------------------------------------------------------------------------
-- | Type for a @Map@ of CSS 'Style'. Used with @StyleSheet@.
-- It maps CSS properties to their values.
type Styles = Map MisoString MisoString
-----------------------------------------------------------------------------
-- | Type for a CSS style on native.
-- Internally it maps From CSS selectors to 'Styles'.
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
-- | Renders a 'Styles' to a 'MisoString'
renderStyles :: Styles -> MisoString
renderStyles m = MS.unlines
  [ mconcat [ spaced, k, ":", v, ";" ]
  | (k,v) <- M.toList m
  , let spaced = " "
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
-- > style_ [ alignContent =: "value" ]
--
alignContent :: MisoString -> Style
alignContent x = "align-content" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ alignItems =: "value" ]
--
alignItems :: MisoString -> Style
alignItems x = "align-items" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ alignSelf =: "value" ]
--
alignSelf :: MisoString -> Style
alignSelf x = "align-self" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationDelay =: "value" ]
--
animationDelay :: MisoString -> Style
animationDelay x = "animation-delay" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationDirection =: "value" ]
--
animationDirection :: MisoString -> Style
animationDirection x = "animation-direction" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationDuration =: "value" ]
--
animationDuration :: MisoString -> Style
animationDuration x = "animation-duration" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationFillMode =: "value" ]
--
animationFillMode :: MisoString -> Style
animationFillMode x = "animation-fill-mode" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationIterationCount =: "value" ]
--
animationIterationCount :: MisoString -> Style
animationIterationCount x = "animation-iteration-count" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animation =: "value" ]
--
animation :: MisoString -> Style
animation x = "animation" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationName =: "value" ]
--
animationName :: MisoString -> Style
animationName x = "animation-name" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationPlayState =: "value" ]
--
animationPlayState :: MisoString -> Style
animationPlayState x = "animation-play-state" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ animationTimingFunction =: "value" ]
--
animationTimingFunction :: MisoString -> Style
animationTimingFunction x = "animation-timing-function" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ aspectRatio =: "value" ]
--
aspectRatio :: MisoString -> Style
aspectRatio x = "aspect-ratio" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundClip =: "value" ]
--
backgroundClip :: MisoString -> Style
backgroundClip x = "background-clip" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundColor =: "value" ]
--
backgroundColor :: Color -> Style
backgroundColor x = "background-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundImage =: "value" ]
--
backgroundImage :: MisoString -> Style
backgroundImage x = "background-image" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ background =: "value" ]
--
background :: MisoString -> Style
background x = "background" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundOrigin =: "value" ]
--
backgroundOrigin :: MisoString -> Style
backgroundOrigin x = "background-origin" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundPosition =: "value" ]
--
backgroundPosition :: MisoString -> Style
backgroundPosition x = "background-position" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundRepeat =: "value" ]
--
backgroundRepeat :: MisoString -> Style
backgroundRepeat x = "background-repeat" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ backgroundSize =: "value" ]
--
backgroundSize :: MisoString -> Style
backgroundSize x = "background-size" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderBottomColor =: "value" ]
--
borderBottomColor :: Color -> Style
borderBottomColor x = "border-bottom-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderBottomLeftRadius =: "value" ]
--
borderBottomLeftRadius :: MisoString -> Style
borderBottomLeftRadius x = "border-bottom-left-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderBottom =: "value" ]
--
borderBottom :: MisoString -> Style
borderBottom x = "border-bottom" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderBottomRightRadius =: "value" ]
--
borderBottomRightRadius :: MisoString -> Style
borderBottomRightRadius x = "border-bottom-right-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderBottomStyle =: "value" ]
--
borderBottomStyle :: MisoString -> Style
borderBottomStyle x = "border-bottom-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderBottomWidth =: "value" ]
--
borderBottomWidth :: MisoString -> Style
borderBottomWidth x = "border-bottom-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderColor =: "value" ]
--
borderColor :: Color -> Style
borderColor x = "border-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderEndEndRadius =: "value" ]
--
borderEndEndRadius :: MisoString -> Style
borderEndEndRadius x = "border-end-end-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderEndStartRadius =: "value" ]
--
borderEndStartRadius :: MisoString -> Style
borderEndStartRadius x = "border-end-start-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderInlineEndColor =: "value" ]
--
borderInlineEndColor :: Color -> Style
borderInlineEndColor x = "border-inline-end-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderInlineEndStyle =: "value" ]
--
borderInlineEndStyle :: MisoString -> Style
borderInlineEndStyle x = "border-inline-end-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderInlineEndWidth =: "value" ]
--
borderInlineEndWidth :: MisoString -> Style
borderInlineEndWidth x = "border-inline-end-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderInlineStartColor =: "value" ]
--
borderInlineStartColor :: Color -> Style
borderInlineStartColor x = "border-inline-start-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderInlineStartStyle =: "value" ]
--
borderInlineStartStyle :: MisoString -> Style
borderInlineStartStyle x = "border-inline-start-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderInlineStartWidth =: "value" ]
--
borderInlineStartWidth :: MisoString -> Style
borderInlineStartWidth x = "border-inline-start-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderLeftColor =: "value" ]
--
borderLeftColor :: Color -> Style
borderLeftColor x = "border-left-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderLeft =: "value" ]
--
borderLeft :: MisoString -> Style
borderLeft x = "border-left" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderLeftStyle =: "value" ]
--
borderLeftStyle :: MisoString -> Style
borderLeftStyle x = "border-left-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderLeftWidth =: "value" ]
--
borderLeftWidth :: MisoString -> Style
borderLeftWidth x = "border-left-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ border =: "value" ]
--
border :: MisoString -> Style
border x = "border" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderRadius =: "value" ]
--
borderRadius :: MisoString -> Style
borderRadius x = "border-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderRightColor =: "value" ]
--
borderRightColor :: Color -> Style
borderRightColor x = "border-right-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderRight =: "value" ]
--
borderRight :: MisoString -> Style
borderRight x = "border-right" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderRightStyle =: "value" ]
--
borderRightStyle :: MisoString -> Style
borderRightStyle x = "border-right-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderRightWidth =: "value" ]
--
borderRightWidth :: MisoString -> Style
borderRightWidth x = "border-right-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderStartEndRadius =: "value" ]
--
borderStartEndRadius :: MisoString -> Style
borderStartEndRadius x = "border-start-end-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderStartStartRadius =: "value" ]
--
borderStartStartRadius :: MisoString -> Style
borderStartStartRadius x = "border-start-start-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderStyle =: "value" ]
--
borderStyle :: MisoString -> Style
borderStyle x = "border-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderTopColor =: "value" ]
--
borderTopColor :: Color -> Style
borderTopColor x = "border-top-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ borderTopLeftRadius =: "value" ]
--
borderTopLeftRadius :: MisoString -> Style
borderTopLeftRadius x = "border-top-left-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderTop =: "value" ]
--
borderTop :: MisoString -> Style
borderTop x = "border-top" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderTopRightRadius =: "value" ]
--
borderTopRightRadius :: MisoString -> Style
borderTopRightRadius x = "border-top-right-radius" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderTopStyle =: "value" ]
--
borderTopStyle :: MisoString -> Style
borderTopStyle x = "border-top-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderTopWidth =: "value" ]
--
borderTopWidth :: MisoString -> Style
borderTopWidth x = "border-top-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ borderWidth =: "value" ]
--
borderWidth :: MisoString -> Style
borderWidth x = "border-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ bottom =: "value" ]
--
bottom :: MisoString -> Style
bottom x = "bottom" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ boxShadow =: "value" ]
--
boxShadow :: MisoString -> Style
boxShadow x = "box-shadow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ boxSizing =: "value" ]
--
boxSizing :: MisoString -> Style
boxSizing x = "box-sizing" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ clipPath =: "value" ]
--
clipPath :: MisoString -> Style
clipPath x = "clip-path" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ color =: "value" ]
--
color :: Color -> Style
color x = "color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ columnGap =: "value" ]
--
columnGap :: MisoString -> Style
columnGap x = "column-gap" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ cssVariable =: "value" ]
--
cssVariable :: MisoString -> Style
cssVariable x = "css-variable" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ direction =: "value" ]
--
direction :: MisoString -> Style
direction x = "direction" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ display =: "value" ]
--
display :: MisoString -> Style
display x = "display" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ filter =: "value" ]
--
filter :: MisoString -> Style
filter x = "filter" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flexBasis =: "value" ]
--
flexBasis :: MisoString -> Style
flexBasis x = "flex-basis" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flexDirection =: "value" ]
--
flexDirection :: MisoString -> Style
flexDirection x = "flex-direction" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flexFlow =: "value" ]
--
flexFlow :: MisoString -> Style
flexFlow x = "flex-flow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flexGrow =: "value" ]
--
flexGrow :: MisoString -> Style
flexGrow x = "flex-grow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flex =: "value" ]
--
flex :: MisoString -> Style
flex x = "flex" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flexShrink =: "value" ]
--
flexShrink :: MisoString -> Style
flexShrink x = "flex-shrink" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ flexWrap =: "value" ]
--
flexWrap :: MisoString -> Style
flexWrap x = "flex-wrap" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ fontFamily =: "value" ]
--
fontFamily :: MisoString -> Style
fontFamily x = "font-family" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ fontSize =: "value" ]
--
fontSize :: MisoString -> Style
fontSize x = "font-size" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ fontStyle =: "value" ]
--
fontStyle :: MisoString -> Style
fontStyle x = "font-style" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ fontWeight =: "value" ]
--
fontWeight :: MisoString -> Style
fontWeight x = "font-weight" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gap =: "value" ]
--
gap :: MisoString -> Style
gap x = "gap" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridAutoColumns =: "value" ]
--
gridAutoColumns :: MisoString -> Style
gridAutoColumns x = "grid-auto-columns" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridAutoFlow =: "value" ]
--
gridAutoFlow :: MisoString -> Style
gridAutoFlow x = "grid-auto-flow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridAutoRows =: "value" ]
--
gridAutoRows :: MisoString -> Style
gridAutoRows x = "grid-auto-rows" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridColumnEnd =: "value" ]
--
gridColumnEnd :: MisoString -> Style
gridColumnEnd x = "grid-column-end" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridColumnSpan =: "value" ]
--
gridColumnSpan :: MisoString -> Style
gridColumnSpan x = "grid-column-span" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridColumnStart =: "value" ]
--
gridColumnStart :: MisoString -> Style
gridColumnStart x = "grid-column-start" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridRowEnd =: "value" ]
--
gridRowEnd :: MisoString -> Style
gridRowEnd x = "grid-row-end" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridRowSpan =: "value" ]
--
gridRowSpan :: MisoString -> Style
gridRowSpan x = "grid-row-span" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridRowStart =: "value" ]
--
gridRowStart :: MisoString -> Style
gridRowStart x = "grid-row-start" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridTemplateColumns =: "value" ]
--
gridTemplateColumns :: MisoString -> Style
gridTemplateColumns x = "grid-template-columns" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ gridTemplateRows =: "value" ]
--
gridTemplateRows :: MisoString -> Style
gridTemplateRows x = "grid-template-rows" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ height =: "value" ]
--
height :: MisoString -> Style
height x = "height" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ imageRendering =: "value" ]
--
imageRendering :: MisoString -> Style
imageRendering x = "image-rendering" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ insetInlineEnd =: "value" ]
--
insetInlineEnd :: MisoString -> Style
insetInlineEnd x = "inset-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ insetInlineStart =: "value" ]
--
insetInlineStart :: MisoString -> Style
insetInlineStart x = "inset-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ justifyContent =: "value" ]
--
justifyContent :: MisoString -> Style
justifyContent x = "justify-content" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ justifyItems =: "value" ]
--
justifyItems :: MisoString -> Style
justifyItems x = "justify-items" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ justifySelf =: "value" ]
--
justifySelf :: MisoString -> Style
justifySelf x = "justify-self" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ left =: "value" ]
--
left :: MisoString -> Style
left x = "left" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ letterSpacing =: "value" ]
--
letterSpacing :: MisoString -> Style
letterSpacing x = "letter-spacing" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ linearCrossGravity =: "value" ]
--
linearCrossGravity :: MisoString -> Style
linearCrossGravity x = "linear-cross-gravity" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ linearDirection =: "value" ]
--
linearDirection :: MisoString -> Style
linearDirection x = "linear-direction" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ linearGravity =: "value" ]
--
linearGravity :: MisoString -> Style
linearGravity x = "linear-gravity" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ linearLayoutGravity =: "value" ]
--
linearLayoutGravity :: MisoString -> Style
linearLayoutGravity x = "linear-layout-gravity" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ linearWeight =: "value" ]
--
linearWeight :: MisoString -> Style
linearWeight x = "linear-weight" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ linearWeightSum =: "value" ]
--
linearWeightSum :: MisoString -> Style
linearWeightSum x = "linear-weight-sum" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ lineHeight =: "value" ]
--
lineHeight :: MisoString -> Style
lineHeight x = "line-height" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ marginBottom =: "value" ]
--
marginBottom :: MisoString -> Style
marginBottom x = "margin-bottom" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ marginInlineEnd =: "value" ]
--
marginInlineEnd :: MisoString -> Style
marginInlineEnd x = "margin-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ marginInlineStart =: "value" ]
--
marginInlineStart :: MisoString -> Style
marginInlineStart x = "margin-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ marginLeft =: "value" ]
--
marginLeft :: MisoString -> Style
marginLeft x = "margin-left" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ margin =: "value" ]
--
margin :: MisoString -> Style
margin x = "margin" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ marginRight =: "value" ]
--
marginRight :: MisoString -> Style
marginRight x = "margin-right" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ marginTop =: "value" ]
--
marginTop :: MisoString -> Style
marginTop x = "margin-top" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ maskImage =: "value" ]
--
maskImage :: MisoString -> Style
maskImage x = "mask-image" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ mask =: "value" ]
--
mask :: MisoString -> Style
mask x = "mask" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ maxHeight =: "value" ]
--
maxHeight :: MisoString -> Style
maxHeight x = "max-height" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ maxWidth =: "value" ]
--
maxWidth :: MisoString -> Style
maxWidth x = "max-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ minHeight =: "value" ]
--
minHeight :: MisoString -> Style
minHeight x = "min-height" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ minWidth =: "value" ]
--
minWidth :: MisoString -> Style
minWidth x = "min-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ opacity =: "value" ]
--
opacity :: MisoString -> Style
opacity x = "opacity" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ order =: "value" ]
--
order :: MisoString -> Style
order x = "order" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ overflow =: "value" ]
--
overflow :: MisoString -> Style
overflow x = "overflow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ overflowX =: "value" ]
--
overflowX :: MisoString -> Style
overflowX x = "overflow-x" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ overflowY =: "value" ]
--
overflowY :: MisoString -> Style
overflowY x = "overflow-y" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ paddingBottom =: "value" ]
--
paddingBottom :: MisoString -> Style
paddingBottom x = "padding-bottom" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ paddingInlineEnd =: "value" ]
--
paddingInlineEnd :: MisoString -> Style
paddingInlineEnd x = "padding-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ paddingInlineStart =: "value" ]
--
paddingInlineStart :: MisoString -> Style
paddingInlineStart x = "padding-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ paddingLeft =: "value" ]
--
paddingLeft :: MisoString -> Style
paddingLeft x = "padding-left" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ padding =: "value" ]
--
padding :: MisoString -> Style
padding x = "padding" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ paddingRight =: "value" ]
--
paddingRight :: MisoString -> Style
paddingRight x = "padding-right" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ paddingTop =: "value" ]
--
paddingTop :: MisoString -> Style
paddingTop x = "padding-top" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ perspective =: "value" ]
--
perspective :: MisoString -> Style
perspective x = "perspective" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ position =: "value" ]
--
position :: MisoString -> Style
position x = "position" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeAlignBottom =: "value" ]
--
relativeAlignBottom :: MisoString -> Style
relativeAlignBottom x = "relative-align-bottom" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeAlignInlineEnd =: "value" ]
--
relativeAlignInlineEnd :: MisoString -> Style
relativeAlignInlineEnd x = "relative-align-inline-end" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeAlignInlineStart =: "value" ]
--
relativeAlignInlineStart :: MisoString -> Style
relativeAlignInlineStart x = "relative-align-inline-start" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeAlignLeft =: "value" ]
--
relativeAlignLeft :: MisoString -> Style
relativeAlignLeft x = "relative-align-left" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeAlignRight =: "value" ]
--
relativeAlignRight :: MisoString -> Style
relativeAlignRight x = "relative-align-right" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeAlignTop =: "value" ]
--
relativeAlignTop :: MisoString -> Style
relativeAlignTop x = "relative-align-top" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeBottomOf =: "value" ]
--
relativeBottomOf :: MisoString -> Style
relativeBottomOf x = "relative-bottom-of" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeCenter =: "value" ]
--
relativeCenter :: MisoString -> Style
relativeCenter x = "relative-center" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeId =: "value" ]
--
relativeId :: MisoString -> Style
relativeId x = "relative-id" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeInlineEndOf =: "value" ]
--
relativeInlineEndOf :: MisoString -> Style
relativeInlineEndOf x = "relative-inline-end-of" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeInlineStartOf =: "value" ]
--
relativeInlineStartOf :: MisoString -> Style
relativeInlineStartOf x = "relative-inline-start-of" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeLayoutOnce =: "value" ]
--
relativeLayoutOnce :: MisoString -> Style
relativeLayoutOnce x = "relative-layout-once" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeLeftOf =: "value" ]
--
relativeLeftOf :: MisoString -> Style
relativeLeftOf x = "relative-left-of" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeRightOf =: "value" ]
--
relativeRightOf :: MisoString -> Style
relativeRightOf x = "relative-right-of" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ relativeTopOf =: "value" ]
--
relativeTopOf :: MisoString -> Style
relativeTopOf x = "relative-top-of" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ right =: "value" ]
--
right :: MisoString -> Style
right x = "right" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ rowGap =: "value" ]
--
rowGap :: MisoString -> Style
rowGap x = "row-gap" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textAlign =: "value" ]
--
textAlign :: MisoString -> Style
textAlign x = "text-align" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textDecoration =: "value" ]
--
textDecoration :: MisoString -> Style
textDecoration x = "text-decoration" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textIndent =: "value" ]
--
textIndent :: MisoString -> Style
textIndent x = "text-indent" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textOverflow =: "value" ]
--
textOverflow :: MisoString -> Style
textOverflow x = "text-overflow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textShadow =: "value" ]
--
textShadow :: MisoString -> Style
textShadow x = "text-shadow" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textStrokeColor =: "value" ]
--
textStrokeColor :: Color -> Style
textStrokeColor x = "text-stroke-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ textStroke =: "value" ]
--
textStroke :: MisoString -> Style
textStroke x = "text-stroke" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ textStrokeWidth =: "value" ]
--
textStrokeWidth :: MisoString -> Style
textStrokeWidth x = "text-stroke-width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ top =: "value" ]
--
top :: MisoString -> Style
top x = "top" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transform =: "value" ]
--
transform :: MisoString -> Style
transform x = "transform" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transformOrigin =: "value" ]
--
transformOrigin :: MisoString -> Style
transformOrigin x = "transform-origin" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transitionDelay =: "value" ]
--
transitionDelay :: MisoString -> Style
transitionDelay x = "transition-delay" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transitionDuration =: "value" ]
--
transitionDuration :: MisoString -> Style
transitionDuration x = "transition-duration" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transition =: "value" ]
--
transition :: MisoString -> Style
transition x = "transition" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transitionProperty =: "value" ]
--
transitionProperty :: MisoString -> Style
transitionProperty x = "transition-property" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ transitionTimingFunction =: "value" ]
--
transitionTimingFunction :: MisoString -> Style
transitionTimingFunction x = "transition-timing-function" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ verticalAlign =: "value" ]
--
verticalAlign :: MisoString -> Style
verticalAlign x = "vertical-align" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ visibility =: "value" ]
--
visibility :: MisoString -> Style
visibility x = "visibility" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ whiteSpace =: "value" ]
--
whiteSpace :: MisoString -> Style
whiteSpace x = "white-space" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ width =: "value" ]
--
width :: MisoString -> Style
width x = "width" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ wordBreak =: "value" ]
--
wordBreak :: MisoString -> Style
wordBreak x = "word-break" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ xAutoFontSize =: "value" ]
--
xAutoFontSize :: MisoString -> Style
xAutoFontSize x = "-x-auto-font-size" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ xAutoFontSizePresetSizes =: "value" ]
--
xAutoFontSizePresetSizes :: MisoString -> Style
xAutoFontSizePresetSizes x = "-x-auto-font-size-preset-sizes" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ xHandleColor =: "value" ]
--
xHandleColor :: Color -> Style
xHandleColor x = "-x-handle-color" =: renderColor x
-----------------------------------------------------------------------------
--
-- > style_ [ xHandleSize =: "value" ]
--
xHandleSize :: MisoString -> Style
xHandleSize x = "-x-handle-size" =: x
-----------------------------------------------------------------------------
--
-- > style_ [ zIndex =: "value" ]
--
zIndex :: MisoString -> Style
zIndex x = "z-index" =: x
-----------------------------------------------------------------------------