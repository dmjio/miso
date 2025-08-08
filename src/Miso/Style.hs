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
    module Miso.Style.Types
  -- *** Smart Constructor
  , style_
  , styleInline_
  , sheet_
  , selector_
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
  , cursor
  , direction
  , display
  , fill
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
  , stroke
  , strokeWidth
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
  -- *** Units
  , px
  , ppx
  , pct
  , pt
  , vw
  , vh
  , deg
  , turn
  , rad
  , rpx
  , rem
  , em
  , s
  , ms
  -- *** Misc
  , url
  -- *** Animation
  , keyframes_
  -- *** Media Queries
  , media_
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
import           Miso.String (MisoString)
import qualified Miso.String as MS
import           Miso.Style.Color
import           Miso.Style.Types
import           Miso.Property
import           Miso.Types (Attribute)
import qualified Miso.Types as MT
import           Miso.Util ((=:))
-----------------------------------------------------------------------------
import           Prelude hiding (filter, rem)
-----------------------------------------------------------------------------
pt :: Double -> MisoString
pt x = MS.ms x <> "pt"
-----------------------------------------------------------------------------
px :: Double -> MisoString
px x = MS.ms x <> "px"
-----------------------------------------------------------------------------
deg :: Double -> MisoString
deg x = MS.ms x <> "deg"
-----------------------------------------------------------------------------
turn :: Double -> MisoString
turn x = MS.ms x <> "turn"
-----------------------------------------------------------------------------
rad :: Double -> MisoString
rad x = MS.ms x <> "rad"
-----------------------------------------------------------------------------
rpx :: Double -> MisoString
rpx x = MS.ms x <> "rpx"
-----------------------------------------------------------------------------
rem :: Double -> MisoString
rem x = MS.ms x <> "rem"
-----------------------------------------------------------------------------
em :: Double -> MisoString
em x = MS.ms x <> "em"
-----------------------------------------------------------------------------
vh :: Double -> MisoString
vh x = MS.ms x <> "vh"
-----------------------------------------------------------------------------
vw :: Double -> MisoString
vw x = MS.ms x <> "vw"
-----------------------------------------------------------------------------
s :: Double -> MisoString
s x = MS.ms x <> "s"
-----------------------------------------------------------------------------
ms :: Double -> MisoString
ms x = MS.ms x <> "ms"
-----------------------------------------------------------------------------
url :: MisoString -> MisoString
url x = "url(" <> x <> ")"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/percentage
pct :: Double -> MisoString
pct x = MS.ms x <> "%"
-----------------------------------------------------------------------------
ppx :: Double -> MisoString
ppx x = MS.ms x <> "ppx"
-----------------------------------------------------------------------------
-- | Used when constructing a 'StyleSheet'
--
-- @
-- sheet_
--   [ selector_ ".name"
--     [ backgroundColor red
--     , alignContent "top"
--     ]
--   ]
-- @
--
selector_ :: MisoString -> [Style] -> Styles
selector_ k v = Styles (k,v)
-----------------------------------------------------------------------------
sheet_ :: [Styles] -> StyleSheet
sheet_ = StyleSheet
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
renderStyles :: Int -> Styles -> MisoString
renderStyles indent (Styles (sel,styles)) = MS.unlines
  [ sel <> " {" <> MS.replicate indent " "
  , MS.intercalate "\n"
        [ mconcat
          [ MS.replicate (indent + 2) " " <> k
          , " : "
          , v
          , ";"
          ]
        | (k,v) <- styles
        ]
  , MS.replicate indent " " <> "}"
  ]
renderStyles indent (KeyFrame name frames) = MS.intercalate " "
  [ "@keyframes"
  , name
  , "{\n"
  , MS.intercalate "\n  "
    [ renderStyles (indent + 2) (Styles frame)
    | frame <- frames
    ]
  , "}\n"
  ]
renderStyles indent (Media name frames) = MS.intercalate " "
  [ "@media"
  , name
  , "{\n"
  , MS.intercalate "\n  "
    [ renderStyles (indent + 2) (Styles frame)
    | frame <- frames
    ]
  , "}\n"
  ]
-----------------------------------------------------------------------------
-- | Render 'StyleSheet' as 'MisoString'
--
renderStyleSheet :: StyleSheet -> MisoString
renderStyleSheet styleSheet = MS.intercalate "\n"
  [ renderStyles 0 styles
  | styles <- getStyleSheet styleSheet
  ]
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes
--
-- @
-- testKeyFrame :: Styles
-- testKeyFrame = keyframes "slide-in"
--   [ "from" =:
--       [ transform "translateX(0%)"
--       ]
--   , "to" =:
--       [ transform "translateX(100%)"
--       , backgroundColor red
--       , backgroundSize "10px"
--       , backgroundRepeat "true"
--       ]
--   , pct 10 =:
--     [ "foo" =: "bar"
--     ]
--  ]
-- @
--
keyframes_ :: MisoString -> [(MisoString, [Style])] -> Styles
keyframes_ = KeyFrame
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/@media
--
-- @
-- media_ "screen and (min-width: 480px)"
--   [ "header" =:
--       [ height "auto"
--       ]
--   , "ul" =:
--       [ display "block"
--       ]
--   ]
-- @
--
media_ :: MisoString -> [(MisoString, [Style])] -> Styles
media_ = Media
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/align-content
--
alignContent :: MisoString -> Style
alignContent x = "align-content" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/align-items
--
alignItems :: MisoString -> Style
alignItems x = "align-items" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/align-self
--
alignSelf :: MisoString -> Style
alignSelf x = "align-self" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation-delay
--
animationDelay :: MisoString -> Style
animationDelay x = "animation-delay" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction
--
animationDirection :: MisoString -> Style
animationDirection x = "animation-direction" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation-duration
--
animationDuration :: MisoString -> Style
animationDuration x = "animation-duration" =: x
-----------------------------------------------------------------------------
-- | https://animation-mozilla.org/en-US/docs/Web/CSS/align-content/animation-fill-mode
--
animationFillMode :: MisoString -> Style
animationFillMode x = "animation-fill-mode" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count
--
animationIterationCount :: MisoString -> Style
animationIterationCount x = "animation-iteration-count" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation
--
animation :: MisoString -> Style
animation x = "animation" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name
--
animationName :: MisoString -> Style
animationName x = "animation-name" =: x
-----------------------------------------------------------------------------
-- |  https://developer.mozilla.org/en-US/docs/Web/CSS/animation-play-state
-- > style_ [ animationPlayState =: "value" ]
--
animationPlayState :: MisoString -> Style
animationPlayState x = "animation-play-state" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function
--
animationTimingFunction :: MisoString -> Style
animationTimingFunction x = "animation-timing-function" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio
--
aspectRatio :: MisoString -> Style
aspectRatio x = "aspect-ratio" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip
--
backgroundClip :: MisoString -> Style
backgroundClip x = "background-clip" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-color
--
backgroundColor :: Color -> Style
backgroundColor x = "background-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-image
--
backgroundImage :: MisoString -> Style
backgroundImage x = "background-image" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background
--
background :: MisoString -> Style
background x = "background" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-origin
--
backgroundOrigin :: MisoString -> Style
backgroundOrigin x = "background-origin" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-position
--
backgroundPosition :: MisoString -> Style
backgroundPosition x = "background-position" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat
--
backgroundRepeat :: MisoString -> Style
backgroundRepeat x = "background-repeat" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/background-size
--
backgroundSize :: MisoString -> Style
backgroundSize x = "background-size" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color
--
borderBottomColor :: Color -> Style
borderBottomColor x = "border-bottom-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-left-radius
--
borderBottomLeftRadius :: MisoString -> Style
borderBottomLeftRadius x = "border-bottom-left-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom
--
borderBottom :: MisoString -> Style
borderBottom x = "border-bottom" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-right-radius
--
borderBottomRightRadius :: MisoString -> Style
borderBottomRightRadius x = "border-bottom-right-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style
--
borderBottomStyle :: MisoString -> Style
borderBottomStyle x = "border-bottom-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width
--
borderBottomWidth :: MisoString -> Style
borderBottomWidth x = "border-bottom-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-color
--
borderColor :: Color -> Style
borderColor x = "border-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-end-radius
--
borderEndEndRadius :: MisoString -> Style
borderEndEndRadius x = "border-end-end-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-end-start-radius
--
borderEndStartRadius :: MisoString -> Style
borderEndStartRadius x = "border-end-start-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-color
--
borderInlineEndColor :: Color -> Style
borderInlineEndColor x = "border-inline-end-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-style
--
borderInlineEndStyle :: MisoString -> Style
borderInlineEndStyle x = "border-inline-end-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-width
--
borderInlineEndWidth :: MisoString -> Style
borderInlineEndWidth x = "border-inline-end-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-color
--
borderInlineStartColor :: Color -> Style
borderInlineStartColor x = "border-inline-start-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-style
--
borderInlineStartStyle :: MisoString -> Style
borderInlineStartStyle x = "border-inline-start-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width
--
borderInlineStartWidth :: MisoString -> Style
borderInlineStartWidth x = "border-inline-start-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color
--
borderLeftColor :: Color -> Style
borderLeftColor x = "border-left-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-left
--
borderLeft :: MisoString -> Style
borderLeft x = "border-left" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style
--
borderLeftStyle :: MisoString -> Style
borderLeftStyle x = "border-left-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width
--
borderLeftWidth :: MisoString -> Style
borderLeftWidth x = "border-left-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border
--
border :: MisoString -> Style
border x = "border" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius
--
borderRadius :: MisoString -> Style
borderRadius x = "border-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color
--
borderRightColor :: Color -> Style
borderRightColor x = "border-right-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-right
--
borderRight :: MisoString -> Style
borderRight x = "border-right" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style
--
borderRightStyle :: MisoString -> Style
borderRightStyle x = "border-right-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width
--
borderRightWidth :: MisoString -> Style
borderRightWidth x = "border-right-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-start-end-radius
--
borderStartEndRadius :: MisoString -> Style
borderStartEndRadius x = "border-start-end-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-start-start-radius
--
borderStartStartRadius :: MisoString -> Style
borderStartStartRadius x = "border-start-start-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-style
--
borderStyle :: MisoString -> Style
borderStyle x = "border-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color
--
borderTopColor :: Color -> Style
borderTopColor x = "border-top-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-left-radius
--
borderTopLeftRadius :: MisoString -> Style
borderTopLeftRadius x = "border-top-left-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-top
--
borderTop :: MisoString -> Style
borderTop x = "border-top" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-right-radius
--
borderTopRightRadius :: MisoString -> Style
borderTopRightRadius x = "border-top-right-radius" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-style
--
borderTopStyle :: MisoString -> Style
borderTopStyle x = "border-top-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width
--
borderTopWidth :: MisoString -> Style
borderTopWidth x = "border-top-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/border-width
--
borderWidth :: MisoString -> Style
borderWidth x = "border-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/bottom
--
bottom :: MisoString -> Style
bottom x = "bottom" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow
--
boxShadow :: MisoString -> Style
boxShadow x = "box-shadow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing
--
boxSizing :: MisoString -> Style
boxSizing x = "box-sizing" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path
--
clipPath :: MisoString -> Style
clipPath x = "clip-path" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/color
--
color :: Color -> Style
color x = "color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap
--
columnGap :: MisoString -> Style
columnGap x = "column-gap" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/css-variable
--
cssVariable :: MisoString -> Style
cssVariable x = "css-variable" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/direction
--
direction :: MisoString -> Style
direction x = "direction" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/display
--
display :: MisoString -> Style
display x = "display" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/fill
--
fill :: MisoString -> Style
fill x = "fill" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/filter
--
filter :: MisoString -> Style
filter x = "filter" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis
--
flexBasis :: MisoString -> Style
flexBasis x = "flex-basis" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction
--
flexDirection :: MisoString -> Style
flexDirection x = "flex-direction" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex-flow
--
flexFlow :: MisoString -> Style
flexFlow x = "flex-flow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow
--
flexGrow :: MisoString -> Style
flexGrow x = "flex-grow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex
--
flex :: MisoString -> Style
flex x = "flex" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink
--
flexShrink :: MisoString -> Style
flexShrink x = "flex-shrink" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap
--
flexWrap :: MisoString -> Style
flexWrap x = "flex-wrap" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/font-family
--
fontFamily :: MisoString -> Style
fontFamily x = "font-family" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/font-size
--
fontSize :: MisoString -> Style
fontSize x = "font-size" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/font-style
--
fontStyle :: MisoString -> Style
fontStyle x = "font-style" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight
--
fontWeight :: MisoString -> Style
fontWeight x = "font-weight" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
--
cursor :: MisoString -> Style
cursor x = "cursor" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/gap
--
gap :: MisoString -> Style
gap x = "gap" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-columns
--
gridAutoColumns :: MisoString -> Style
gridAutoColumns x = "grid-auto-columns" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-flow
--
gridAutoFlow :: MisoString -> Style
gridAutoFlow x = "grid-auto-flow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-rows
--
gridAutoRows :: MisoString -> Style
gridAutoRows x = "grid-auto-rows" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-end
--
gridColumnEnd :: MisoString -> Style
gridColumnEnd x = "grid-column-end" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-span
--
gridColumnSpan :: MisoString -> Style
gridColumnSpan x = "grid-column-span" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start
--
gridColumnStart :: MisoString -> Style
gridColumnStart x = "grid-column-start" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-end
--
gridRowEnd :: MisoString -> Style
gridRowEnd x = "grid-row-end" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-span
--
gridRowSpan :: MisoString -> Style
gridRowSpan x = "grid-row-span" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-start
--
gridRowStart :: MisoString -> Style
gridRowStart x = "grid-row-start" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns
--
gridTemplateColumns :: MisoString -> Style
gridTemplateColumns x = "grid-template-columns" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-rows
--
gridTemplateRows :: MisoString -> Style
gridTemplateRows x = "grid-template-rows" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/height
--
height :: MisoString -> Style
height x = "height" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/image-rendering
--
imageRendering :: MisoString -> Style
imageRendering x = "image-rendering" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline-end
--
insetInlineEnd :: MisoString -> Style
insetInlineEnd x = "inset-inline-end" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline-start
--
insetInlineStart :: MisoString -> Style
insetInlineStart x = "inset-inline-start" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content
--
justifyContent :: MisoString -> Style
justifyContent x = "justify-content" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items
--
justifyItems :: MisoString -> Style
justifyItems x = "justify-items" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self
--
justifySelf :: MisoString -> Style
justifySelf x = "justify-self" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/left
--
left :: MisoString -> Style
left x = "left" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing
--
letterSpacing :: MisoString -> Style
letterSpacing x = "letter-spacing" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/linear-cross-gravity
--
linearCrossGravity :: MisoString -> Style
linearCrossGravity x = "linear-cross-gravity" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/linear-direction
--
linearDirection :: MisoString -> Style
linearDirection x = "linear-direction" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/linear-gravity
--
linearGravity :: MisoString -> Style
linearGravity x = "linear-gravity" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/linear-layout-gravity
--
linearLayoutGravity :: MisoString -> Style
linearLayoutGravity x = "linear-layout-gravity" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/linear-weight
--
linearWeight :: MisoString -> Style
linearWeight x = "linear-weight" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/linear-weight-sum
--
linearWeightSum :: MisoString -> Style
linearWeightSum x = "linear-weight-sum" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/line-height
--
lineHeight :: MisoString -> Style
lineHeight x = "line-height" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom
--
marginBottom :: MisoString -> Style
marginBottom x = "margin-bottom" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end
--
marginInlineEnd :: MisoString -> Style
marginInlineEnd x = "margin-inline-end" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-start
--
marginInlineStart :: MisoString -> Style
marginInlineStart x = "margin-inline-start" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left
--
marginLeft :: MisoString -> Style
marginLeft x = "margin-left" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin
--
margin :: MisoString -> Style
margin x = "margin" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right
--
marginRight :: MisoString -> Style
marginRight x = "margin-right" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top
--
marginTop :: MisoString -> Style
marginTop x = "margin-top" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/mask-image
--
maskImage :: MisoString -> Style
maskImage x = "mask-image" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/mask
--
mask :: MisoString -> Style
mask x = "mask" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/max-height
--
maxHeight :: MisoString -> Style
maxHeight x = "max-height" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/max-width
--
maxWidth :: MisoString -> Style
maxWidth x = "max-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/min-height
--
minHeight :: MisoString -> Style
minHeight x = "min-height" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/min-width
--
minWidth :: MisoString -> Style
minWidth x = "min-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/opacity
--
opacity :: MisoString -> Style
opacity x = "opacity" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/order
--
order :: MisoString -> Style
order x = "order" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/overflow
--
overflow :: MisoString -> Style
overflow x = "overflow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x
--
overflowX :: MisoString -> Style
overflowX x = "overflow-x" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y
--
overflowY :: MisoString -> Style
overflowY x = "overflow-y" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom
--
paddingBottom :: MisoString -> Style
paddingBottom x = "padding-bottom" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end
--
paddingInlineEnd :: MisoString -> Style
paddingInlineEnd x = "padding-inline-end" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start
--
paddingInlineStart :: MisoString -> Style
paddingInlineStart x = "padding-inline-start" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left
--
paddingLeft :: MisoString -> Style
paddingLeft x = "padding-left" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding
--
padding :: MisoString -> Style
padding x = "padding" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right
--
paddingRight :: MisoString -> Style
paddingRight x = "padding-right" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top
--
paddingTop :: MisoString -> Style
paddingTop x = "padding-top" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/perspective
--
perspective :: MisoString -> Style
perspective x = "perspective" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/position
--
position :: MisoString -> Style
position x = "position" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-align-bottom
--
relativeAlignBottom :: MisoString -> Style
relativeAlignBottom x = "relative-align-bottom" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-align-inline-end
--
relativeAlignInlineEnd :: MisoString -> Style
relativeAlignInlineEnd x = "relative-align-inline-end" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-align-inline-start
--
relativeAlignInlineStart :: MisoString -> Style
relativeAlignInlineStart x = "relative-align-inline-start" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-align-left
--
relativeAlignLeft :: MisoString -> Style
relativeAlignLeft x = "relative-align-left" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-align-right
--
relativeAlignRight :: MisoString -> Style
relativeAlignRight x = "relative-align-right" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-align-top
--
relativeAlignTop :: MisoString -> Style
relativeAlignTop x = "relative-align-top" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-bottom-of
--
relativeBottomOf :: MisoString -> Style
relativeBottomOf x = "relative-bottom-of" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-center
--
relativeCenter :: MisoString -> Style
relativeCenter x = "relative-center" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-id
--
relativeId :: MisoString -> Style
relativeId x = "relative-id" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-inline-end-of
--
relativeInlineEndOf :: MisoString -> Style
relativeInlineEndOf x = "relative-inline-end-of" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-inline-start-of
--
relativeInlineStartOf :: MisoString -> Style
relativeInlineStartOf x = "relative-inline-start-of" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-layout-once
--
relativeLayoutOnce :: MisoString -> Style
relativeLayoutOnce x = "relative-layout-once" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-left-of
--
relativeLeftOf :: MisoString -> Style
relativeLeftOf x = "relative-left-of" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-right-of
--
relativeRightOf :: MisoString -> Style
relativeRightOf x = "relative-right-of" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/relative-top-of
--
relativeTopOf :: MisoString -> Style
relativeTopOf x = "relative-top-of" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/right
--
right :: MisoString -> Style
right x = "right" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap
--
rowGap :: MisoString -> Style
rowGap x = "row-gap" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/stroke
--
stroke :: MisoString -> Style
stroke x = "stroke" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/stroke-width
--
strokeWidth :: MisoString -> Style
strokeWidth x = "stroke-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-align
--
textAlign :: MisoString -> Style
textAlign x = "text-align" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration
--
textDecoration :: MisoString -> Style
textDecoration x = "text-decoration" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent
--
textIndent :: MisoString -> Style
textIndent x = "text-indent" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow
--
textOverflow :: MisoString -> Style
textOverflow x = "text-overflow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow
--
textShadow :: MisoString -> Style
textShadow x = "text-shadow" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-stroke-color
--
textStrokeColor :: Color -> Style
textStrokeColor x = "text-stroke-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-stroke
--
textStroke :: MisoString -> Style
textStroke x = "text-stroke" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/text-stroke-width
--
textStrokeWidth :: MisoString -> Style
textStrokeWidth x = "text-stroke-width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/top
--
top :: MisoString -> Style
top x = "top" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transform
--
transform :: MisoString -> Style
transform x = "transform" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin
--
transformOrigin :: MisoString -> Style
transformOrigin x = "transform-origin" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transition-delay
--
transitionDelay :: MisoString -> Style
transitionDelay x = "transition-delay" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transition-duration
--
transitionDuration :: MisoString -> Style
transitionDuration x = "transition-duration" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transition
--
transition :: MisoString -> Style
transition x = "transition" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transition-property
--
transitionProperty :: MisoString -> Style
transitionProperty x = "transition-property" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function
--
transitionTimingFunction :: MisoString -> Style
transitionTimingFunction x = "transition-timing-function" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align
--
verticalAlign :: MisoString -> Style
verticalAlign x = "vertical-align" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/visibility
--
visibility :: MisoString -> Style
visibility x = "visibility" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/white-space
--
whiteSpace :: MisoString -> Style
whiteSpace x = "white-space" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/width
--
width :: MisoString -> Style
width x = "width" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/word-break
--
wordBreak :: MisoString -> Style
wordBreak x = "word-break" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/xAutoFontSize
--
xAutoFontSize :: MisoString -> Style
xAutoFontSize x = "-x-auto-font-size" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/xAutoFontSizePresetSizes
--
xAutoFontSizePresetSizes :: MisoString -> Style
xAutoFontSizePresetSizes x = "-x-auto-font-size-preset-sizes" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/xHandleColor
--
xHandleColor :: Color -> Style
xHandleColor x = "-x-handle-color" =: renderColor x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/xHandleSize
--
xHandleSize :: MisoString -> Style
xHandleSize x = "-x-handle-size" =: x
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/CSS/z-index
--
zIndex :: MisoString -> Style
zIndex x = "z-index" =: x
-----------------------------------------------------------------------------
