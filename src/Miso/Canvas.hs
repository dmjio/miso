-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Canvas
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- [Canvas Example](https://canvas.haskell-miso.org)
--
----------------------------------------------------------------------------
module Miso.Canvas
  ( -- * Types
    Canvas
  , CanvasContext2D
  , Pattern            (..)
  , Gradient           (..)
  , ImageData          (..)
  , LineCapType        (..)
  , PatternType        (..)
  , LineJoinType       (..)
  , DirectionType      (..)
  , TextAlignType      (..)
  , TextBaselineType   (..)
  , CompositeOperation (..)
  , StyleArg           (..)
  , Coord
   -- * Property
  , canvas
  , canvas_
    -- * API
  , globalCompositeOperation
  , clearRect
  , fillRect
  , strokeRect
  , beginPath
  , closePath
  , moveTo
  , lineTo
  , fill
  , rect
  , stroke
  , bezierCurveTo
  , arc
  , arcTo
  , quadraticCurveTo
  , direction
  , fillText
  , font
  , strokeText
  , textAlign
  , textBaseline
  , addColorStop
  , createLinearGradient
  , createPattern
  , createRadialGradient
  , fillStyle
  , lineCap
  , lineJoin
  , lineWidth
  , miterLimit
  , shadowBlur
  , shadowColor
  , shadowOffsetX
  , shadowOffsetY
  , strokeStyle
  , scale
  , rotate
  , translate
  , transform
  , setTransform
  , drawImage
  , drawImage'
  , createImageData
  , getImageData
  , setImageData
  , height
  , width
  , putImageData
  , globalAlpha
  , clip
  , save
  , restore
  -- * Smart constructors
  , gradient
  , pattern_
  , color
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
import           Language.Javascript.JSaddle ( JSVal, (#), fromJSVal, MakeArgs (..)
                                             , (<#), toJSVal, (!), fromJSValUnchecked
                                             , liftJSM, FromJSVal, Object (..)
                                             , ToJSVal, MakeObject, (<##)
                                             )
-----------------------------------------------------------------------------
import qualified Miso.FFI as FFI
import           Miso.FFI (Image)
import           Miso.Types
import           Miso.CSS (Color, renderColor)
-----------------------------------------------------------------------------
-- | Another variant of canvas, this is not specialized to 'ReaderT'. This is
-- useful when building applications w/ three.js, or other libraries where
-- explicit context is not necessary.
canvas_
  :: forall model action canvasState
   . (FromJSVal canvasState, ToJSVal canvasState)
  => [ Attribute action ]
  -> (DOMRef -> JSM canvasState)
  -- ^ Init function, takes 'DOMRef' as arg, returns canvas init. state.
  -> (canvasState -> JSM ())
  -- ^ Callback to render graphics using this canvas' context, takes init state as arg.
  -> View model action
canvas_ attributes initialize_ draw_ = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute action ]
    attrs = initCallback : drawCallack : attributes

    initCallback :: Attribute action
    initCallback = Event $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "onCreated") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          initialState <- initialize_ domRef
          FFI.set "state" initialState (Object domRef)

    drawCallack :: Attribute action
    drawCallack = Event $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "draw") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          state <- fromJSValUnchecked =<< domRef ! ("state" :: MisoString)
          draw_ state
-----------------------------------------------------------------------------
-- | Element for drawing on a [\<canvas\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas).
-- This function abstracts over the context and interpret callback,
-- including dimension ("2d" or "3d") canvas.
canvas
  :: forall model action canvasState
   . (FromJSVal canvasState, ToJSVal canvasState)
  => [ Attribute action ]
  -> (DOMRef -> Canvas canvasState)
  -- ^ Init function, takes 'DOMRef' as arg, returns canvas init. state.
  -> (canvasState -> Canvas ())
  -- ^ Callback to render graphics using this canvas' context, takes init state as arg.
  -> View model action
canvas attributes initialize draw = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute action ]
    attrs = initCallback : drawCallack : attributes

    initCallback :: Attribute action
    initCallback = Event $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "onCreated") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
          initialState <- runReaderT (initialize domRef) ctx
          FFI.set "state" initialState (Object domRef)

    drawCallack :: Attribute action
    drawCallack = Event $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "draw") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          jval <- domRef ! ("state" :: MisoString)
          initialState <- fromJSValUnchecked jval
          ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
          runReaderT (draw initialState) ctx
-----------------------------------------------------------------------------
-- | Various patterns used in the canvas API
data PatternType = Repeat | RepeatX | RepeatY | NoRepeat
-----------------------------------------------------------------------------
instance ToJSVal PatternType where
  toJSVal = toJSVal . renderPattern
-----------------------------------------------------------------------------
instance FromJSVal PatternType where
  fromJSVal pat =
    fromJSValUnchecked @MisoString pat >>= \case
      "repeat" -> pure (Just Repeat)
      "repeat-x" -> pure (Just RepeatX)
      "repeat-y" -> pure (Just RepeatY)
      "no-repeat" -> pure (Just NoRepeat)
      _ -> pure Nothing
-----------------------------------------------------------------------------
-- | Color, Gradient or Pattern styling
data StyleArg
  = ColorArg Color
  | GradientArg Gradient
  | PatternArg Pattern
-----------------------------------------------------------------------------
-- | Smart constructor for 'Color' when using 'StyleArg'
color :: Color -> StyleArg
color = ColorArg
-----------------------------------------------------------------------------
-- | Smart constructor for t'Gradient' when using t'StyleArg'
gradient :: Gradient -> StyleArg
gradient = GradientArg
-----------------------------------------------------------------------------
-- | Smart constructor for t'Pattern' when using t'StyleArg'
pattern_ :: Pattern -> StyleArg
pattern_ = PatternArg
-----------------------------------------------------------------------------
-- | Renders a t'StyleArg' to a 'JSVal'
renderStyleArg :: StyleArg -> JSM JSVal
renderStyleArg (ColorArg c)    = toJSVal (renderColor c)
renderStyleArg (GradientArg g) = toJSVal g
renderStyleArg (PatternArg p)  = toJSVal p
-----------------------------------------------------------------------------
instance MakeArgs StyleArg where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal StyleArg where
  toJSVal = toJSVal . renderStyleArg
-----------------------------------------------------------------------------
-- | Pretty-prints a t'PatternType' as 'Miso.String.MisoString'
renderPattern :: PatternType -> MisoString
renderPattern Repeat   = "repeat"
renderPattern RepeatX  = "repeat-x"
renderPattern RepeatY  = "repeat-y"
renderPattern NoRepeat = "no-repeat"
-----------------------------------------------------------------------------
-- | [LineCap](https://www.w3schools.com/tags/canvas_linecap.asp)
data LineCapType
  = LineCapButt
  | LineCapRound
  | LineCapSquare
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance MakeArgs LineCapType where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal LineCapType where
  toJSVal = toJSVal . renderLineCapType
-----------------------------------------------------------------------------
-- | Pretty-printing for 'LineCapType'
renderLineCapType :: LineCapType -> MisoString
renderLineCapType LineCapButt = "butt"
renderLineCapType LineCapRound = "round"
renderLineCapType LineCapSquare = "square"
-----------------------------------------------------------------------------
-- | [LineJoin](https://www.w3schools.com/tags/canvas_linejoin.asp)
data LineJoinType = LineJoinBevel | LineJoinRound | LineJoinMiter
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance MakeArgs LineJoinType where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal LineJoinType where
  toJSVal = toJSVal . renderLineJoinType
-----------------------------------------------------------------------------
-- | Pretty-print a 'LineJoinType'
renderLineJoinType :: LineJoinType -> MisoString
renderLineJoinType LineJoinBevel = "bevel"
renderLineJoinType LineJoinRound = "round"
renderLineJoinType LineJoinMiter = "miter"
-----------------------------------------------------------------------------
-- | Left-to-right, right-to-left, or inherit direction type.
data DirectionType = LTR | RTL | Inherit
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance MakeArgs DirectionType where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal DirectionType where
  toJSVal = toJSVal . renderDirectionType
-----------------------------------------------------------------------------
-- | Pretty-printing for 'DirectionType'
renderDirectionType :: DirectionType -> MisoString
renderDirectionType LTR = "ltr"
renderDirectionType RTL = "rtl"
renderDirectionType Inherit = "inherit"
-----------------------------------------------------------------------------
-- | Text alignment type
data TextAlignType
  = TextAlignCenter
  | TextAlignEnd
  | TextAlignLeft
  | TextAlignRight
  | TextAlignStart
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance MakeArgs TextAlignType where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal TextAlignType where
  toJSVal = toJSVal . renderTextAlignType
-----------------------------------------------------------------------------
-- | Pretty-print 'TextAlignType'
renderTextAlignType :: TextAlignType -> MisoString
renderTextAlignType TextAlignCenter = "center"
renderTextAlignType TextAlignEnd    = "end"
renderTextAlignType TextAlignLeft   = "left"
renderTextAlignType TextAlignRight  = "right"
renderTextAlignType TextAlignStart  = "start"
-----------------------------------------------------------------------------
-- | TextBaselineType
data TextBaselineType
  = TextBaselineAlphabetic
  | TextBaselineTop
  | TextBaselineHanging
  | TextBaselineMiddle
  | TextBaselineIdeographic
  | TextBaselineBottom
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance MakeArgs TextBaselineType where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal TextBaselineType where
  toJSVal = toJSVal . renderTextBaselineType
-----------------------------------------------------------------------------
-- | Pretty-printing for 'TextBaselineType'
renderTextBaselineType :: TextBaselineType -> MisoString
renderTextBaselineType TextBaselineAlphabetic = "alphabetic"
renderTextBaselineType TextBaselineTop = "top"
renderTextBaselineType TextBaselineHanging = "hanging"
renderTextBaselineType TextBaselineMiddle = "middle"
renderTextBaselineType TextBaselineIdeographic = "ideographic"
renderTextBaselineType TextBaselineBottom = "bottom"
-----------------------------------------------------------------------------
-- | CompositeOperation
data CompositeOperation
  = SourceOver
  | SourceAtop
  | SourceIn
  | SourceOut
  | DestinationOver
  | DestinationAtop
  | DestinationIn
  | DestinationOut
  | Lighter
  | Copy
  | Xor
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance MakeArgs CompositeOperation where
  makeArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal CompositeOperation where
  toJSVal = toJSVal . renderCompositeOperation
-----------------------------------------------------------------------------
-- | Pretty-print a 'CompositeOperation'
renderCompositeOperation :: CompositeOperation -> MisoString
renderCompositeOperation SourceOver      = "source-over"
renderCompositeOperation SourceAtop      = "source-atop"
renderCompositeOperation SourceIn        = "source-in"
renderCompositeOperation SourceOut       = "source-out"
renderCompositeOperation DestinationOver = "destination-over"
renderCompositeOperation DestinationAtop = "destination-atop"
renderCompositeOperation DestinationIn   = "destination-in"
renderCompositeOperation DestinationOut  = "destination-out"
renderCompositeOperation Lighter         = "lighter"
renderCompositeOperation Copy            = "copy"
renderCompositeOperation Xor             = "xor"
-----------------------------------------------------------------------------
-- | Type used to hold a canvas Pattern
newtype Pattern = Pattern JSVal deriving (ToJSVal)
-----------------------------------------------------------------------------
instance FromJSVal Pattern where
  fromJSVal = pure . pure . Pattern
-----------------------------------------------------------------------------
-- | Type used to hold a Gradient
newtype Gradient = Gradient JSVal deriving (ToJSVal)
-----------------------------------------------------------------------------
instance FromJSVal Gradient where
  fromJSVal = pure . pure . Gradient
-----------------------------------------------------------------------------
-- | Type used to hold t'ImageData'
newtype ImageData = ImageData JSVal deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
instance MakeArgs ImageData where
  makeArgs args = (:[]) <$> toJSVal args
-----------------------------------------------------------------------------
instance FromJSVal ImageData where
  fromJSVal = pure . pure . ImageData
-----------------------------------------------------------------------------
-- | An (x,y) coordinate.
type Coord = (Double, Double)
-----------------------------------------------------------------------------
-- | The canvas t'CanvasContext2D'
type CanvasContext2D = JSVal
-----------------------------------------------------------------------------
call :: (FromJSVal a, MakeArgs args) => MisoString -> args -> Canvas a
call name arg = do
  ctx <- ask
  liftJSM $ fromJSValUnchecked =<< do
    ctx # name $ arg
-----------------------------------------------------------------------------
set :: MakeArgs args => MisoString -> args -> Canvas ()
set name arg = do
  ctx <- ask
  liftJSM (ctx <# name $ makeArgs arg)
-----------------------------------------------------------------------------
-- | DSL for expressing operations on 'canvas_'
type Canvas a = ReaderT CanvasContext2D JSM a
-----------------------------------------------------------------------------
-- | [ctx.globalCompositeOperation = "source-over"](https://www.w3schools.com/tags/canvas_globalcompositeoperation.asp)
globalCompositeOperation :: CompositeOperation -> Canvas ()
globalCompositeOperation = set "globalCompositeOperation"
-----------------------------------------------------------------------------
-- | [ctx.clearRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_clearrect.asp)
clearRect :: (Double, Double, Double, Double) -> Canvas ()
clearRect = call "clearRect"
-----------------------------------------------------------------------------
-- | [ctx.fillRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_fillrect.asp)
fillRect :: (Double, Double, Double, Double) -> Canvas ()
fillRect = call "fillRect"
-----------------------------------------------------------------------------
-- | [ctx.strokeRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_strokerect.asp)
strokeRect :: (Double, Double, Double, Double) -> Canvas ()
strokeRect = call "strokeRect"
-----------------------------------------------------------------------------
-- | [ctx.beginPath()](https://www.w3schools.com/tags/canvas_beginpath.asp)
beginPath :: () -> Canvas ()
beginPath = call "beginPath"
-----------------------------------------------------------------------------
-- | [ctx.closePath()](https://www.w3schools.com/tags/canvas_closepath.asp)
closePath :: () -> Canvas ()
closePath = call "closePath"
-----------------------------------------------------------------------------
-- | [ctx.moveTo(x,y)](https://www.w3schools.com/tags/canvas_moveto.asp)
moveTo :: Coord -> Canvas ()
moveTo = call "moveTo"
-----------------------------------------------------------------------------
-- | [ctx.lineTo(x,y)](https://www.w3schools.com/tags/canvas_lineto.asp)
lineTo :: Coord -> Canvas ()
lineTo = call "lineTo"
-----------------------------------------------------------------------------
-- | [ctx.fill()](https://www.w3schools.com/tags/canvas_fill.asp)
fill :: () -> Canvas ()
fill = call "fill"
-----------------------------------------------------------------------------
-- | [ctx.rect(x,y,width,height)](https://www.w3schools.com/tags/canvas_rect.asp)
rect :: (Double, Double, Double, Double) -> Canvas ()
rect = call "rect"
-----------------------------------------------------------------------------
-- | [ctx.stroke()](https://www.w3schools.com/tags/canvas_stroke.asp)
stroke :: () -> Canvas ()
stroke = call "stroke"
-----------------------------------------------------------------------------
-- | [ctx.bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y)](https://www.w3schools.com/tags/canvas_beziercurveto.asp)
bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
bezierCurveTo = call "bezierCurveTo"
-----------------------------------------------------------------------------
-- | [context.arc(x, y, r, sAngle, eAngle, counterclockwise)](https://www.w3schools.com/tags/canvas_arc.asp)
arc :: (Double, Double, Double, Double, Double) -> Canvas ()
arc = call "arc"
-----------------------------------------------------------------------------
-- | [context.arcTo(x1, y1, x2, y2, r)](https://www.w3schools.com/tags/canvas_arcto.asp)
arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
arcTo = call "arcTo"
-----------------------------------------------------------------------------
-- | [context.quadraticCurveTo(cpx,cpy,x,y)](https://www.w3schools.com/tags/canvas_quadraticcurveto.asp)
quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
quadraticCurveTo = call "quadraticCurveTo"
-----------------------------------------------------------------------------
-- | [context.direction = "ltr"](https://www.w3schools.com/tags/canvas_direction.asp)
direction :: DirectionType -> Canvas ()
direction = set "direction"
-----------------------------------------------------------------------------
-- | [context.fillText(text,x,y)](https://www.w3schools.com/tags/canvas_filltext.asp)
fillText :: (MisoString, Double, Double) -> Canvas ()
fillText = call "fillText"
-----------------------------------------------------------------------------
-- | [context.font = "italic small-caps bold 12px arial"](https://www.w3schools.com/tags/canvas_font.asp)
font :: MisoString -> Canvas ()
font = set "font"
-----------------------------------------------------------------------------
-- | [ctx.strokeText()](https://www.w3schools.com/tags/canvas_stroketext.asp)
strokeText :: (MisoString, Double, Double) -> Canvas ()
strokeText = call "strokeText"
-----------------------------------------------------------------------------
-- | [ctx.textAlign = "start"](https://www.w3schools.com/tags/canvas_textalign.asp)
textAlign :: TextAlignType -> Canvas ()
textAlign = set "textAlign"
-----------------------------------------------------------------------------
-- | [ctx.textBaseline = "top"](https://www.w3schools.com/tags/canvas_textBaseLine.asp)
textBaseline :: TextBaselineType -> Canvas ()
textBaseline = set "textBaseline"
-----------------------------------------------------------------------------
-- | [gradient.addColorStop(stop,color)](https://www.w3schools.com/tags/canvas_addcolorstop.asp)
addColorStop :: (Gradient, Double, Color) -> Canvas ()
addColorStop = call "addColorStop"
-----------------------------------------------------------------------------
-- | [ctx.createLinearGradient(x0,y0,x1,y1)](https://www.w3schools.com/tags/canvas_createlineargradient.asp)
createLinearGradient :: (Double, Double, Double, Double) -> Canvas Gradient
createLinearGradient = call "createLinearGradient"
-----------------------------------------------------------------------------
-- | [ctx.createPattern(image, "repeat")](https://www.w3schools.com/tags/canvas_createpattern.asp)
createPattern :: (Image, PatternType) -> Canvas Pattern
createPattern = call "createPattern"
-----------------------------------------------------------------------------
-- | [ctx.createRadialGradient(x0,y0,r0,x1,y1,r1)](https://www.w3schools.com/tags/canvas_createradialgradient.asp)
createRadialGradient :: (Double,Double,Double,Double,Double,Double) -> Canvas Gradient
createRadialGradient = call "createRadialGradient"
-----------------------------------------------------------------------------
-- | [ctx.fillStyle = "red"](https://www.w3schools.com/tags/canvas_fillstyle.asp)
fillStyle :: StyleArg -> Canvas ()
fillStyle = set "fillStyle"
-----------------------------------------------------------------------------
-- | [ctx.lineCap = "butt"](https://www.w3schools.com/tags/canvas_lineCap.asp)
lineCap :: LineCapType -> Canvas ()
lineCap = set "lineCap"
-----------------------------------------------------------------------------
-- | [ctx.lineJoin = "bevel"](https://www.w3schools.com/tags/canvas_lineJoin.asp)
lineJoin :: LineJoinType -> Canvas ()
lineJoin = set "lineJoin"
-----------------------------------------------------------------------------
-- | [ctx.lineWidth = 10](https://www.w3schools.com/tags/canvas_lineWidth.asp)
lineWidth :: Double -> Canvas ()
lineWidth = set "lineWidth"
-----------------------------------------------------------------------------
-- | [ctx.miterLimit = 10](https://www.w3schools.com/tags/canvas_miterLimit.asp)
miterLimit :: Double -> Canvas ()
miterLimit = set "miterLimit"
-----------------------------------------------------------------------------
-- | [ctx.shadowBlur = 10](https://www.w3schools.com/tags/canvas_shadowBlur.asp)
shadowBlur :: Double -> Canvas ()
shadowBlur = set "shadowBlur"
-----------------------------------------------------------------------------
-- | [ctx.shadowColor = "red"](https://www.w3schools.com/tags/canvas_shadowColor.asp)
shadowColor :: Color -> Canvas ()
shadowColor = set "shadowColor"
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetX = 20](https://www.w3schools.com/tags/canvas_shadowOffsetX.asp)
shadowOffsetX :: Double -> Canvas ()
shadowOffsetX = set "shadowOffsetX"
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetY = 20](https://www.w3schools.com/tags/canvas_shadowOffsetY.asp)
shadowOffsetY :: Double -> Canvas ()
shadowOffsetY = set "shadowOffsetY"
-----------------------------------------------------------------------------
-- | [ctx.strokeStyle = "red"](https://www.w3schools.com/tags/canvas_strokeStyle.asp)
strokeStyle :: StyleArg -> Canvas ()
strokeStyle = set "strokeStyle"
-----------------------------------------------------------------------------
-- | [ctx.scale(width,height)](https://www.w3schools.com/tags/canvas_scale.asp)
scale :: (Double, Double) -> Canvas ()
scale = call "scale"
-----------------------------------------------------------------------------
-- | [ctx.rotate(angle)](https://www.w3schools.com/tags/canvas_rotate.asp)
rotate :: Double -> Canvas ()
rotate = call "rotate"
-----------------------------------------------------------------------------
-- | [ctx.translate(angle)](https://www.w3schools.com/tags/canvas_translate.asp)
translate :: Coord -> Canvas ()
translate = call "translate"
-----------------------------------------------------------------------------
-- | [ctx.transform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_transform.asp)
transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
transform = call "transform"
-----------------------------------------------------------------------------
-- | [ctx.setTransform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_setTransform.asp)
setTransform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
setTransform = call "setTransform"
----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage :: (Image, Double, Double) -> Canvas ()
drawImage = call "drawImage"
-----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage' :: (Image, Double, Double, Double, Double) -> Canvas ()
drawImage' = call "drawImage"
-----------------------------------------------------------------------------
-- | [ctx.createImageData(width,height)](https://www.w3schools.com/tags/canvas_createImageData.asp)
createImageData :: (Double, Double) -> Canvas ImageData
createImageData = call "createImageData"
-----------------------------------------------------------------------------
-- | [ctx.getImageData(w,x,y,z)](https://www.w3schools.com/tags/canvas_getImageData.asp)
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData = call "getImageData"
-----------------------------------------------------------------------------
-- | [imageData.data[index] = 255](https://www.w3schools.com/tags/canvas_imagedata_data.asp)
setImageData :: (ImageData, Int, Double) -> Canvas ()
setImageData (imgData, index, value) = liftJSM $ do
   o <- imgData ! ("data" :: MisoString)
   (o <## index) value
-----------------------------------------------------------------------------
-- | [imageData.height](https://www.w3schools.com/tags/canvas_imagedata_height.asp)
height :: ImageData -> Canvas Double
height (ImageData imgData) = liftJSM $ do
  fromJSValUnchecked =<< imgData ! ("height" :: MisoString)
-----------------------------------------------------------------------------
-- | [imageData.width](https://www.w3schools.com/tags/canvas_imagedata_width.asp)
width :: ImageData -> Canvas Double
width (ImageData imgData) = liftJSM $ do
  fromJSValUnchecked =<< imgData ! ("width" :: MisoString)
-----------------------------------------------------------------------------
-- | [ctx.putImageData(imageData,x,y)](https://www.w3schools.com/tags/canvas_putImageData.asp)
putImageData :: (ImageData, Double, Double) -> Canvas ()
putImageData = call "putImageData"
-----------------------------------------------------------------------------
-- | [ctx.globalAlpha = 0.2](https://www.w3schools.com/tags/canvas_globalAlpha.asp)
globalAlpha :: Double -> Canvas ()
globalAlpha = set "globalAlpha"
-----------------------------------------------------------------------------
-- | [ctx.clip()](https://www.w3schools.com/tags/canvas_clip.asp)
clip :: () -> Canvas ()
clip = call "clip"
-----------------------------------------------------------------------------
-- | [ctx.save()](https://www.w3schools.com/tags/canvas_save.asp)
save :: () -> Canvas ()
save = call "save"
-----------------------------------------------------------------------------
-- | [ctx.restore()](https://www.w3schools.com/tags/canvas_restore.asp)
restore :: () -> Canvas ()
restore = call "restore"
-----------------------------------------------------------------------------
