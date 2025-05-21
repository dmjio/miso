-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
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
-- @
-- -----------------------------------------------------------------------------
-- import Miso
-- import qualified Miso.Canvas as Canvas
-- import Miso.Canvas
-- -----------------------------------------------------------------------------
-- baseUrl :: MisoString
-- baseUrl = "https://7b40c187-5088-4a99-9118-37d20a2f875e.mdnplay.dev/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_animations/"
-- -----------------------------------------------------------------------------
-- main :: IO ()
-- main =
--   run $ do
--     sun \<- newImage (baseUrl \<> "canvas_sun.png")
--     moon \<- newImage (baseUrl \<> "canvas_moon.png")
--     earth \<- newImage (baseUrl \<> "canvas_earth.png")
--     startComponent (app sun moon earth) { initialAction = Just GetTime }
--   where
--     app sun moon earth = defaultComponent (0.0, 0.0) updateModel (view_ sun moon earth)
--     view_ sun moon earth m =
--       div_
--       [ id_ "canvas grid" ]
--       [ Canvas.canvas_
--         [ id_ "canvas"
--         , width_ "300"
--         , height_ "300"
--         ] (canvasDraw sun moon earth m n)
--       | n \<- [ 1 :: Int .. 4 ]
--       ]
-- -----------------------------------------------------------------------------
-- canvasDraw :: Image -> Image -> Image -> (Double, Double) -> Int -> Canvas ()
-- canvasDraw sun moon earth (millis', secs') n = do
--    let
--      secs = secs' + fromIntegral n
--      millis = millis' + fromIntegral n
--    globalCompositeOperation DestinationOver
--    clearRect (0, 0, 300, 300)
--    fillStyle $ Canvas.color (rgba 0 0 0 0.6)
--    strokeStyle $ Canvas.color (rgba 0 153 255 0.4)
--    save ()
--    translate (150, 150)
--    rotate ((((2 * pi) \/ 60) * secs) + (((2 * pi) \/ 60000) * millis))
--    translate (105,0)
--    fillRect (0, -12, 50, 24)
--    drawImage (earth, -12, -12)
--    save ()
--    rotate ((((2 * pi) \/ 6) * secs) + (((2 * pi) \/ 6000) * millis))
--    translate (0, 28.5)
--    drawImage (moon, -3.5, -3.5)
--    replicateM_ 2 (restore ())
--    beginPath ()
--    arc (150, 150, 105, 0, pi * 2)
--    stroke ()
--    drawImage' (sun, 0, 0, 300, 300)
-- @
----------------------------------------------------------------------------
module Miso.Canvas
  ( -- * Types
    Canvas             (..)
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
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad (void, liftM, ap, liftM2)
import           Data.Kind (Type)
import           Language.Javascript.JSaddle ( JSM, JSVal, (#), fromJSVal
                                             , (<#), toJSVal, (!)
                                             , liftJSM, Function
                                             , ToJSVal, MakeObject, (<##)
#ifndef GHCJS_BOTH
                                             , MonadJSM(..)
#endif
                                             )
-----------------------------------------------------------------------------
import qualified Miso.FFI as FFI
import           Miso.FFI (Image)
import           Miso.Html.Types
import           Miso.Style (Color, renderColor)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Element for drawing on a [\<canvas\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas).
-- This function abstracts over the context and interpret callback,
-- including dimension ("2d" or "3d") canvas.
canvas
  :: forall action
   . [ Attribute action ]
  -> JSM Function -- ^ Callback to render graphics using this canvas' context
  -> View action
canvas attributes callback = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute action ]
    attrs = flip (:) attributes $ Event $ \_ obj _ _ ->
      flip (FFI.set "draw") obj =<< toJSVal =<< callback
-----------------------------------------------------------------------------
-- | Element for drawing on a [\<canvas\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas),
-- includes a 'Canvas' DSL.
-- Specialized to ["2d"](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext#2d) canvas.
canvas_
  :: [ Attribute action ]
  -> Canvas a
  -> View action
canvas_ attributes canvas' =
  canvas attributes $
    FFI.syncCallback1 $ \domRef -> do
      ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
      void (interpret ctx canvas')
-----------------------------------------------------------------------------
data PatternType = Repeat | RepeatX | RepeatY | NoRepeat
-----------------------------------------------------------------------------
data StyleArg
  = ColorArg Color
  | GradientArg Gradient
  | PatternArg Pattern
-----------------------------------------------------------------------------
color :: Color -> StyleArg
color = ColorArg
-----------------------------------------------------------------------------
gradient :: Gradient -> StyleArg
gradient = GradientArg
-----------------------------------------------------------------------------
pattern_ :: Pattern -> StyleArg
pattern_ = PatternArg
-----------------------------------------------------------------------------
renderStyleArg :: StyleArg -> JSM JSVal
renderStyleArg (ColorArg c)    = toJSVal (renderColor c)
renderStyleArg (GradientArg g) = toJSVal g
renderStyleArg (PatternArg p)  = toJSVal p
-----------------------------------------------------------------------------
renderPattern :: PatternType -> MisoString
renderPattern Repeat   = "repeat"
renderPattern RepeatX  = "repeat-x"
renderPattern RepeatY  = "repeat-y"
renderPattern NoRepeat = "no-repeat"
-----------------------------------------------------------------------------
data LineCapType
  = LineCapButt
  | LineCapRound
  | LineCapSquare
  deriving (Show, Eq)
-----------------------------------------------------------------------------
renderLineCapType :: LineCapType -> MisoString
renderLineCapType LineCapButt = "butt"
renderLineCapType LineCapRound = "round"
renderLineCapType LineCapSquare = "square"
-----------------------------------------------------------------------------
data LineJoinType = LineJoinBevel | LineJoinRound | LineJoinMiter
  deriving (Show, Eq)
-----------------------------------------------------------------------------
renderLineJoinType :: LineJoinType -> MisoString
renderLineJoinType LineJoinBevel = "bevel"
renderLineJoinType LineJoinRound = "round"
renderLineJoinType LineJoinMiter = "miter"
-----------------------------------------------------------------------------
data DirectionType = LTR | RTL | Inherit
  deriving (Show, Eq)
-----------------------------------------------------------------------------
renderDirectionType :: DirectionType -> MisoString
renderDirectionType LTR = "ltr"
renderDirectionType RTL = "rtl"
renderDirectionType Inherit = "inherit"
-----------------------------------------------------------------------------
data TextAlignType
  = TextAlignCenter
  | TextAlignEnd
  | TextAlignLeft
  | TextAlignRight
  | TextAlignStart
  deriving (Show, Eq)
-----------------------------------------------------------------------------
renderTextAlignType :: TextAlignType -> MisoString
renderTextAlignType TextAlignCenter = "center"
renderTextAlignType TextAlignEnd    = "end"
renderTextAlignType TextAlignLeft   = "left"
renderTextAlignType TextAlignRight  = "right"
renderTextAlignType TextAlignStart  = "start"
-----------------------------------------------------------------------------
data TextBaselineType
  = TextBaselineAlphabetic
  | TextBaselineTop
  | TextBaselineHanging
  | TextBaselineMiddle
  | TextBaselineIdeographic
  | TextBaselineBottom
  deriving (Show, Eq)
-----------------------------------------------------------------------------
renderTextBaselineType :: TextBaselineType -> MisoString
renderTextBaselineType TextBaselineAlphabetic = "alphabetic"
renderTextBaselineType TextBaselineTop = "top"
renderTextBaselineType TextBaselineHanging = "hanging"
renderTextBaselineType TextBaselineMiddle = "middle"
renderTextBaselineType TextBaselineIdeographic = "ideographic"
renderTextBaselineType TextBaselineBottom = "bottom"
-----------------------------------------------------------------------------
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
newtype Pattern = Pattern JSVal deriving (ToJSVal)
-----------------------------------------------------------------------------
newtype Gradient = Gradient JSVal deriving (ToJSVal)
-----------------------------------------------------------------------------
newtype ImageData = ImageData JSVal deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
type Coord = (Double, Double)
-----------------------------------------------------------------------------
-- | DSL for expressing operations on 'canvas_'
data Canvas :: Type -> Type where
  Bind :: Canvas a -> (a -> Canvas b) -> Canvas b
  Pure :: a -> Canvas a
  LiftIO :: IO a -> Canvas a
  LiftJSM :: JSM a -> Canvas a
  IsPointInPath :: Coord -> Canvas Bool
  MeasureText :: MisoString -> Canvas Double
  ClearRect :: (Double, Double, Double, Double) -> Canvas ()
  FillRect :: (Double, Double, Double, Double) -> Canvas ()
  StrokeRect :: (Double, Double, Double, Double) -> Canvas ()
  BeginPath :: Canvas ()
  ClosePath :: Canvas ()
  MoveTo :: (Double, Double) -> Canvas ()
  LineTo :: (Double, Double) -> Canvas ()
  Fill :: Canvas ()
  Rect :: (Double, Double, Double, Double) -> Canvas ()
  Stroke :: Canvas ()
  BezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
  Arc :: (Double, Double, Double, Double, Double) -> Canvas ()
  ArcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
  QuadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
  Direction :: DirectionType -> Canvas ()
  FillText :: (MisoString, Double, Double) -> Canvas ()
  Font :: MisoString -> Canvas ()
  StrokeText :: (MisoString, Double, Double) -> Canvas ()
  TextAlign :: TextAlignType -> Canvas ()
  TextBaseline :: TextBaselineType -> Canvas ()
  AddColorStop :: Gradient -> Double -> Color -> Canvas ()
  CreateLinearGradient :: (Double, Double, Double, Double) -> Canvas Gradient
  CreatePattern :: Image -> PatternType -> Canvas Pattern
  CreateRadialGradient :: (Double, Double, Double, Double, Double, Double) -> Canvas Gradient
  FillStyle :: StyleArg -> Canvas ()
  LineCap :: LineCapType -> Canvas ()
  LineJoin :: LineJoinType -> Canvas ()
  LineWidth :: Double -> Canvas ()
  MiterLimit :: Double -> Canvas ()
  ShadowBlur :: Double -> Canvas ()
  ShadowColor :: Color -> Canvas ()
  ShadowOffsetX :: Double -> Canvas ()
  ShadowOffsetY :: Double -> Canvas ()
  StrokeStyle :: StyleArg -> Canvas ()
  Scale :: (Double, Double) -> Canvas ()
  Rotate :: Double -> Canvas ()
  Translate :: Coord -> Canvas ()
  Transform :: (Double,Double,Double,Double,Double,Double) -> Canvas ()
  SetTransform :: (Double,Double,Double,Double,Double,Double) -> Canvas ()
  DrawImage :: (Image,Double,Double) -> Canvas ()
  DrawImage' :: (Image, Double, Double, Double, Double) -> Canvas ()
  CreateImageData :: (Double, Double) -> Canvas ImageData
  GetImageData :: (Double, Double, Double, Double) -> Canvas ImageData
  SetImageData :: ImageData -> Int -> Double -> Canvas ()
  ImageDataHeight :: ImageData -> Canvas Double
  ImageDataWidth :: ImageData -> Canvas Double
  PutImageData :: (ImageData, Double, Double) -> Canvas ()
  GlobalAlpha :: Double -> Canvas ()
  GlobalCompositeOperation :: CompositeOperation -> Canvas ()
  Clip :: Canvas ()
  Save :: Canvas ()
  Restore :: Canvas ()
-----------------------------------------------------------------------------
instance MonadIO Canvas where
  liftIO = LiftIO
-----------------------------------------------------------------------------
#ifndef GHCJS_BOTH
instance MonadJSM Canvas where
  liftJSM' = LiftJSM
#endif
-----------------------------------------------------------------------------
instance Monad Canvas where
  (>>=) = Bind
  return = pure
-----------------------------------------------------------------------------
instance Applicative Canvas where
  (<*>) = ap
  pure = Pure
-----------------------------------------------------------------------------
instance Functor Canvas where
  fmap = liftM
-----------------------------------------------------------------------------
instance Semigroup a => Semigroup (Canvas a) where
  (<>) = liftM2 (<>)
-----------------------------------------------------------------------------
instance Monoid a => Monoid (Canvas a) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftM2 mappend
#endif
  mempty  = return mempty
-----------------------------------------------------------------------------
-- | Interprets 'Canvas' DSL inside of the "draw" synchronous callback.
interpret :: JSVal -> Canvas a -> JSM a
interpret ctx (Bind m f) =
  interpret ctx =<< f <$> interpret ctx m
interpret _ (LiftIO io) =
  liftIO io
interpret _ (LiftJSM jsm) =
  liftJSM jsm
interpret _ (Pure m) =
  pure m
interpret ctx (ClearRect (x,y,h,w)) =
  void $ ctx # ("clearRect" :: MisoString) $ [ x, y, h, w ]
interpret ctx (FillRect (x,y,h,w)) =
  void $ ctx # ("fillRect" :: MisoString) $ [ x, y, h, w ]
interpret ctx (StrokeRect (x,y,h,w)) =
  void $ ctx # ("strokeRect" :: MisoString) $ [ x, y, h, w ]
interpret ctx BeginPath =
  void $ ctx # ("beginPath" :: MisoString) $ ([] :: [MisoString])
interpret ctx ClosePath =
  void $ ctx # ("closePath" :: MisoString) $ ([] :: [MisoString])
interpret ctx Clip =
  void $ ctx # ("clip" :: MisoString) $ ([] :: [MisoString])
interpret ctx Save =
  void $ ctx # ("save" :: MisoString) $ ([] :: [MisoString])
interpret ctx Restore =
  void $ ctx # ("restore" :: MisoString) $ ([] :: [MisoString])
interpret ctx (MoveTo (x,y)) =
  void $ ctx # ("moveTo" :: MisoString) $ [x,y]
interpret ctx (LineTo (x,y)) =
  void $ ctx # ("lineTo" :: MisoString) $ [x,y]
interpret ctx Stroke =
  void $ ctx # ("stroke" :: MisoString) $ ([] :: [MisoString])
interpret ctx Fill =
  void $ ctx # ("fill" :: MisoString) $ ([] :: [MisoString])
interpret ctx (Rect (x,y,h,w)) =
  void $ ctx # ("rect" :: MisoString) $ [x,y,h,w]
interpret ctx (BezierCurveTo (a,b,c,d,e,f)) =
  void $ ctx # ("bezierCurveTo" :: MisoString) $ [a,b,c,d,e,f]
interpret ctx (Arc (a,b,c,d,e)) =
  void $ ctx # ("arc" :: MisoString) $ [a,b,c,d,e]
interpret ctx (ArcTo (a,b,c,d,e)) =
  void $ ctx # ("arcTo" :: MisoString) $ [a,b,c,d,e]
interpret ctx (QuadraticCurveTo (a,b,c,d)) =
  void $ ctx # ("quadraticCurveTo" :: MisoString) $ [a,b,c,d]
interpret ctx (IsPointInPath (x,y)) = do
  Just result <- fromJSVal =<< do
    ctx # ("isPointInPath" :: MisoString) $ [x,y]
  pure result
interpret ctx (Direction d) =
  void $ (ctx <# ("direction" :: MisoString)) (renderDirectionType d)
interpret ctx (TextAlign t) =
  void $ (ctx <# ("textAlign" :: MisoString)) (renderTextAlignType t)
interpret ctx (TextBaseline t) =
  void $ (ctx <# ("textBaseline" :: MisoString)) (renderTextBaselineType t)
interpret ctx (Font f) =
  void $ (ctx <# ("font" :: MisoString)) f
interpret ctx (FillStyle style) =
  void $ (ctx <# ("fillStyle" :: MisoString)) =<< renderStyleArg style
interpret ctx (GlobalCompositeOperation s) =
  void $ (ctx <# ("globalCompositeOperation" :: MisoString)) (renderCompositeOperation s)
interpret ctx (FillText (txt', x' , y')) = do
  txt <- toJSVal txt'
  x <- toJSVal x'
  y <- toJSVal y'
  void $ (ctx # ("fillText" :: MisoString)) [txt, x, y]
interpret ctx (StrokeText (txt', x', y')) = do
  txt <- toJSVal txt'
  x <- toJSVal x'
  y <- toJSVal y'
  void $ (ctx # ("strokeText" :: MisoString)) [txt, x, y]
interpret ctx (MeasureText txt) = do
  o <- ctx # ("measureText" :: MisoString) $ [txt]
  Just w <- fromJSVal =<< o ! ("width" :: MisoString)
  pure w
interpret ctx (Translate (x,y)) =
  void $ ctx # ("translate" :: MisoString) $ [ x, y ]
interpret _ (AddColorStop (Gradient grd) x' color') = do
  x <- toJSVal x'
  c <- toJSVal (renderColor color')
  void $ grd # ("addColorStop" :: MisoString) $ [ x, c ]
interpret ctx (CreateLinearGradient (w,x,y,z)) =
  Gradient <$> do
    ctx # ("createLinearGradient" :: MisoString) $
      [w,x,y,z]
interpret ctx (CreatePattern image patternType) = do
  img <- toJSVal image
  pt <- toJSVal (renderPattern patternType)
  Pattern <$> do
    ctx # ("createPattern" :: MisoString) $
      [ img, pt ]
interpret ctx (CreateRadialGradient (w,x,y,z,k,j)) =
  Gradient <$> do
    ctx # ("createRadialGradient" :: MisoString) $
      [w,x,y,z,k,j]
interpret ctx (LineCap typ) = do
  t <- toJSVal (renderLineCapType typ)
  void $ do
    ctx <# ("lineCap" :: MisoString) $ t
interpret ctx (LineJoin ljt') = do
  ljt <- toJSVal (renderLineJoinType ljt')
  void $ do ctx <# ("lineJoin" :: MisoString) $ ljt
interpret ctx (LineWidth w) =
  void $ do ctx <# ("lineWidth" :: MisoString) $ w
interpret ctx (MiterLimit w) =
  void $ do ctx <# ("miterLimit" :: MisoString) $ w
interpret ctx (ShadowBlur w) =
  void $ do ctx <# ("shadowBlur" :: MisoString) $ w
interpret ctx (ShadowColor c) =
  void $ do ctx <# ("shadowColor" :: MisoString) $ renderColor c
interpret ctx (ShadowOffsetX x) =
  void $ do ctx <# ("shadowOffsetX" :: MisoString) $ x
interpret ctx (ShadowOffsetY y) =
  void $ do ctx <# ("shadowOffsetY" :: MisoString) $ y
interpret ctx (StrokeStyle s) =
  (ctx <# ("strokeStyle" :: MisoString)) =<< renderStyleArg s
interpret ctx (Scale (w,h)) = do
  void $ ctx # ("scale" :: MisoString) $ [w,h]
interpret ctx (Rotate x) =
  void $ ctx # ("rotate" :: MisoString) $ [x]
interpret ctx (Transform (a,b,c,d,e,f)) =
  void $ ctx # ("transform" :: MisoString) $ [a,b,c,d,e,f]
interpret ctx (SetTransform (a,b,c,d,e,f)) =
  void $ ctx # ("setTransform" :: MisoString) $ [a,b,c,d,e,f]
interpret ctx (DrawImage (img', x',y')) = do
  img <- toJSVal img'
  x <- toJSVal x'
  y <- toJSVal y'
  void $ ctx # ("drawImage" :: MisoString) $ [img,x,y]
interpret ctx (DrawImage' (img', w',x',y',z')) = do
  img <- toJSVal img'
  w <- toJSVal w'
  x <- toJSVal x'
  y <- toJSVal y'
  z <- toJSVal z'
  void $ ctx # ("drawImage" :: MisoString) $ [img,w,x,y,z]
interpret ctx (CreateImageData (x,y)) =
  ImageData <$> do
    ctx # ("createImageData" :: MisoString) $
      [x,y]
interpret ctx (GetImageData (w,x,y,z)) =
  ImageData <$> do
    ctx # ("getImageData" :: MisoString) $
      [w,x,y,z]
interpret ctx (PutImageData (imgData, x',y')) = do
  img <- toJSVal imgData
  x <- toJSVal x'
  y <- toJSVal y'
  void $
    ctx # ("putImageData" :: MisoString) $
      [img,x,y]
interpret _ (SetImageData imgData index value) = do
  o <- imgData ! ("data" :: MisoString)
  (o <## index) value
interpret _ (ImageDataHeight imgData) = do
  Just h <- fromJSVal =<< imgData ! ("height" :: MisoString)
  pure h
interpret _ (ImageDataWidth imgData) = do
  Just w <- fromJSVal =<< imgData ! ("width" :: MisoString)
  pure w
interpret ctx (GlobalAlpha alpha) =
  ctx <# ("globalAlpha" :: MisoString) $ alpha
-----------------------------------------------------------------------------
-- | [ctx.globalCompositeOperation = "source-over"](https://www.w3schools.com/tags/canvas_globalcompositeoperation.asp)
globalCompositeOperation :: CompositeOperation -> Canvas ()
globalCompositeOperation = GlobalCompositeOperation
-----------------------------------------------------------------------------
-- | [ctx.clearRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_clearrect.asp)
clearRect :: (Double, Double, Double, Double) -> Canvas ()
clearRect = ClearRect
-----------------------------------------------------------------------------
-- | [ctx.fillRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_fillrect.asp)
fillRect :: (Double, Double, Double, Double) -> Canvas ()
fillRect = FillRect
-----------------------------------------------------------------------------
-- | [ctx.strokeRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_strokerect.asp)
strokeRect :: (Double, Double, Double, Double) -> Canvas ()
strokeRect = StrokeRect
-----------------------------------------------------------------------------
-- | [ctx.beginPath()](https://www.w3schools.com/tags/canvas_beginpath.asp)
beginPath :: () -> Canvas ()
beginPath () = BeginPath
-----------------------------------------------------------------------------
-- | [ctx.closePath()](https://www.w3schools.com/tags/canvas_closepath.asp)
closePath :: Canvas ()
closePath = ClosePath
-----------------------------------------------------------------------------
-- | [ctx.moveTo(x,y)](https://www.w3schools.com/tags/canvas_moveto.asp)
moveTo :: Coord -> Canvas ()
moveTo = MoveTo
-----------------------------------------------------------------------------
-- | [ctx.lineTo(x,y)](https://www.w3schools.com/tags/canvas_lineto.asp)
lineTo :: Coord -> Canvas ()
lineTo = LineTo
-----------------------------------------------------------------------------
-- | [ctx.fill()](https://www.w3schools.com/tags/canvas_fill.asp)
fill :: Canvas ()
fill = Fill
-----------------------------------------------------------------------------
-- | [ctx.rect(x,y,width,height)](https://www.w3schools.com/tags/canvas_rect.asp)
rect :: (Double, Double, Double, Double) -> Canvas ()
rect = Rect
-----------------------------------------------------------------------------
-- | [ctx.stroke()](https://www.w3schools.com/tags/canvas_stroke.asp)
stroke :: () -> Canvas ()
stroke () = Stroke
-----------------------------------------------------------------------------
-- | [ctx.bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y)](https://www.w3schools.com/tags/canvas_beziercurveto.asp)
bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
bezierCurveTo = BezierCurveTo
-----------------------------------------------------------------------------
-- | [context.arc(x, y, r, sAngle, eAngle, counterclockwise)](https://www.w3schools.com/tags/canvas_arc.asp)
arc :: (Double, Double, Double, Double, Double) -> Canvas ()
arc = Arc
-----------------------------------------------------------------------------
-- | [context.arcTo(x1, y1, x2, y2, r)](https://www.w3schools.com/tags/canvas_arcto.asp)
arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
arcTo = ArcTo
-----------------------------------------------------------------------------
-- | [context.quadraticCurveTo(cpx,cpy,x,y)](https://www.w3schools.com/tags/canvas_quadraticcurveto.asp)
quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
quadraticCurveTo = QuadraticCurveTo
-----------------------------------------------------------------------------
-- | [context.direction = "ltr"](https://www.w3schools.com/tags/canvas_direction.asp)
direction :: DirectionType -> Canvas ()
direction = Direction
-----------------------------------------------------------------------------
-- | [context.fillText(text,x,y)](https://www.w3schools.com/tags/canvas_filltext.asp)
fillText :: (MisoString, Double, Double) -> Canvas ()
fillText = FillText
-----------------------------------------------------------------------------
-- | [context.font = "italic small-caps bold 12px arial"](https://www.w3schools.com/tags/canvas_font.asp)
font :: MisoString -> Canvas ()
font = Font
-----------------------------------------------------------------------------
-- | [ctx.strokeText()](https://www.w3schools.com/tags/canvas_stroketext.asp)
strokeText :: (MisoString, Double, Double) -> Canvas ()
strokeText = StrokeText
-----------------------------------------------------------------------------
-- | [ctx.textAlign = "start"](https://www.w3schools.com/tags/canvas_textalign.asp)
textAlign :: TextAlignType -> Canvas ()
textAlign = TextAlign
-----------------------------------------------------------------------------
-- | [ctx.textBaseline = "top"](https://www.w3schools.com/tags/canvas_textBaseLine.asp)
textBaseline :: TextBaselineType -> Canvas ()
textBaseline = TextBaseline
-----------------------------------------------------------------------------
-- | [gradient.addColorStop(stop,color)](https://www.w3schools.com/tags/canvas_addcolorstop.asp)
addColorStop :: Gradient -> Double -> Color -> Canvas ()
addColorStop = AddColorStop
-----------------------------------------------------------------------------
-- | [ctx.createLinearGradient(x0,y0,x1,y1)](https://www.w3schools.com/tags/canvas_createlineargradient.asp)
createLinearGradient :: (Double, Double, Double, Double) -> Canvas Gradient
createLinearGradient = CreateLinearGradient
-----------------------------------------------------------------------------
-- | [ctx.createPattern(image, "repeat")](https://www.w3schools.com/tags/canvas_createpattern.asp)
createPattern :: Image -> PatternType -> Canvas Pattern
createPattern = CreatePattern
-----------------------------------------------------------------------------
-- | [ctx.createRadialGradient(x0,y0,r0,x1,y1,r1)](https://www.w3schools.com/tags/canvas_createradialgradient.asp)
createRadialGradient :: (Double,Double,Double,Double,Double,Double) -> Canvas Gradient
createRadialGradient = CreateRadialGradient
-----------------------------------------------------------------------------
-- | [ctx.fillStyle = "red"](https://www.w3schools.com/tags/canvas_fillstyle.asp)
fillStyle :: StyleArg -> Canvas ()
fillStyle = FillStyle
-----------------------------------------------------------------------------
-- | [ctx.lineCap = "butt"](https://www.w3schools.com/tags/canvas_lineCap.asp)
lineCap :: LineCapType -> Canvas ()
lineCap = LineCap
-----------------------------------------------------------------------------
-- | [ctx.lineJoin = "bevel"](https://www.w3schools.com/tags/canvas_lineJoin.asp)
lineJoin :: LineJoinType -> Canvas ()
lineJoin = LineJoin
-----------------------------------------------------------------------------
-- | [ctx.lineWidth = 10](https://www.w3schools.com/tags/canvas_lineWidth.asp)
lineWidth :: Double -> Canvas ()
lineWidth = LineWidth
-----------------------------------------------------------------------------
-- | [ctx.miterLimit = 10](https://www.w3schools.com/tags/canvas_miterLimit.asp)
miterLimit :: Double -> Canvas ()
miterLimit = MiterLimit
-----------------------------------------------------------------------------
-- | [ctx.shadowBlur = 10](https://www.w3schools.com/tags/canvas_shadowBlur.asp)
shadowBlur :: Double -> Canvas ()
shadowBlur = ShadowBlur
-----------------------------------------------------------------------------
-- | [ctx.shadowColor = "red"](https://www.w3schools.com/tags/canvas_shadowColor.asp)
shadowColor :: Color -> Canvas ()
shadowColor = ShadowColor
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetX = 20](https://www.w3schools.com/tags/canvas_shadowOffsetX.asp)
shadowOffsetX :: Double -> Canvas ()
shadowOffsetX = ShadowOffsetX
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetY = 20](https://www.w3schools.com/tags/canvas_shadowOffsetY.asp)
shadowOffsetY :: Double -> Canvas ()
shadowOffsetY = ShadowOffsetY
-----------------------------------------------------------------------------
-- | [ctx.strokeStyle = "red"](https://www.w3schools.com/tags/canvas_strokeStyle.asp)
strokeStyle :: StyleArg -> Canvas ()
strokeStyle = StrokeStyle
-----------------------------------------------------------------------------
-- | [ctx.scale(width,height)](https://www.w3schools.com/tags/canvas_scale.asp)
scale :: (Double, Double) -> Canvas ()
scale = Scale
-----------------------------------------------------------------------------
-- | [ctx.rotate(angle)](https://www.w3schools.com/tags/canvas_rotate.asp)
rotate :: Double -> Canvas ()
rotate = Rotate
-----------------------------------------------------------------------------
-- | [ctx.translate(angle)](https://www.w3schools.com/tags/canvas_translate.asp)
translate :: Coord -> Canvas ()
translate = Translate
-----------------------------------------------------------------------------
-- | [ctx.transform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_transform.asp)
transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
transform = Transform
-----------------------------------------------------------------------------
-- | [ctx.setTransform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_setTransform.asp)
setTransform
  :: (Double, Double, Double, Double, Double, Double)
  -> Canvas ()
setTransform = SetTransform
-----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage :: (Image, Double, Double) -> Canvas ()
drawImage = DrawImage
-----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage' :: (Image, Double, Double, Double, Double) -> Canvas ()
drawImage' = DrawImage'
-----------------------------------------------------------------------------
-- | [ctx.createImageData(width,height)](https://www.w3schools.com/tags/canvas_createImageData.asp)
createImageData :: (Double, Double) -> Canvas ImageData
createImageData = CreateImageData
-----------------------------------------------------------------------------
-- | [ctx.getImageData(w,x,y,z)](https://www.w3schools.com/tags/canvas_getImageData.asp)
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData = GetImageData
-----------------------------------------------------------------------------
-- | [imageData.data[index] = 255](https://www.w3schools.com/tags/canvas_imagedata_data.asp)
setImageData :: ImageData -> Int -> Double -> Canvas ()
setImageData = SetImageData
-----------------------------------------------------------------------------
-- | [imageData.height](https://www.w3schools.com/tags/canvas_imagedata_height.asp)
height :: ImageData -> Canvas Double
height = ImageDataHeight
-----------------------------------------------------------------------------
-- | [imageData.width](https://www.w3schools.com/tags/canvas_imagedata_width.asp)
width :: ImageData -> Canvas Double
width = ImageDataWidth
-----------------------------------------------------------------------------
-- | [ctx.putImageData(imageData,x,y)](https://www.w3schools.com/tags/canvas_putImageData.asp)
putImageData :: (ImageData, Double, Double) -> Canvas ()
putImageData = PutImageData
-----------------------------------------------------------------------------
-- | [ctx.globalAlpha = 0.2](https://www.w3schools.com/tags/canvas_globalAlpha.asp)
globalAlpha :: Double -> Canvas ()
globalAlpha = GlobalAlpha
-----------------------------------------------------------------------------
-- | [ctx.clip()](https://www.w3schools.com/tags/canvas_clip.asp)
clip :: () -> Canvas ()
clip () = Clip
-----------------------------------------------------------------------------
-- | [ctx.save()](https://www.w3schools.com/tags/canvas_save.asp)
save :: () -> Canvas ()
save () = Save
-----------------------------------------------------------------------------
-- | [ctx.restore()](https://www.w3schools.com/tags/canvas_restore.asp)
restore :: () -> Canvas ()
restore () = Restore
-----------------------------------------------------------------------------
