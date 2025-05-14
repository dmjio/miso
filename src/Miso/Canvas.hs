-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
----------------------------------------------------------------------------
module Miso.Canvas
  ( -- * Types
    Canvas (..)
  , Pattern (..)
  , PatternType (..)
  , LineJoinType (..)
  , LineCapType (..)
  , Gradient (..)
  , ImageData (..)
    -- * Property
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
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad (void, liftM, ap, liftM2)
import           Data.Kind (Type)
import           Language.Javascript.JSaddle ( JSM, JSVal, (#), fromJSVal
                                             , (<#), toJSVal, (!)
                                             , liftJSM, MonadJSM(..)
                                             , ToJSVal, MakeObject, (<##)
                                             )
-----------------------------------------------------------------------------
import qualified Miso.FFI as FFI
import           Miso.FFI (Image)
import           Miso.Html.Types
import           Miso.Style (Color, renderColor)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
canvas_ :: forall action a . [ Attribute action ] -> Canvas a -> View action
canvas_ attributes canvas = node HTML "canvas" Nothing attrs []
  where
    attrs :: [ Attribute action ]
    attrs = flip (:) attributes $ Event $ \_ obj _ _ ->
      flip (FFI.set "draw") obj =<< do
        FFI.syncCallback1 $ \domRef -> do
          ctx <- domRef # ("getContext" :: String) $ ["2d" :: MisoString]
          void (interpret ctx canvas)
-----------------------------------------------------------------------------
data PatternType = Repeat | RepeatX | RepeatY | NoRepeat
-----------------------------------------------------------------------------
renderPattern :: PatternType -> MisoString
renderPattern Repeat   = "repeat"
renderPattern RepeatX  = "repeat-x"
renderPattern RepeatY  = "repeat-y"
renderPattern NoRepeat = "no-repeat"
-----------------------------------------------------------------------------
data LineCapType = LineCapButt | LineCapRound | LineCapSquare
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
newtype Pattern = Pattern JSVal
-----------------------------------------------------------------------------
newtype Gradient = Gradient JSVal
-----------------------------------------------------------------------------
newtype ImageData = ImageData JSVal deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
type Coord = (Double, Double)
-----------------------------------------------------------------------------
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
  Direction :: MisoString -> Canvas ()
  FillText :: (MisoString, Double, Double) -> Canvas ()
  Font :: MisoString -> Canvas ()
  StrokeText :: (MisoString, Double, Double) -> Canvas ()
  TextAlign :: MisoString -> Canvas ()
  TextBaseline :: MisoString -> Canvas ()
  AddColorStop :: Gradient -> Double -> Color -> Canvas ()
  CreateLinearGradient :: (Double, Double, Double, Double) -> Canvas Gradient
  CreatePattern :: Image -> PatternType -> Canvas Pattern
  CreateRadialGradient :: (Double, Double, Double, Double, Double, Double) -> Canvas Gradient
  FillStyle :: MisoString -> Canvas ()
  LineCap :: LineCapType -> Canvas ()
  LineJoin :: LineJoinType -> Canvas ()
  LineWidth :: Double -> Canvas ()
  MiterLimit :: Double -> Canvas ()
  ShadowBlur :: Double -> Canvas ()
  ShadowColor :: Color -> Canvas ()
  ShadowOffsetX :: Double -> Canvas ()
  ShadowOffsetY :: Double -> Canvas ()
  StrokeStyle :: MisoString -> Canvas ()
  Scale :: (Double, Double) -> Canvas ()
  Rotate :: Double -> Canvas ()
  Translate :: Coord -> Canvas ()
  Transform :: (Double,Double,Double,Double,Double,Double) -> Canvas ()
  SetTransform :: (Double,Double,Double,Double,Double,Double) -> Canvas ()
  DrawImage :: Image -> Coord -> Canvas ()
  DrawImage' :: Image -> (Double, Double, Double, Double) -> Canvas ()
  CreateImageData :: (Double, Double) -> Canvas ImageData
  GetImageData :: (Double, Double, Double, Double) -> Canvas ImageData
  SetImageData :: ImageData -> Int -> Double -> Canvas ()
  ImageDataHeight :: ImageData -> Canvas Double
  ImageDataWidth :: ImageData -> Canvas Double
  PutImageData :: (ImageData, Double, Double) -> Canvas ()
  GlobalAlpha :: Double -> Canvas ()
  GlobalCompositeOperation :: MisoString -> Canvas ()
  Clip :: Canvas ()
  Save :: Canvas ()
  Restore :: Canvas ()
-----------------------------------------------------------------------------
instance MonadIO Canvas where
  liftIO = LiftIO
-----------------------------------------------------------------------------
#ifndef GHCJS_OLD
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
  void $ ctx # ("clearRect" :: String) $ [ x, y, h, w ]
interpret ctx (FillRect (x,y,h,w)) =
  void $ ctx # ("fillRect" :: String) $ [ x, y, h, w ]
interpret ctx (StrokeRect (x,y,h,w)) =
  void $ ctx # ("strokeRect" :: String) $ [ x, y, h, w ]
interpret ctx BeginPath =
  void $ ctx # ("beginPath" :: String) $ ([] :: [MisoString])
interpret ctx ClosePath =
  void $ ctx # ("closePath" :: String) $ ([] :: [MisoString])
interpret ctx Clip =
  void $ ctx # ("clip" :: String) $ ([] :: [MisoString])
interpret ctx Save =
  void $ ctx # ("save" :: String) $ ([] :: [MisoString])
interpret ctx Restore =
  void $ ctx # ("restore" :: String) $ ([] :: [MisoString])
interpret ctx (MoveTo (x,y)) =
  void $ ctx # ("moveTo" :: String) $ [x,y]
interpret ctx (LineTo (x,y)) =
  void $ ctx # ("lineTo" :: String) $ [x,y]
interpret ctx Stroke =
  void $ ctx # ("stroke" :: String) $ ([] :: [MisoString])
interpret ctx Fill =
  void $ ctx # ("fill" :: String) $ ([] :: [MisoString])
interpret ctx (Rect (x,y,h,w)) =
  void $ ctx # ("rect" :: String) $ [x,y,h,w]
interpret ctx (BezierCurveTo (a,b,c,d,e,f)) =
  void $ ctx # ("bezierCurveTo" :: String) $ [a,b,c,d,e,f]
interpret ctx (Arc (a,b,c,d,e)) =
  void $ ctx # ("arc" :: String) $ [a,b,c,d,e]
interpret ctx (ArcTo (a,b,c,d,e)) =
  void $ ctx # ("arcTo" :: String) $ [a,b,c,d,e]
interpret ctx (QuadraticCurveTo (a,b,c,d)) =
  void $ ctx # ("quadraticCurveTo" :: String) $ [a,b,c,d]
interpret ctx (IsPointInPath (x,y)) = do
  Just result <- fromJSVal =<< do
    ctx # ("isPointInPath" :: String) $ [x,y]
  pure result
interpret ctx (Direction d) =
  void $ (ctx <# ("direction" :: MisoString)) d
interpret ctx (TextAlign t) =
  void $ (ctx <# ("textAlign" :: MisoString)) t
interpret ctx (TextBaseline t) =
  void $ (ctx <# ("textBaseline" :: MisoString)) t
interpret ctx (Font f) =
  void $ (ctx <# ("font" :: MisoString)) f
interpret ctx (FillStyle style) =
  void $ (ctx <# ("fillStyle" :: MisoString)) style
interpret ctx (GlobalCompositeOperation s) =
  void $ (ctx <# ("globalCompositeOperation" :: MisoString)) s
interpret ctx (FillText args) =
  void $ (ctx # ("fillText" :: MisoString)) =<< toJSVal args
interpret ctx (StrokeText args) =
  void $ (ctx # ("strokeText" :: MisoString)) =<< toJSVal args
interpret ctx (MeasureText txt) = do
  o <- ctx # ("measureText" :: String) $ [txt]
  Just w <- fromJSVal =<< o ! ("width" :: MisoString)
  pure w
interpret ctx (Translate (x,y)) =
  void $ ctx # ("translate" :: String) $ [ x, y ]
interpret _ (AddColorStop (Gradient grd) x' color') = do
  x <- toJSVal x'
  color <- toJSVal (renderColor color')
  void $ grd # ("addColorStop" :: String) $ [ x, color ]
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
interpret ctx (ShadowColor color) =
  void $ do ctx <# ("shadowColor" :: MisoString) $ renderColor color
interpret ctx (ShadowOffsetX x) =
  void $ do ctx <# ("shadowOffsetX" :: MisoString) $ x
interpret ctx (ShadowOffsetY y) =
  void $ do ctx <# ("shadowOffsetY" :: MisoString) $ y
interpret ctx (StrokeStyle s) =
  (ctx <# ("strokeStyle" :: MisoString)) s
interpret ctx (Scale (w,h)) = do
  void $ ctx # ("scale" :: MisoString) $ [w,h]
interpret ctx (Rotate x) =
  void $ ctx # ("rotate" :: MisoString) $ [x]
interpret ctx (Transform (a,b,c,d,e,f)) =
  void $ ctx # ("transform" :: MisoString) $ [a,b,c,d,e,f]
interpret ctx (SetTransform (a,b,c,d,e,f)) =
  void $ ctx # ("setTransform" :: MisoString) $ [a,b,c,d,e,f]
interpret ctx (DrawImage img' (x',y')) = do
  img <- toJSVal img'
  x <- toJSVal x'
  y <- toJSVal y'
  void $ ctx # ("drawImage" :: MisoString) $ [img,x,y]
interpret ctx (DrawImage' img' (w',x',y',z')) = do
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
  o <- imgData ! "data"
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
globalCompositeOperation :: MisoString -> Canvas ()
globalCompositeOperation = GlobalCompositeOperation
-----------------------------------------------------------------------------
clearRect :: (Double, Double, Double, Double) -> Canvas ()
clearRect = ClearRect
-----------------------------------------------------------------------------
fillRect :: (Double, Double, Double, Double) -> Canvas ()
fillRect = FillRect
-----------------------------------------------------------------------------
strokeRect :: (Double, Double, Double, Double) -> Canvas ()
strokeRect = StrokeRect
-----------------------------------------------------------------------------
beginPath :: () -> Canvas ()
beginPath () = BeginPath
-----------------------------------------------------------------------------
closePath :: Canvas ()
closePath = ClosePath
-----------------------------------------------------------------------------
moveTo :: (Double, Double) -> Canvas ()
moveTo = MoveTo
-----------------------------------------------------------------------------
lineTo :: (Double, Double) -> Canvas ()
lineTo = LineTo
-----------------------------------------------------------------------------
fill :: Canvas ()
fill = Fill
-----------------------------------------------------------------------------
rect :: (Double, Double, Double, Double) -> Canvas ()
rect = Rect
-----------------------------------------------------------------------------
stroke :: () -> Canvas ()
stroke () = Stroke
-----------------------------------------------------------------------------
bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
bezierCurveTo = BezierCurveTo
-----------------------------------------------------------------------------
arc :: (Double, Double, Double, Double, Double) -> Canvas ()
arc = Arc
-----------------------------------------------------------------------------
arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
arcTo = ArcTo
-----------------------------------------------------------------------------
quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
quadraticCurveTo = QuadraticCurveTo
-----------------------------------------------------------------------------
direction :: MisoString -> Canvas ()
direction = Direction
-----------------------------------------------------------------------------
fillText :: (MisoString, Double, Double) -> Canvas ()
fillText = FillText
-----------------------------------------------------------------------------
font :: MisoString -> Canvas ()
font = Font
-----------------------------------------------------------------------------
strokeText :: (MisoString, Double, Double) -> Canvas ()
strokeText = StrokeText
-----------------------------------------------------------------------------
textAlign :: MisoString -> Canvas ()
textAlign = TextAlign
-----------------------------------------------------------------------------
textBaseline :: MisoString -> Canvas ()
textBaseline = TextBaseline
-----------------------------------------------------------------------------
addColorStop :: Gradient -> Double -> Color -> Canvas ()
addColorStop = AddColorStop
-----------------------------------------------------------------------------
createLinearGradient :: (Double, Double, Double, Double) -> Canvas Gradient
createLinearGradient = CreateLinearGradient
-----------------------------------------------------------------------------
createPattern :: Image -> PatternType -> Canvas Pattern
createPattern = CreatePattern
-----------------------------------------------------------------------------
createRadialGradient :: (Double,Double,Double,Double,Double,Double) -> Canvas Gradient
createRadialGradient = CreateRadialGradient
-----------------------------------------------------------------------------
fillStyle :: Color -> Canvas ()
fillStyle color = FillStyle (renderColor color)
-----------------------------------------------------------------------------
lineCap :: LineCapType -> Canvas ()
lineCap = LineCap
-----------------------------------------------------------------------------
lineJoin :: LineJoinType -> Canvas ()
lineJoin = LineJoin
-----------------------------------------------------------------------------
lineWidth :: Double -> Canvas ()
lineWidth = LineWidth
-----------------------------------------------------------------------------
miterLimit :: Double -> Canvas ()
miterLimit = MiterLimit
-----------------------------------------------------------------------------
shadowBlur :: Double -> Canvas ()
shadowBlur = ShadowBlur
-----------------------------------------------------------------------------
shadowColor :: Color -> Canvas ()
shadowColor = ShadowColor
-----------------------------------------------------------------------------
shadowOffsetX :: Double -> Canvas ()
shadowOffsetX = ShadowOffsetX
-----------------------------------------------------------------------------
shadowOffsetY :: Double -> Canvas ()
shadowOffsetY = ShadowOffsetY
-----------------------------------------------------------------------------
strokeStyle :: Color -> Canvas ()
strokeStyle color = StrokeStyle (renderColor color)
-----------------------------------------------------------------------------
scale :: (Double, Double) -> Canvas ()
scale = Scale
-----------------------------------------------------------------------------
rotate :: Double -> Canvas ()
rotate = Rotate
-----------------------------------------------------------------------------
translate :: Coord -> Canvas ()
translate = Translate
-----------------------------------------------------------------------------
transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
transform = Transform
-----------------------------------------------------------------------------
setTransform
  :: (Double, Double, Double, Double, Double, Double)
  -> Canvas ()
setTransform = SetTransform
-----------------------------------------------------------------------------
drawImage :: Image -> Coord -> Canvas ()
drawImage = DrawImage
-----------------------------------------------------------------------------
drawImage' :: Image -> (Double, Double, Double, Double) -> Canvas ()
drawImage' = DrawImage'
-----------------------------------------------------------------------------
createImageData :: (Double, Double) -> Canvas ImageData
createImageData = CreateImageData
-----------------------------------------------------------------------------
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData = GetImageData
-----------------------------------------------------------------------------
setImageData :: ImageData -> Int -> Double -> Canvas ()
setImageData = SetImageData
-----------------------------------------------------------------------------
height :: ImageData -> Canvas Double
height = ImageDataHeight
-----------------------------------------------------------------------------
width :: ImageData -> Canvas Double
width = ImageDataWidth
-----------------------------------------------------------------------------
putImageData :: (ImageData, Double, Double) -> Canvas ()
putImageData = PutImageData
-----------------------------------------------------------------------------
globalAlpha :: Double -> Canvas ()
globalAlpha = GlobalAlpha
-----------------------------------------------------------------------------
clip :: () -> Canvas ()
clip () = Clip
-----------------------------------------------------------------------------
save :: () -> Canvas ()
save () = Save
-----------------------------------------------------------------------------
restore :: () -> Canvas ()
restore () = Restore
-----------------------------------------------------------------------------
