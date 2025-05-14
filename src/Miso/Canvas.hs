-----------------------------------------------------------------------------
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
----------------------------------------------------------------------------
module Miso.Canvas
  ( -- * Types
    Canvas (..)
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
  , imageDataData
  , imageDataHeight
  , imageDataWidth
  , putImageData
  , globalAlpha
  , clip
  , save
  , restore
  , createEvent
  , getContext
  , toDataUrl
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad (void, liftM, ap, liftM2)
import           Data.Kind (Type)
import           Language.Javascript.JSaddle ( JSM, JSVal, (#), fromJSVal
                                             , (<#), toJSVal, (!)
                                             , liftJSM, MonadJSM(..)
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
  AddColorStop :: Canvas ()
  CreateLinearGradient :: Canvas ()
  CreatePattern :: Canvas ()
  CreateRadialGradient :: Canvas ()
  FillStyle :: MisoString -> Canvas ()
  LineCap :: Canvas ()
  LineJoin :: Canvas ()
  LineWidth :: Canvas ()
  MiterLimit :: Canvas ()
  ShadowBlur :: Canvas ()
  ShadowColor :: Canvas ()
  ShadowOffsetX :: Canvas ()
  ShadowOffsetY :: Canvas ()
  StrokeStyle :: MisoString -> Canvas ()
  Scale :: Canvas ()
  Rotate :: Double -> Canvas ()
  Translate :: Coord -> Canvas ()
  Transform :: Canvas ()
  SetTransform :: Canvas ()
  DrawImage :: Image -> Coord -> Canvas ()
  DrawImage' :: Image -> (Double, Double, Double, Double) -> Canvas ()
  CreateImageData :: Canvas ()
  GetImageData :: Canvas ()
  ImageDataData :: Canvas ()
  ImageDataHeight :: Canvas ()
  ImageDataWidth :: Canvas ()
  PutImageData :: Canvas ()
  GlobalAlpha :: Canvas ()
  GlobalCompositeOperation :: MisoString -> Canvas ()
  Clip :: Canvas ()
  Save :: Canvas ()
  Restore :: Canvas ()
  CreateEvent :: Canvas ()
  GetContext :: Canvas ()
  ToDataURL :: Canvas ()
-----------------------------------------------------------------------------
instance MonadIO Canvas where
  liftIO = LiftIO
-----------------------------------------------------------------------------
instance MonadJSM Canvas where
  liftJSM' = LiftJSM
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
  Just result <- fromJSVal =<< do ctx # ("isPointInPath" :: String) $ [x,y]
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
  Just width <- fromJSVal =<< o ! ("width" :: MisoString)
  pure width
interpret ctx (Translate (x,y)) =
  void $ ctx # ("translate" :: String) $ [ x, y ]
interpret ctx (Bind m f) =
  interpret ctx =<< f <$> interpret ctx m
interpret _ (LiftIO io) =
  liftIO io
interpret _ (LiftJSM jsm) =
  liftJSM jsm
interpret _ (Pure m) =
  pure m
interpret _ (AddColorStop) =
  error "add color stop"
interpret _ (CreateLinearGradient) =
  error "create linear gradient"
interpret _ (CreatePattern) =
  error "create pattern"
interpret _ (CreateRadialGradient) =
  error "create radial grandient"
interpret _ (LineCap) =
  error "LineCap"
interpret _ (LineJoin) =
  error "LineJoin"
interpret _ (LineWidth) =
  error "LineWidth"
interpret _ (MiterLimit) =
  error "MiterLimit"
interpret _ (ShadowBlur) =
  error "ShadowBlur"
interpret _ (ShadowColor) =
  error "ShadowColor"
interpret _ (ShadowOffsetX) =
  error "ShadowOffsetX"
interpret _ (ShadowOffsetY) =
  error "ShadowOffsetY"
interpret ctx (StrokeStyle s) =
  (ctx <# ("strokeStyle" :: MisoString)) s
interpret _ (Scale) =
  error "Scale"
interpret ctx (Rotate x) =
  void $ ctx # ("rotate" :: MisoString) $ [x]
interpret _ (Transform) =
  error "Transform"
interpret _ (SetTransform) =
  error "SetTransform"
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
interpret _ (CreateImageData) =
  error "CreateImageData"
interpret _ (GetImageData) =
  error "GetImageData"
interpret _ (ImageDataData) =
  error "ImageDataData"
interpret _ (ImageDataHeight) =
  error "ImageDataHeight"
interpret _ (ImageDataWidth) =
  error "ImageDataWidth"
interpret _ (PutImageData) =
  error "PutImageData"
interpret _ (GlobalAlpha) =
  error "GlobalAlpha"
interpret _ (CreateEvent) =
  error "CreateEvent"
interpret _ (GetContext) =
  error "GetContext"
interpret _ (ToDataURL) =
  error "ToDataURL"
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
addColorStop :: Canvas ()
addColorStop = AddColorStop
-----------------------------------------------------------------------------
createLinearGradient :: Canvas ()
createLinearGradient = CreateLinearGradient
-----------------------------------------------------------------------------
createPattern :: Canvas ()
createPattern = CreatePattern
-----------------------------------------------------------------------------
createRadialGradient :: Canvas ()
createRadialGradient = CreateRadialGradient
-----------------------------------------------------------------------------
fillStyle :: Color -> Canvas ()
fillStyle color = (FillStyle (renderColor color))
-----------------------------------------------------------------------------
lineCap :: Canvas ()
lineCap = LineCap
-----------------------------------------------------------------------------
lineJoin :: Canvas ()
lineJoin = LineJoin
-----------------------------------------------------------------------------
lineWidth :: Canvas ()
lineWidth = LineWidth
-----------------------------------------------------------------------------
miterLimit :: Canvas ()
miterLimit = MiterLimit
-----------------------------------------------------------------------------
shadowBlur :: Canvas ()
shadowBlur = ShadowBlur
-----------------------------------------------------------------------------
shadowColor :: Canvas ()
shadowColor = ShadowColor
-----------------------------------------------------------------------------
shadowOffsetX :: Canvas ()
shadowOffsetX = ShadowOffsetX
-----------------------------------------------------------------------------
shadowOffsetY :: Canvas ()
shadowOffsetY = ShadowOffsetY
-----------------------------------------------------------------------------
strokeStyle :: Color -> Canvas ()
strokeStyle color = (StrokeStyle (renderColor color))
-----------------------------------------------------------------------------
scale :: Canvas ()
scale = Scale
-----------------------------------------------------------------------------
rotate :: Double -> Canvas ()
rotate = Rotate
-----------------------------------------------------------------------------
translate :: Coord -> Canvas ()
translate coord = (Translate coord)
-----------------------------------------------------------------------------
transform :: Canvas ()
transform = Transform
-----------------------------------------------------------------------------
setTransform :: Canvas ()
setTransform = SetTransform
-----------------------------------------------------------------------------
drawImage :: Image -> Coord -> Canvas ()
drawImage = DrawImage
-----------------------------------------------------------------------------
drawImage' :: Image -> (Double, Double, Double, Double) -> Canvas ()
drawImage' = DrawImage'
-----------------------------------------------------------------------------
createImageData :: Canvas ()
createImageData = CreateImageData
-----------------------------------------------------------------------------
getImageData :: Canvas ()
getImageData = GetImageData
-----------------------------------------------------------------------------
imageDataData :: Canvas ()
imageDataData = ImageDataData
-----------------------------------------------------------------------------
imageDataHeight :: Canvas ()
imageDataHeight = ImageDataHeight
-----------------------------------------------------------------------------
imageDataWidth :: Canvas ()
imageDataWidth = ImageDataWidth
-----------------------------------------------------------------------------
putImageData :: Canvas ()
putImageData = PutImageData
-----------------------------------------------------------------------------
globalAlpha :: Canvas ()
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
createEvent :: Canvas ()
createEvent = CreateEvent
-----------------------------------------------------------------------------
getContext :: Canvas ()
getContext = GetContext
-----------------------------------------------------------------------------
toDataUrl :: Canvas ()
toDataUrl = ToDataURL
-----------------------------------------------------------------------------
