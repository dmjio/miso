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
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Canvas" is a typed Haskell wrapper around the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API HTML5 Canvas 2D API>.
-- It lets you draw graphics imperatively inside a miso 'Miso.Types.View'
-- without leaving Haskell.
--
-- The central abstraction is the 'Canvas' monad:
--
-- @
-- type 'Canvas' a = 'Control.Monad.Reader.ReaderT' 'CanvasContext2D' IO a
-- @
--
-- Every drawing operation ('fillRect', 'arc', 'fillText', …) is a 'Canvas'
-- action that reads the implicit
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D CanvasRenderingContext2D>
-- and calls the corresponding JavaScript method.
--
-- = Quick start
--
-- Wire a canvas element into your view using 'canvas'. The runtime calls
-- @init@ once when the DOM node is created and @draw@ after every VDOM
-- update, passing the state returned by @init@:
--
-- @
-- import           "Miso"
-- import           "Miso.Canvas"
-- import qualified "Miso.CSS"       as CSS
-- import qualified "Miso.CSS.Color" as Color
-- import qualified "Miso.Html.Property" as HP
--
-- view :: Model -> 'Miso.Types.View' context props Model Action
-- view m =
--   'canvas'
--     [ HP.'Miso.Html.Property.width_' \"800\", HP.'Miso.Html.Property.height_' \"480\" ]
--     (\\_ -> pure ())          -- init: no canvas-level state needed
--     (\\() -> drawScene m)    -- draw: closure over current model
--
-- drawScene :: Model -> 'Canvas' ()
-- drawScene m = do
--   'clearRect' (0, 0, 800, 480)
--   'fillStyle' ('color' Color.'Miso.CSS.Color.cornflowerblue')
--   'fillRect'  (0, 0, 800, 480)
--   'fillStyle' ('color' Color.'Miso.CSS.Color.white')
--   'font'      \"24px sans-serif\"
--   'fillText'  (\"Hello, miso!\", 32, 48)
-- @
--
-- = canvas vs canvas_
--
-- Two element constructors are provided:
--
-- * 'canvas' — the standard variant. Acquires a @\"2d\"@
--   'CanvasContext2D' automatically and runs @init@ \/ @draw@ inside
--   the 'Canvas' monad.
--
-- * 'canvas_' — the escape hatch. @init@ and @draw@ receive raw 'IO'
--   callbacks and a 'Miso.DSL.DOMRef', letting you hand the element off to
--   a third-party JavaScript library (e.g. Three.js, WebGL) that manages
--   its own context.
--
-- = Styling
--
-- 'fillStyle' and 'strokeStyle' accept a 'StyleArg', which can be a plain
-- 'Miso.CSS.Color.Color' (via 'color'), a t'Gradient' (via 'gradient'), or a
-- t'Pattern' (via 'pattern_'):
--
-- @
-- 'fillStyle' ('color' Color.'Miso.CSS.Color.red')
-- 'fillStyle' ('gradient' myGradient)
-- 'fillStyle' ('pattern_' myPattern)
-- @
--
-- Note: @'Miso.Canvas.color'@ and @'Miso.CSS.color'@ have the same name but
-- different types. Import "Miso.CSS" qualified to avoid ambiguity when using
-- both in the same file.
--
-- = See also
--
-- * "Miso.CSS.Color" — 'Miso.CSS.Color.Color' type and named colors
-- * "Miso.CSS" — CSS property DSL for non-canvas styling
-- * "Miso.FFI" — lower-level JS interop used internally
-----------------------------------------------------------------------------
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
  , set
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
#ifdef __MHS__
import Prelude hiding (setField)
#endif
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
-----------------------------------------------------------------------------
import           Miso.DSL hiding (call)
import qualified Miso.Canvas.FFI as C
import qualified Miso.FFI as FFI
import           Miso.FFI (Image)
import           Miso.Types
import           Miso.CSS (Color (..), renderColor)
-----------------------------------------------------------------------------
-- | Another variant of canvas, this is not specialized to 'ReaderT'. This is
-- useful when building applications with three.js, or other libraries where
-- explicit context is not necessary.
canvas_
  :: forall context props model action canvasState
   . (FromJSVal canvasState, ToJSVal canvasState)
  => [ Attribute model action ]
  -> (DOMRef -> IO canvasState)
  -- ^ Init function, takes @DOMRef@ as arg, returns canvas init. state.
  -> (canvasState -> IO ())
  -- ^ Callback to render graphics using this canvas' context, takes init state as arg.
  -> View context props model action
canvas_ attributes initialize_ draw_ = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute model action ]
    attrs = On initCallback : On drawCallack : attributes

    initCallback _ _ (VTree vtree) _ _ =
      flip (FFI.set "onCreated") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          initialState <- initialize_ domRef
          FFI.set "state" initialState (Object domRef)

    drawCallack _ _ (VTree vtree) _ _ =
      flip (FFI.set "draw") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          state <- fromJSValUnchecked =<< domRef ! ("state" :: MisoString)
          draw_ state
-----------------------------------------------------------------------------
-- | Element for drawing on a [\<canvas\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas).
-- This function abstracts over the context and interpret callback,
-- including dimension ("2d" or "3d") canvas.
canvas
  :: forall context props model action canvasState
   . (FromJSVal canvasState, ToJSVal canvasState)
  => [ Attribute model action ]
  -> (DOMRef -> Canvas canvasState)
  -- ^ Init function, takes @DOMRef@ as arg, returns canvas init. state.
  -> (canvasState -> Canvas ())
  -- ^ Callback to render graphics using this canvas' context, takes init state as arg.
  -> View context props model action
canvas attributes initialize draw = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute model action ]
    attrs = On initCallback : On drawCallack : attributes

    initCallback _ _ (VTree vtree) _ _ =
      flip (FFI.set "onCreated") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
          initialState <- runReaderT (initialize domRef) ctx
          FFI.set "state" initialState (Object domRef)

    drawCallack _ _ (VTree vtree) _ _ =
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
renderStyleArg :: StyleArg -> IO JSVal
renderStyleArg = \case
  ColorArg c -> toJSVal (renderColor c)
  GradientArg g -> toJSVal g
  PatternArg p -> toJSVal p
-----------------------------------------------------------------------------
instance ToArgs StyleArg where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal StyleArg where
  toJSVal = renderStyleArg
-----------------------------------------------------------------------------
-- | Pretty-prints a t'PatternType' as 'Miso.String.MisoString'
renderPattern :: PatternType -> MisoString
renderPattern = \case
  Repeat -> "repeat"
  RepeatX -> "repeat-x"
  RepeatY -> "repeat-y"
  NoRepeat -> "no-repeat"
-----------------------------------------------------------------------------
-- | [LineCap](https://www.w3schools.com/tags/canvas_linecap.asp)
data LineCapType
  = LineCapButt
  | LineCapRound
  | LineCapSquare
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToArgs LineCapType where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal LineCapType where
  toJSVal = toJSVal . renderLineCapType
-----------------------------------------------------------------------------
-- | Pretty-printing for 'LineCapType'
renderLineCapType :: LineCapType -> MisoString
renderLineCapType = \case
  LineCapButt -> "butt"
  LineCapRound -> "round"
  LineCapSquare -> "square"
-----------------------------------------------------------------------------
-- | [LineJoin](https://www.w3schools.com/tags/canvas_linejoin.asp)
data LineJoinType
  = LineJoinBevel
  | LineJoinRound
  | LineJoinMiter
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToArgs LineJoinType where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal LineJoinType where
  toJSVal = toJSVal . renderLineJoinType
-----------------------------------------------------------------------------
-- | Pretty-print a 'LineJoinType'
renderLineJoinType :: LineJoinType -> MisoString
renderLineJoinType = \case
 LineJoinBevel -> "bevel"
 LineJoinRound -> "round"
 LineJoinMiter -> "miter"
-----------------------------------------------------------------------------
-- | Left-to-right, right-to-left, or inherit direction type.
data DirectionType
  = LTR
  | RTL
  | Inherit
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToArgs DirectionType where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal DirectionType where
  toJSVal = toJSVal . renderDirectionType
-----------------------------------------------------------------------------
-- | Pretty-printing for 'DirectionType'
renderDirectionType :: DirectionType -> MisoString
renderDirectionType = \case
  LTR -> "ltr"
  RTL -> "rtl"
  Inherit -> "inherit"
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
instance ToArgs TextAlignType where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal TextAlignType where
  toJSVal = toJSVal . renderTextAlignType
-----------------------------------------------------------------------------
-- | Pretty-print 'TextAlignType'
renderTextAlignType :: TextAlignType -> MisoString
renderTextAlignType = \case
  TextAlignCenter -> "center"
  TextAlignEnd -> "end"
  TextAlignLeft -> "left"
  TextAlignRight -> "right"
  TextAlignStart -> "start"
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
instance ToArgs TextBaselineType where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal TextBaselineType where
  toJSVal = toJSVal . renderTextBaselineType
-----------------------------------------------------------------------------
-- | Pretty-printing for 'TextBaselineType'
renderTextBaselineType :: TextBaselineType -> MisoString
renderTextBaselineType = \case
  TextBaselineAlphabetic -> "alphabetic"
  TextBaselineTop -> "top"
  TextBaselineHanging -> "hanging"
  TextBaselineMiddle -> "middle"
  TextBaselineIdeographic -> "ideographic"
  TextBaselineBottom -> "bottom"
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
instance ToArgs CompositeOperation where
  toArgs arg = (:[]) <$> toJSVal arg
-----------------------------------------------------------------------------
instance ToJSVal CompositeOperation where
  toJSVal = toJSVal . renderCompositeOperation
-----------------------------------------------------------------------------
-- | Pretty-print a 'CompositeOperation'
renderCompositeOperation :: CompositeOperation -> MisoString
renderCompositeOperation = \case
  SourceOver -> "source-over"
  SourceAtop -> "source-atop"
  SourceIn -> "source-in"
  SourceOut -> "source-out"
  DestinationOver -> "destination-over"
  DestinationAtop -> "destination-atop"
  DestinationIn -> "destination-in"
  DestinationOut -> "destination-out"
  Lighter -> "lighter"
  Copy -> "copy"
  Xor -> "xor"
-----------------------------------------------------------------------------
-- | Type used to hold a canvas Pattern
newtype Pattern = Pattern JSVal deriving (ToJSVal)
-----------------------------------------------------------------------------
instance FromJSVal Pattern where
  fromJSVal = pure . pure . Pattern
-----------------------------------------------------------------------------
-- | Type used to hold a Gradient
newtype Gradient = Gradient JSVal deriving (ToJSVal, ToArgs)
-----------------------------------------------------------------------------
instance FromJSVal Gradient where
  fromJSVal = pure . pure . Gradient
-----------------------------------------------------------------------------
-- | Type used to hold t'ImageData'
newtype ImageData = ImageData JSVal deriving (ToJSVal, ToObject)
-----------------------------------------------------------------------------
instance ToArgs ImageData where
  toArgs args = (:[]) <$> toJSVal args
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
call :: (FromJSVal a, ToArgs args) => MisoString -> args -> Canvas a
call name arg = do
  ctx <- ask
  liftIO $ fromJSValUnchecked =<< do
    ctx # name $ arg
-----------------------------------------------------------------------------
-- | Run a direct import ("Miso.Canvas.FFI") against the context.
ctxIO :: (CanvasContext2D -> IO ()) -> Canvas ()
ctxIO f = ask >>= liftIO . f
-----------------------------------------------------------------------------
-- | Property setter specialized to t'Canvas'.
--
-- @
-- globalCompositeOperation :: CompositeOperation -> Canvas ()
-- globalCompositeOperation = set "globalCompositeOperation"
-- @
--
set :: ToArgs args => MisoString -> args -> Canvas ()
set name args = do
  ctx <- ask
  liftIO $ setField ctx name (toArgs args)
-----------------------------------------------------------------------------
-- | DSL for expressing operations on 'canvas_'
type Canvas a = ReaderT CanvasContext2D IO a
-----------------------------------------------------------------------------
-- | [ctx.globalCompositeOperation = "source-over"](https://www.w3schools.com/tags/canvas_globalcompositeoperation.asp)
globalCompositeOperation :: CompositeOperation -> Canvas ()
globalCompositeOperation v = ctxIO $ \ctx -> C.globalCompositeOperation ctx (C.jsString (renderCompositeOperation v))
-----------------------------------------------------------------------------
-- | [ctx.clearRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_clearrect.asp)
clearRect :: (Double, Double, Double, Double) -> Canvas ()
clearRect (a, b, c, d) = ctxIO $ \ctx -> C.clearRect ctx a b c d
-----------------------------------------------------------------------------
-- | [ctx.fillRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_fillrect.asp)
fillRect :: (Double, Double, Double, Double) -> Canvas ()
fillRect (a, b, c, d) = ctxIO $ \ctx -> C.fillRect ctx a b c d
-----------------------------------------------------------------------------
-- | [ctx.strokeRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_strokerect.asp)
strokeRect :: (Double, Double, Double, Double) -> Canvas ()
strokeRect (a, b, c, d) = ctxIO $ \ctx -> C.strokeRect ctx a b c d
-----------------------------------------------------------------------------
-- | [ctx.beginPath()](https://www.w3schools.com/tags/canvas_beginpath.asp)
beginPath :: () -> Canvas ()
beginPath () = ctxIO C.beginPath
-----------------------------------------------------------------------------
-- | [ctx.closePath()](https://www.w3schools.com/tags/canvas_closepath.asp)
closePath :: () -> Canvas ()
closePath () = ctxIO C.closePath
-----------------------------------------------------------------------------
-- | [ctx.moveTo(x,y)](https://www.w3schools.com/tags/canvas_moveto.asp)
moveTo :: Coord -> Canvas ()
moveTo (a, b) = ctxIO $ \ctx -> C.moveTo ctx a b
-----------------------------------------------------------------------------
-- | [ctx.lineTo(x,y)](https://www.w3schools.com/tags/canvas_lineto.asp)
lineTo :: Coord -> Canvas ()
lineTo (a, b) = ctxIO $ \ctx -> C.lineTo ctx a b
-----------------------------------------------------------------------------
-- | [ctx.fill()](https://www.w3schools.com/tags/canvas_fill.asp)
fill :: () -> Canvas ()
fill () = ctxIO C.fill
-----------------------------------------------------------------------------
-- | [ctx.rect(x,y,width,height)](https://www.w3schools.com/tags/canvas_rect.asp)
rect :: (Double, Double, Double, Double) -> Canvas ()
rect (a, b, c, d) = ctxIO $ \ctx -> C.rect ctx a b c d
-----------------------------------------------------------------------------
-- | [ctx.stroke()](https://www.w3schools.com/tags/canvas_stroke.asp)
stroke :: () -> Canvas ()
stroke () = ctxIO C.stroke
-----------------------------------------------------------------------------
-- | [ctx.bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y)](https://www.w3schools.com/tags/canvas_beziercurveto.asp)
bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
bezierCurveTo (a, b, c, d, e, f) = ctxIO $ \ctx -> C.bezierCurveTo ctx a b c d e f
-----------------------------------------------------------------------------
-- | [context.arc(x, y, r, sAngle, eAngle, counterclockwise)](https://www.w3schools.com/tags/canvas_arc.asp)
arc :: (Double, Double, Double, Double, Double) -> Canvas ()
arc (a, b, c, d, e) = ctxIO $ \ctx -> C.arc ctx a b c d e
-----------------------------------------------------------------------------
-- | [context.arcTo(x1, y1, x2, y2, r)](https://www.w3schools.com/tags/canvas_arcto.asp)
arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
arcTo (a, b, c, d, e) = ctxIO $ \ctx -> C.arcTo ctx a b c d e
-----------------------------------------------------------------------------
-- | [context.quadraticCurveTo(cpx,cpy,x,y)](https://www.w3schools.com/tags/canvas_quadraticcurveto.asp)
quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
quadraticCurveTo (a, b, c, d) = ctxIO $ \ctx -> C.quadraticCurveTo ctx a b c d
-----------------------------------------------------------------------------
-- | [context.direction = "ltr"](https://www.w3schools.com/tags/canvas_direction.asp)
direction :: DirectionType -> Canvas ()
direction v = ctxIO $ \ctx -> C.direction ctx (C.jsString (renderDirectionType v))
-----------------------------------------------------------------------------
-- | [context.fillText(text,x,y)](https://www.w3schools.com/tags/canvas_filltext.asp)
fillText :: (MisoString, Double, Double) -> Canvas ()
fillText (s, x, y) = ctxIO $ \ctx -> C.fillText ctx (C.jsString s) x y
-----------------------------------------------------------------------------
-- | [context.font = "italic small-caps bold 12px arial"](https://www.w3schools.com/tags/canvas_font.asp)
font :: MisoString -> Canvas ()
font f = ctxIO $ \ctx -> C.font ctx (C.jsString f)
-----------------------------------------------------------------------------
-- | [ctx.strokeText()](https://www.w3schools.com/tags/canvas_stroketext.asp)
strokeText :: (MisoString, Double, Double) -> Canvas ()
strokeText (s, x, y) = ctxIO $ \ctx -> C.strokeText ctx (C.jsString s) x y
-----------------------------------------------------------------------------
-- | [ctx.textAlign = "start"](https://www.w3schools.com/tags/canvas_textalign.asp)
textAlign :: TextAlignType -> Canvas ()
textAlign v = ctxIO $ \ctx -> C.textAlign ctx (C.jsString (renderTextAlignType v))
-----------------------------------------------------------------------------
-- | [ctx.textBaseline = "top"](https://www.w3schools.com/tags/canvas_textBaseLine.asp)
textBaseline :: TextBaselineType -> Canvas ()
textBaseline v = ctxIO $ \ctx -> C.textBaseline ctx (C.jsString (renderTextBaselineType v))
-----------------------------------------------------------------------------
-- | [gradient.addColorStop(stop,color)](https://www.w3schools.com/tags/canvas_addcolorstop.asp)
addColorStop
  :: (Double, Color)
  -- ^ @(stop, color)@ — position along the gradient (0.0–1.0) and the colour at that stop
  -> Gradient
  -- ^ The gradient object to add the colour stop to
  -> Canvas ()
addColorStop args (Gradient g) = do
  _ <- liftIO $ g # ("addColorStop" :: MisoString) $ args
  pure ()
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
fillStyle (ColorArg (RGB r g b)) = ctxIO $ \ctx -> C.fillStyleRGB ctx r g b
fillStyle (ColorArg (RGBA r g b a)) = ctxIO $ \ctx -> C.fillStyleRGBA ctx r g b a
fillStyle arg = ctxIO $ \ctx -> toJSVal arg >>= C.fillStyle ctx
-----------------------------------------------------------------------------
-- | [ctx.lineCap = "butt"](https://www.w3schools.com/tags/canvas_lineCap.asp)
lineCap :: LineCapType -> Canvas ()
lineCap v = ctxIO $ \ctx -> C.lineCap ctx (C.jsString (renderLineCapType v))
-----------------------------------------------------------------------------
-- | [ctx.lineJoin = "bevel"](https://www.w3schools.com/tags/canvas_lineJoin.asp)
lineJoin :: LineJoinType -> Canvas ()
lineJoin v = ctxIO $ \ctx -> C.lineJoin ctx (C.jsString (renderLineJoinType v))
-----------------------------------------------------------------------------
-- | [ctx.lineWidth = 10](https://www.w3schools.com/tags/canvas_lineWidth.asp)
lineWidth :: Double -> Canvas ()
lineWidth v = ctxIO $ \ctx -> C.lineWidth ctx v
-----------------------------------------------------------------------------
-- | [ctx.miterLimit = 10](https://www.w3schools.com/tags/canvas_miterLimit.asp)
miterLimit :: Double -> Canvas ()
miterLimit v = ctxIO $ \ctx -> C.miterLimit ctx v
-----------------------------------------------------------------------------
-- | [ctx.shadowBlur = 10](https://www.w3schools.com/tags/canvas_shadowBlur.asp)
shadowBlur :: Double -> Canvas ()
shadowBlur v = ctxIO $ \ctx -> C.shadowBlur ctx v
-----------------------------------------------------------------------------
-- | [ctx.shadowColor = "red"](https://www.w3schools.com/tags/canvas_shadowColor.asp)
shadowColor :: Color -> Canvas ()
shadowColor (RGB r g b) = ctxIO $ \ctx -> C.shadowColorRGB ctx r g b
shadowColor (RGBA r g b a) = ctxIO $ \ctx -> C.shadowColorRGBA ctx r g b a
shadowColor v = ctxIO $ \ctx -> C.shadowColor ctx (C.jsString (renderColor v))
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetX = 20](https://www.w3schools.com/tags/canvas_shadowOffsetX.asp)
shadowOffsetX :: Double -> Canvas ()
shadowOffsetX v = ctxIO $ \ctx -> C.shadowOffsetX ctx v
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetY = 20](https://www.w3schools.com/tags/canvas_shadowOffsetY.asp)
shadowOffsetY :: Double -> Canvas ()
shadowOffsetY v = ctxIO $ \ctx -> C.shadowOffsetY ctx v
-----------------------------------------------------------------------------
-- | [ctx.strokeStyle = "red"](https://www.w3schools.com/tags/canvas_strokeStyle.asp)
strokeStyle :: StyleArg -> Canvas ()
strokeStyle (ColorArg (RGB r g b)) = ctxIO $ \ctx -> C.strokeStyleRGB ctx r g b
strokeStyle (ColorArg (RGBA r g b a)) = ctxIO $ \ctx -> C.strokeStyleRGBA ctx r g b a
strokeStyle arg = ctxIO $ \ctx -> toJSVal arg >>= C.strokeStyle ctx
-----------------------------------------------------------------------------
-- | [ctx.scale(width,height)](https://www.w3schools.com/tags/canvas_scale.asp)
scale :: (Double, Double) -> Canvas ()
scale (a, b) = ctxIO $ \ctx -> C.scale ctx a b
-----------------------------------------------------------------------------
-- | [ctx.rotate(angle)](https://www.w3schools.com/tags/canvas_rotate.asp)
rotate :: Double -> Canvas ()
rotate a = ctxIO $ \ctx -> C.rotate ctx a
-----------------------------------------------------------------------------
-- | [ctx.translate(angle)](https://www.w3schools.com/tags/canvas_translate.asp)
translate :: Coord -> Canvas ()
translate (a, b) = ctxIO $ \ctx -> C.translate ctx a b
-----------------------------------------------------------------------------
-- | [ctx.transform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_transform.asp)
transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
transform (a, b, c, d, e, f) = ctxIO $ \ctx -> C.transform ctx a b c d e f
-----------------------------------------------------------------------------
-- | [ctx.setTransform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_setTransform.asp)
setTransform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
setTransform (a, b, c, d, e, f) = ctxIO $ \ctx -> C.setTransform ctx a b c d e f
----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage :: (Image, Double, Double) -> Canvas ()
drawImage (img, x, y) = ctxIO $ \ctx -> toJSVal img >>= \i -> C.drawImage ctx i x y
-----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage' :: (Image, Double, Double, Double, Double) -> Canvas ()
drawImage' (img, x, y, w, h) = ctxIO $ \ctx -> toJSVal img >>= \i -> C.drawImage4 ctx i x y w h
-----------------------------------------------------------------------------
-- | [ctx.createImageData(width,height)](https://www.w3schools.com/tags/canvas_createImageData.asp)
createImageData :: (Double, Double) -> Canvas ImageData
createImageData = call "createImageData"
-----------------------------------------------------------------------------
-- | [ctx.getImageData(w,x,y,z)](https://www.w3schools.com/tags/canvas_getImageData.asp)
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData = call "getImageData"
-----------------------------------------------------------------------------
-- | [imageData.data\[index\] = 255](https://www.w3schools.com/tags/canvas_imagedata_data.asp)
setImageData :: (ImageData, Int, Double) -> Canvas ()
setImageData (imgData, index, value) = liftIO $ do
   o <- imgData ! ("data" :: MisoString)
   (o <## index) value
-----------------------------------------------------------------------------
-- | [imageData.height](https://www.w3schools.com/tags/canvas_imagedata_height.asp)
height :: ImageData -> Canvas Double
height (ImageData imgData) = liftIO $ do
  fromJSValUnchecked =<< imgData ! ("height" :: MisoString)
-----------------------------------------------------------------------------
-- | [imageData.width](https://www.w3schools.com/tags/canvas_imagedata_width.asp)
width :: ImageData -> Canvas Double
width (ImageData imgData) = liftIO $
  fromJSValUnchecked =<< imgData ! ("width" :: MisoString)
-----------------------------------------------------------------------------
-- | [ctx.putImageData(imageData,x,y)](https://www.w3schools.com/tags/canvas_putImageData.asp)
putImageData :: (ImageData, Double, Double) -> Canvas ()
putImageData (d, x, y) = ctxIO $ \ctx -> toJSVal d >>= \v -> C.putImageData ctx v x y
-----------------------------------------------------------------------------
-- | [ctx.globalAlpha = 0.2](https://www.w3schools.com/tags/canvas_globalAlpha.asp)
globalAlpha :: Double -> Canvas ()
globalAlpha v = ctxIO $ \ctx -> C.globalAlpha ctx v
-----------------------------------------------------------------------------
-- | [ctx.clip()](https://www.w3schools.com/tags/canvas_clip.asp)
clip :: () -> Canvas ()
clip () = ctxIO C.clip
-----------------------------------------------------------------------------
-- | [ctx.save()](https://www.w3schools.com/tags/canvas_save.asp)
save :: () -> Canvas ()
save () = ctxIO C.save
-----------------------------------------------------------------------------
-- | [ctx.restore()](https://www.w3schools.com/tags/canvas_restore.asp)
restore :: () -> Canvas ()
restore () = ctxIO C.restore
-----------------------------------------------------------------------------
