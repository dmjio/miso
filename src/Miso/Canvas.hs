-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
#ifdef WASM
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultilineStrings #-}
#endif
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
-- = Batching (WASM backend)
--
-- On the WASM backend, 'Canvas' actions that take only numeric\/enum
-- arguments (path construction, transforms, @save@\/@restore@, numeric
-- properties, and colors) are not sent to JavaScript one call at a time.
-- Instead they're queued into a single growable buffer, and replayed
-- against the real canvas context in one FFI call at the end of @draw@ (or
-- whenever an operation that genuinely can't be queued — drawing text, or
-- reading pixel data back — needs to run). This is purely an internal
-- performance optimization; it has no effect on the public API or on
-- drawing order. Other backends (GHCJS, vanilla GHC) are unaffected and
-- keep issuing one call per operation, since they have no linear memory to
-- batch into.
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
-- view :: Model -> 'Miso.Types.View' Model Action
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
--   its own context. 'canvas_' never touches the 'Canvas' monad, so the
--   batching described above has no bearing on it whatsoever.
--
-- = Styling
--
-- 'fillStyle' and 'strokeStyle' accept a 'StyleArg', which can be a plain
-- 'Miso.CSS.Color.Color' (via 'color'), a 'Gradient' (via 'gradient'), or a
-- 'Pattern' (via 'pattern_'):
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
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, runReaderT, ask)
#ifdef WASM
import           Control.Monad (when)
import           Data.IORef
import           Foreign.Marshal.Alloc (mallocBytes, reallocBytes, free)
import           Foreign.Ptr (Ptr, wordPtrToPtr, ptrToWordPtr)
import           Foreign.Storable (pokeElemOff, sizeOf)
#endif
-----------------------------------------------------------------------------
import           Miso.DSL hiding (call)
import qualified Miso.FFI as FFI
import           Miso.FFI (Image (..))
import           Miso.Types
import           Miso.CSS (Color (RGBA, RGB, Hex), renderColor)
#ifdef WASM
import qualified Miso.String as MS
#endif
-----------------------------------------------------------------------------
-- | Another variant of canvas, this is not specialized to 'ReaderT'. This is
-- useful when building applications with three.js, or other libraries where
-- explicit context is not necessary.
--
-- 'canvas_' never constructs a 'Canvas' value, so the batching described in
-- the module overview does not apply here — every call your @init@\/@draw@
-- callbacks make is whatever it already was (e.g. plain FFI, or a
-- third-party library's own API).
canvas_
  :: forall model action canvasState
   . (FromJSVal canvasState, ToJSVal canvasState)
  => [ Attribute action ]
  -> (DOMRef -> IO canvasState)
  -- ^ Init function, takes 'DOMRef' as arg, returns canvas init. state.
  -> (canvasState -> IO ())
  -- ^ Callback to render graphics using this canvas' context, takes init state as arg.
  -> View model action
canvas_ attributes initialize_ draw_ = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute action ]
    attrs = initCallback : drawCallack : attributes

    initCallback :: Attribute action
    initCallback = On $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "onCreated") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          initialState <- initialize_ domRef
          FFI.set "state" initialState (Object domRef)

    drawCallack :: Attribute action
    drawCallack = On $ \_ (VTree vtree) _ _ -> do
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
#ifdef WASM
    attrs = initCallback : drawCallack : destroyCallback : attributes
#else
    attrs = initCallback : drawCallack : attributes
#endif

    initCallback :: Attribute action
    initCallback = On $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "onCreated") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
#ifdef WASM
          initialState <- withCanvasEnv domRef (runReaderT (initialize domRef))
#else
          ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
          initialState <- runReaderT (initialize domRef) ctx
#endif
          FFI.set "state" initialState (Object domRef)

    drawCallack :: Attribute action
    drawCallack = On $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "draw") vtree =<< do
        FFI.syncCallback1 $ \domRef -> do
          jval <- domRef ! ("state" :: MisoString)
          initialState <- fromJSValUnchecked jval
#ifdef WASM
          _ <- withCanvasEnv domRef (runReaderT (draw initialState))
          pure ()
#else
          ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
          runReaderT (draw initialState) ctx
#endif

#ifdef WASM
    -- | Frees this canvas's batch buffer once the DOM node is removed. The
    -- buffer is plain 'mallocBytes'-allocated linear memory, not tracked by
    -- the Haskell GC, so it must be released explicitly.
    destroyCallback :: Attribute action
    destroyCallback = On $ \_ (VTree vtree) _ _ -> do
      flip (FFI.set "onDestroyed") vtree =<< do
        FFI.syncCallback $ do
          domRef <- vtree ! ("domRef" :: MisoString)
          hasBuf <- not <$> (isUndefined =<< domRef ! bufPtrKey)
          when hasBuf $ do
            p <- fromJSValUnchecked =<< domRef ! bufPtrKey
            free (wordPtrToPtr (fromIntegral (p :: Int)))
#endif
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
  deriving (Show, Eq, Enum)
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
  deriving (Show, Eq, Enum)
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
  deriving (Show, Eq, Enum)
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
  deriving (Show, Eq, Enum)
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
  deriving (Show, Eq, Enum)
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
  deriving (Show, Eq, Enum)
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
#ifdef WASM
-- | Per-canvas environment used to batch draw calls on the WASM backend.
-- @ceBufPtr@\/@ceBufCap@ describe the buffer's current address and capacity
-- (in 'Double' slots); @ceBufLen@ is how much of it is currently queued.
-- This type is entirely internal — it never appears in any exported
-- signature, so it has no bearing on the public API.
data CanvasEnv = CanvasEnv
  { ceContext :: !CanvasContext2D
  , ceBufPtr  :: !(IORef (Ptr Double))
  , ceBufCap  :: !(IORef Int)
  , ceBufLen  :: !(IORef Int)
  }
#else
-- | On non-WASM backends there is no linear memory to batch into, so the
-- environment is just the context, exactly as before.
type CanvasEnv = CanvasContext2D
#endif
-----------------------------------------------------------------------------
-- | DSL for expressing operations on 'canvas_'
type Canvas a = ReaderT CanvasEnv IO a
-----------------------------------------------------------------------------
-- | Retrieves the underlying t'CanvasContext2D' from the 'Canvas'
-- environment, regardless of backend.
askCtx :: Canvas CanvasContext2D
#ifdef WASM
askCtx = ceContext <$> ask
#else
askCtx = ask
#endif
-----------------------------------------------------------------------------
call :: (FromJSVal a, ToArgs args) => MisoString -> args -> Canvas a
call name arg = do
  ctx <- askCtx
  liftIO $ fromJSValUnchecked =<< do
    ctx # name $ arg
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
  ctx <- askCtx
  liftIO $ setField ctx name (toArgs args)
-----------------------------------------------------------------------------
#ifdef WASM
-----------------------------------------------------------------------------
-- | Every batchable canvas operation, in a fixed order. 'fromEnum' turns a
-- constructor into the opcode written to the batch buffer; the JavaScript
-- interpreter in 'flushCanvasOps_ffi' switches on the exact same order, so
-- the two must never be changed independently of one another.
data Op
  = OpBeginPath
  | OpClosePath
  | OpFill
  | OpStroke
  | OpSave
  | OpRestore
  | OpClip
  | OpMoveTo
  | OpLineTo
  | OpRect
  | OpClearRect
  | OpFillRect
  | OpStrokeRect
  | OpBezierCurveTo
  | OpArc
  | OpArcTo
  | OpQuadraticCurveTo
  | OpScale
  | OpRotate
  | OpTranslate
  | OpTransform
  | OpSetTransform
  | OpGlobalAlpha
  | OpLineWidth
  | OpMiterLimit
  | OpShadowBlur
  | OpShadowOffsetX
  | OpShadowOffsetY
  | OpGlobalCompositeOperation
  | OpDirection
  | OpLineCap
  | OpLineJoin
  | OpTextAlign
  | OpTextBaseline
  | OpSetColorStyle
  | OpSetHandleStyle
  | OpDrawImage
  | OpDrawImage2
  deriving (Enum)
-----------------------------------------------------------------------------
opCode :: Op -> Double
opCode = fromIntegral . fromEnum
-----------------------------------------------------------------------------
-- | Number of 'Double' slots a fresh canvas buffer starts with (2KB). Grows
-- by doubling via 'reallocBytes' whenever a batch needs more room.
initialCapacity :: Int
initialCapacity = 256
-----------------------------------------------------------------------------
allocBuffer :: Int -> IO (Ptr Double)
allocBuffer cap = mallocBytes (cap * sizeOf (0 :: Double))
-----------------------------------------------------------------------------
-- | Ensure the buffer has room for @n@ more 'Double's plus the eventual
-- @-1@ terminator, growing (doubling) via 'reallocBytes' if necessary.
ensureCapacity :: Int -> Canvas ()
ensureCapacity n = do
  env <- ask
  liftIO $ do
    cap <- readIORef (ceBufCap env)
    len <- readIORef (ceBufLen env)
    when (len + n + 1 > cap) $ do
      let needed = len + n + 1
          growCap c = if c >= needed then c else growCap (c * 2)
          newCap = growCap (max 1 cap)
      ptr <- readIORef (ceBufPtr env)
      ptr' <- reallocBytes ptr (newCap * sizeOf (0 :: Double))
      writeIORef (ceBufPtr env) ptr'
      writeIORef (ceBufCap env) newCap
-----------------------------------------------------------------------------
-- | Queue an opcode and its numeric arguments into the batch buffer. This
-- is a pure Haskell memory write — no FFI call happens here.
emit :: Op -> [Double] -> Canvas ()
emit op args = do
  ensureCapacity (1 + length args)
  env <- ask
  liftIO $ do
    ptr <- readIORef (ceBufPtr env)
    len <- readIORef (ceBufLen env)
    pokeElemOff ptr len (opCode op)
    let go _ [] = pure ()
        go i (a:as) = pokeElemOff ptr i a >> go (i + 1) as
    go (len + 1) args
    writeIORef (ceBufLen env) (len + 1 + length args)
-----------------------------------------------------------------------------
-- | Replay every queued operation against the real canvas context in a
-- single FFI call, then reset the buffer so the next batch starts fresh.
-- Called automatically at the end of every @draw@\/@init@, and before any
-- operation that can't be queued (drawing text, reading pixel data back,
-- referencing a CSS variable color, …).
flushCanvas :: Canvas ()
flushCanvas = do
  env <- ask
  liftIO $ do
    len <- readIORef (ceBufLen env)
    when (len > 0) $ do
      ptr <- readIORef (ceBufPtr env)
      pokeElemOff ptr len (-1)
      flushCanvasOps_ffi (ceContext env) ptr len
      writeIORef (ceBufLen env) 0
-----------------------------------------------------------------------------
-- | Interprets a batch of queued canvas operations written into wasm linear
-- memory as a flat array of 'Double's: @[opcode, args..., opcode, args...,
-- …, -1]@. Reads directly from @__exports.memory.buffer@ starting at the
-- given pointer — see 'Foreign.Ptr.Ptr' in GHC's wasm backend, which passes
-- pointers as plain linear-memory offsets (unlike 'JSVal', which is an
-- opaque @externref@ and can never be observed this way).
foreign import javascript unsafe
  """
  const buf = new Float64Array(__exports.memory.buffer, $2, $3 + 1);
  const handles = (globalThis.__misoCanvasHandles ?? []);
  const ctx = $1;
  const styleTarget = (t, v) => {
    if (t === 0) { ctx.fillStyle = v; }
    else if (t === 1) { ctx.strokeStyle = v; }
    else { ctx.shadowColor = v; }
  };
  const compositeOps = ["source-over","source-atop","source-in","source-out","destination-over","destination-atop","destination-in","destination-out","lighter","copy","xor"];
  const directions = ["ltr","rtl","inherit"];
  const lineCaps = ["butt","round","square"];
  const lineJoins = ["bevel","round","miter"];
  const textAligns = ["center","end","left","right","start"];
  const textBaselines = ["alphabetic","top","hanging","middle","ideographic","bottom"];
  let i = 0;
  while (true) {
    const op = buf[i++];
    if (op === -1) break;
    switch (op) {
      case 0: ctx.beginPath(); break;
      case 1: ctx.closePath(); break;
      case 2: ctx.fill(); break;
      case 3: ctx.stroke(); break;
      case 4: ctx.save(); break;
      case 5: ctx.restore(); break;
      case 6: ctx.clip(); break;
      case 7: ctx.moveTo(buf[i], buf[i+1]); i += 2; break;
      case 8: ctx.lineTo(buf[i], buf[i+1]); i += 2; break;
      case 9: ctx.rect(buf[i], buf[i+1], buf[i+2], buf[i+3]); i += 4; break;
      case 10: ctx.clearRect(buf[i], buf[i+1], buf[i+2], buf[i+3]); i += 4; break;
      case 11: ctx.fillRect(buf[i], buf[i+1], buf[i+2], buf[i+3]); i += 4; break;
      case 12: ctx.strokeRect(buf[i], buf[i+1], buf[i+2], buf[i+3]); i += 4; break;
      case 13: ctx.bezierCurveTo(buf[i], buf[i+1], buf[i+2], buf[i+3], buf[i+4], buf[i+5]); i += 6; break;
      case 14: ctx.arc(buf[i], buf[i+1], buf[i+2], buf[i+3], buf[i+4]); i += 5; break;
      case 15: ctx.arcTo(buf[i], buf[i+1], buf[i+2], buf[i+3], buf[i+4]); i += 5; break;
      case 16: ctx.quadraticCurveTo(buf[i], buf[i+1], buf[i+2], buf[i+3]); i += 4; break;
      case 17: ctx.scale(buf[i], buf[i+1]); i += 2; break;
      case 18: ctx.rotate(buf[i]); i += 1; break;
      case 19: ctx.translate(buf[i], buf[i+1]); i += 2; break;
      case 20: ctx.transform(buf[i], buf[i+1], buf[i+2], buf[i+3], buf[i+4], buf[i+5]); i += 6; break;
      case 21: ctx.setTransform(buf[i], buf[i+1], buf[i+2], buf[i+3], buf[i+4], buf[i+5]); i += 6; break;
      case 22: ctx.globalAlpha = buf[i]; i += 1; break;
      case 23: ctx.lineWidth = buf[i]; i += 1; break;
      case 24: ctx.miterLimit = buf[i]; i += 1; break;
      case 25: ctx.shadowBlur = buf[i]; i += 1; break;
      case 26: ctx.shadowOffsetX = buf[i]; i += 1; break;
      case 27: ctx.shadowOffsetY = buf[i]; i += 1; break;
      case 28: ctx.globalCompositeOperation = compositeOps[buf[i]]; i += 1; break;
      case 29: ctx.direction = directions[buf[i]]; i += 1; break;
      case 30: ctx.lineCap = lineCaps[buf[i]]; i += 1; break;
      case 31: ctx.lineJoin = lineJoins[buf[i]]; i += 1; break;
      case 32: ctx.textAlign = textAligns[buf[i]]; i += 1; break;
      case 33: ctx.textBaseline = textBaselines[buf[i]]; i += 1; break;
      case 34: styleTarget(buf[i], `rgba(${buf[i+1]},${buf[i+2]},${buf[i+3]},${buf[i+4]})`); i += 5; break;
      case 35: styleTarget(buf[i], handles[buf[i+1]]); i += 2; break;
      case 36: ctx.drawImage(handles[buf[i]], buf[i+1], buf[i+2]); i += 3; break;
      case 37: ctx.drawImage(handles[buf[i]], buf[i+1], buf[i+2], buf[i+3], buf[i+4]); i += 5; break;
      default: throw new WebAssembly.RuntimeError('Miso.Canvas: bad opcode ' + op);
    }
  }
  """
  flushCanvasOps_ffi :: JSVal -> Ptr Double -> Int -> IO ()
-----------------------------------------------------------------------------
-- | Registers (or, if already registered, looks up) a small integer handle
-- for a live JS object reference — a 'Gradient', 'Pattern', or 'Image' —
-- so that it can be referred to from inside the batch buffer as a plain
-- number instead of needing its own FFI call every time it's used.
--
-- Deduplicated via a JS-side @Map@ keyed on object identity: calling this
-- twice with the same underlying JS object returns the same handle both
-- times, so drawing the same 'Image' every frame registers it exactly
-- once, not once per frame.
--
-- This table is intentionally unmanaged — entries live for the page's
-- lifetime, the same as the handful of gradients\/patterns\/images a
-- typical application creates once and reuses. Don't create fresh
-- 'Gradient's, 'Pattern's, or 'Image's in a per-frame hot loop; create them
-- once (e.g. in a canvas's @init@) and hold onto the result.
foreign import javascript unsafe
  """
  const map = globalThis.__misoCanvasHandleMap || (globalThis.__misoCanvasHandleMap = new Map());
  const arr = globalThis.__misoCanvasHandles || (globalThis.__misoCanvasHandles = []);
  if (!map.has($1)) { map.set($1, arr.push($1) - 1); }
  return map.get($1);
  """
  registerOrGetHandle_ffi :: JSVal -> IO Int
-----------------------------------------------------------------------------
bufPtrKey, bufCapKey :: MisoString
bufPtrKey = "__misoCanvasBufPtr"
bufCapKey = "__misoCanvasBufCap"
-----------------------------------------------------------------------------
-- | Acquires this canvas's persistent batch buffer (allocating one on
-- first use), runs the given action, flushes anything left queued, then
-- persists the buffer's (possibly grown) address\/capacity back onto the
-- DOM node so the next @draw@ can reuse it without reallocating.
withCanvasEnv :: DOMRef -> (CanvasEnv -> IO a) -> IO a
withCanvasEnv domRef action = do
  ctx <- domRef # ("getContext" :: MisoString) $ ["2d" :: MisoString]
  hasBuf <- not <$> (isUndefined =<< domRef ! bufPtrKey)
  (ptr, cap) <-
    if hasBuf
      then do
        p <- fromJSValUnchecked =<< domRef ! bufPtrKey
        c <- fromJSValUnchecked =<< domRef ! bufCapKey
        pure (wordPtrToPtr (fromIntegral (p :: Int)), c)
      else do
        p <- allocBuffer initialCapacity
        pure (p, initialCapacity)
  ptrRef <- newIORef ptr
  capRef <- newIORef cap
  lenRef <- newIORef 0
  let env = CanvasEnv ctx ptrRef capRef lenRef
  result <- action env
  runReaderT flushCanvas env
  ptr' <- readIORef ptrRef
  cap' <- readIORef capRef
  FFI.set bufPtrKey (fromIntegral (ptrToWordPtr ptr') :: Int) (Object domRef)
  FFI.set bufCapKey cap' (Object domRef)
  pure result
-----------------------------------------------------------------------------
-- | Converts a 'Color' to plain @(r, g, b, a)@ components when possible.
-- 'RGBA'\/'RGB' pass straight through; 'Hex' is parsed in pure Haskell.
-- Every other constructor ('HSL', 'HSLA', 'OKLCH', 'OKLCHA', a CSS-variable
-- color) can't be reduced to four numbers without either real color-space
-- conversion math or a live reference to computed style, so those stay on
-- the slow (flush-then-direct-call) path.
colorToRGBA :: Color -> Maybe (Double, Double, Double, Double)
colorToRGBA = \case
  RGBA r g b a -> Just (fromIntegral r, fromIntegral g, fromIntegral b, a)
  RGB  r g b   -> Just (fromIntegral r, fromIntegral g, fromIntegral b, 1)
  Hex str      -> hexToRGBA str
  _            -> Nothing
-----------------------------------------------------------------------------
hexToRGBA :: MisoString -> Maybe (Double, Double, Double, Double)
hexToRGBA raw =
  case dropWhile (== '#') (MS.unpack raw) of
    [r,g,b]             -> assemble [r,r] [g,g] [b,b] "ff"
    [r,g,b,a]           -> assemble [r,r] [g,g] [b,b] [a,a]
    [r1,r2,g1,g2,b1,b2] -> assemble [r1,r2] [g1,g2] [b1,b2] "ff"
    [r1,r2,g1,g2,b1,b2,a1,a2] -> assemble [r1,r2] [g1,g2] [b1,b2] [a1,a2]
    _ -> Nothing
  where
    assemble rs gs bs as = do
      r <- hexByte rs
      g <- hexByte gs
      b <- hexByte bs
      a <- hexByte as
      pure (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a / 255)
-----------------------------------------------------------------------------
hexByte :: String -> Maybe Int
hexByte [a,b] = do
  hi <- hexDigit a
  lo <- hexDigit b
  pure (hi * 16 + lo)
hexByte _ = Nothing
-----------------------------------------------------------------------------
hexDigit :: Char -> Maybe Int
hexDigit c
  | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
  | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
  | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
  | otherwise = Nothing
-----------------------------------------------------------------------------
-- | Sets @fillStyle@\/@strokeStyle@\/@shadowColor@. @target@ is @0@ for
-- @fillStyle@, @1@ for @strokeStyle@, @2@ for @shadowColor@ — matching the
-- dispatch in 'flushCanvasOps_ffi'.
setStyle :: Double -> StyleArg -> Canvas ()
setStyle target styleArg = case styleArg of
  ColorArg c
    | Just (r, g, b, a) <- colorToRGBA c -> emit OpSetColorStyle [target, r, g, b, a]
  GradientArg (Gradient jsval) -> handleStyle jsval
  PatternArg (Pattern jsval) -> handleStyle jsval
  _ -> slowSetStyle target styleArg
  where
    handleStyle jsval = do
      h <- liftIO (registerOrGetHandle_ffi jsval)
      emit OpSetHandleStyle [target, fromIntegral h]
-----------------------------------------------------------------------------
slowSetStyle :: Double -> StyleArg -> Canvas ()
slowSetStyle target styleArg = do
  flushCanvas
  ctx <- askCtx
  liftIO $ setField ctx (styleProp target) =<< renderStyleArg styleArg
  where
    styleProp 0 = "fillStyle" :: MisoString
    styleProp 1 = "strokeStyle"
    styleProp _ = "shadowColor"
#endif
-----------------------------------------------------------------------------
-- | [ctx.globalCompositeOperation = "source-over"](https://www.w3schools.com/tags/canvas_globalcompositeoperation.asp)
globalCompositeOperation :: CompositeOperation -> Canvas ()
#ifdef WASM
globalCompositeOperation v = emit OpGlobalCompositeOperation [fromIntegral (fromEnum v)]
#else
globalCompositeOperation = set "globalCompositeOperation"
#endif
-----------------------------------------------------------------------------
-- | [ctx.clearRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_clearrect.asp)
clearRect :: (Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
clearRect (x,y,w,h) = emit OpClearRect [x,y,w,h]
#else
clearRect = call "clearRect"
#endif
-----------------------------------------------------------------------------
-- | [ctx.fillRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_fillrect.asp)
fillRect :: (Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
fillRect (x,y,w,h) = emit OpFillRect [x,y,w,h]
#else
fillRect = call "fillRect"
#endif
-----------------------------------------------------------------------------
-- | [ctx.strokeRect(x,y,width,height)](https://www.w3schools.com/tags/canvas_strokerect.asp)
strokeRect :: (Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
strokeRect (x,y,w,h) = emit OpStrokeRect [x,y,w,h]
#else
strokeRect = call "strokeRect"
#endif
-----------------------------------------------------------------------------
-- | [ctx.beginPath()](https://www.w3schools.com/tags/canvas_beginpath.asp)
beginPath :: () -> Canvas ()
#ifdef WASM
beginPath () = emit OpBeginPath []
#else
beginPath = call "beginPath"
#endif
-----------------------------------------------------------------------------
-- | [ctx.closePath()](https://www.w3schools.com/tags/canvas_closepath.asp)
closePath :: () -> Canvas ()
#ifdef WASM
closePath () = emit OpClosePath []
#else
closePath = call "closePath"
#endif
-----------------------------------------------------------------------------
-- | [ctx.moveTo(x,y)](https://www.w3schools.com/tags/canvas_moveto.asp)
moveTo :: Coord -> Canvas ()
#ifdef WASM
moveTo (x,y) = emit OpMoveTo [x,y]
#else
moveTo = call "moveTo"
#endif
-----------------------------------------------------------------------------
-- | [ctx.lineTo(x,y)](https://www.w3schools.com/tags/canvas_lineto.asp)
lineTo :: Coord -> Canvas ()
#ifdef WASM
lineTo (x,y) = emit OpLineTo [x,y]
#else
lineTo = call "lineTo"
#endif
-----------------------------------------------------------------------------
-- | [ctx.fill()](https://www.w3schools.com/tags/canvas_fill.asp)
fill :: () -> Canvas ()
#ifdef WASM
fill () = emit OpFill []
#else
fill = call "fill"
#endif
-----------------------------------------------------------------------------
-- | [ctx.rect(x,y,width,height)](https://www.w3schools.com/tags/canvas_rect.asp)
rect :: (Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
rect (x,y,w,h) = emit OpRect [x,y,w,h]
#else
rect = call "rect"
#endif
-----------------------------------------------------------------------------
-- | [ctx.stroke()](https://www.w3schools.com/tags/canvas_stroke.asp)
stroke :: () -> Canvas ()
#ifdef WASM
stroke () = emit OpStroke []
#else
stroke = call "stroke"
#endif
-----------------------------------------------------------------------------
-- | [ctx.bezierCurveTo(cp1x,cp1y,cp2x,cp2y,x,y)](https://www.w3schools.com/tags/canvas_beziercurveto.asp)
bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
bezierCurveTo (a,b,c,d,e,f) = emit OpBezierCurveTo [a,b,c,d,e,f]
#else
bezierCurveTo = call "bezierCurveTo"
#endif
-----------------------------------------------------------------------------
-- | [context.arc(x, y, r, sAngle, eAngle)](https://www.w3schools.com/tags/canvas_arc.asp)
arc :: (Double, Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
arc (x,y,r,s,e) = emit OpArc [x,y,r,s,e]
#else
arc = call "arc"
#endif
-----------------------------------------------------------------------------
-- | [context.arcTo(x1, y1, x2, y2, r)](https://www.w3schools.com/tags/canvas_arcto.asp)
arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
arcTo (x1,y1,x2,y2,r) = emit OpArcTo [x1,y1,x2,y2,r]
#else
arcTo = call "arcTo"
#endif
-----------------------------------------------------------------------------
-- | [context.quadraticCurveTo(cpx,cpy,x,y)](https://www.w3schools.com/tags/canvas_quadraticcurveto.asp)
quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
quadraticCurveTo (cpx,cpy,x,y) = emit OpQuadraticCurveTo [cpx,cpy,x,y]
#else
quadraticCurveTo = call "quadraticCurveTo"
#endif
-----------------------------------------------------------------------------
-- | [context.direction = "ltr"](https://www.w3schools.com/tags/canvas_direction.asp)
direction :: DirectionType -> Canvas ()
#ifdef WASM
direction v = emit OpDirection [fromIntegral (fromEnum v)]
#else
direction = set "direction"
#endif
-----------------------------------------------------------------------------
-- | [context.fillText(text,x,y)](https://www.w3schools.com/tags/canvas_filltext.asp)
fillText :: (MisoString, Double, Double) -> Canvas ()
#ifdef WASM
fillText args = flushCanvas >> call "fillText" args
#else
fillText = call "fillText"
#endif
-----------------------------------------------------------------------------
-- | [context.font = "italic small-caps bold 12px arial"](https://www.w3schools.com/tags/canvas_font.asp)
font :: MisoString -> Canvas ()
#ifdef WASM
font v = flushCanvas >> set "font" v
#else
font = set "font"
#endif
-----------------------------------------------------------------------------
-- | [ctx.strokeText()](https://www.w3schools.com/tags/canvas_stroketext.asp)
strokeText :: (MisoString, Double, Double) -> Canvas ()
#ifdef WASM
strokeText args = flushCanvas >> call "strokeText" args
#else
strokeText = call "strokeText"
#endif
-----------------------------------------------------------------------------
-- | [ctx.textAlign = "start"](https://www.w3schools.com/tags/canvas_textalign.asp)
textAlign :: TextAlignType -> Canvas ()
#ifdef WASM
textAlign v = emit OpTextAlign [fromIntegral (fromEnum v)]
#else
textAlign = set "textAlign"
#endif
-----------------------------------------------------------------------------
-- | [ctx.textBaseline = "top"](https://www.w3schools.com/tags/canvas_textBaseLine.asp)
textBaseline :: TextBaselineType -> Canvas ()
#ifdef WASM
textBaseline v = emit OpTextBaseline [fromIntegral (fromEnum v)]
#else
textBaseline = set "textBaseline"
#endif
-----------------------------------------------------------------------------
-- | [gradient.addColorStop(stop,color)](https://www.w3schools.com/tags/canvas_addcolorstop.asp)
--
-- Mutates the 'Gradient' object directly rather than the canvas context, so
-- it never needs to flush the pending batch — anything queued so far still
-- runs in the order it was queued, and this always executes before whatever
-- later 'flushCanvas' eventually reads the gradient's handle.
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
#ifdef WASM
createLinearGradient args = flushCanvas >> call "createLinearGradient" args
#else
createLinearGradient = call "createLinearGradient"
#endif
-----------------------------------------------------------------------------
-- | [ctx.createPattern(image, "repeat")](https://www.w3schools.com/tags/canvas_createpattern.asp)
createPattern :: (Image, PatternType) -> Canvas Pattern
#ifdef WASM
createPattern args = flushCanvas >> call "createPattern" args
#else
createPattern = call "createPattern"
#endif
-----------------------------------------------------------------------------
-- | [ctx.createRadialGradient(x0,y0,r0,x1,y1,r1)](https://www.w3schools.com/tags/canvas_createradialgradient.asp)
createRadialGradient :: (Double,Double,Double,Double,Double,Double) -> Canvas Gradient
#ifdef WASM
createRadialGradient args = flushCanvas >> call "createRadialGradient" args
#else
createRadialGradient = call "createRadialGradient"
#endif
-----------------------------------------------------------------------------
-- | [ctx.fillStyle = "red"](https://www.w3schools.com/tags/canvas_fillstyle.asp)
fillStyle :: StyleArg -> Canvas ()
#ifdef WASM
fillStyle = setStyle 0
#else
fillStyle = set "fillStyle"
#endif
-----------------------------------------------------------------------------
-- | [ctx.lineCap = "butt"](https://www.w3schools.com/tags/canvas_lineCap.asp)
lineCap :: LineCapType -> Canvas ()
#ifdef WASM
lineCap v = emit OpLineCap [fromIntegral (fromEnum v)]
#else
lineCap = set "lineCap"
#endif
-----------------------------------------------------------------------------
-- | [ctx.lineJoin = "bevel"](https://www.w3schools.com/tags/canvas_lineJoin.asp)
lineJoin :: LineJoinType -> Canvas ()
#ifdef WASM
lineJoin v = emit OpLineJoin [fromIntegral (fromEnum v)]
#else
lineJoin = set "lineJoin"
#endif
-----------------------------------------------------------------------------
-- | [ctx.lineWidth = 10](https://www.w3schools.com/tags/canvas_lineWidth.asp)
lineWidth :: Double -> Canvas ()
#ifdef WASM
lineWidth v = emit OpLineWidth [v]
#else
lineWidth = set "lineWidth"
#endif
-----------------------------------------------------------------------------
-- | [ctx.miterLimit = 10](https://www.w3schools.com/tags/canvas_miterLimit.asp)
miterLimit :: Double -> Canvas ()
#ifdef WASM
miterLimit v = emit OpMiterLimit [v]
#else
miterLimit = set "miterLimit"
#endif
-----------------------------------------------------------------------------
-- | [ctx.shadowBlur = 10](https://www.w3schools.com/tags/canvas_shadowBlur.asp)
shadowBlur :: Double -> Canvas ()
#ifdef WASM
shadowBlur v = emit OpShadowBlur [v]
#else
shadowBlur = set "shadowBlur"
#endif
-----------------------------------------------------------------------------
-- | [ctx.shadowColor = "red"](https://www.w3schools.com/tags/canvas_shadowColor.asp)
shadowColor :: Color -> Canvas ()
#ifdef WASM
shadowColor c
  | Just (r,g,b,a) <- colorToRGBA c = emit OpSetColorStyle [2, r, g, b, a]
  | otherwise = do
      flushCanvas
      ctx <- askCtx
      liftIO $ setField ctx ("shadowColor" :: MisoString) (renderColor c)
#else
shadowColor = set "shadowColor"
#endif
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetX = 20](https://www.w3schools.com/tags/canvas_shadowOffsetX.asp)
shadowOffsetX :: Double -> Canvas ()
#ifdef WASM
shadowOffsetX v = emit OpShadowOffsetX [v]
#else
shadowOffsetX = set "shadowOffsetX"
#endif
-----------------------------------------------------------------------------
-- | [ctx.shadowOffsetY = 20](https://www.w3schools.com/tags/canvas_shadowOffsetY.asp)
shadowOffsetY :: Double -> Canvas ()
#ifdef WASM
shadowOffsetY v = emit OpShadowOffsetY [v]
#else
shadowOffsetY = set "shadowOffsetY"
#endif
-----------------------------------------------------------------------------
-- | [ctx.strokeStyle = "red"](https://www.w3schools.com/tags/canvas_strokeStyle.asp)
strokeStyle :: StyleArg -> Canvas ()
#ifdef WASM
strokeStyle = setStyle 1
#else
strokeStyle = set "strokeStyle"
#endif
-----------------------------------------------------------------------------
-- | [ctx.scale(width,height)](https://www.w3schools.com/tags/canvas_scale.asp)
scale :: (Double, Double) -> Canvas ()
#ifdef WASM
scale (x,y) = emit OpScale [x,y]
#else
scale = call "scale"
#endif
-----------------------------------------------------------------------------
-- | [ctx.rotate(angle)](https://www.w3schools.com/tags/canvas_rotate.asp)
rotate :: Double -> Canvas ()
#ifdef WASM
rotate v = emit OpRotate [v]
#else
rotate = call "rotate"
#endif
-----------------------------------------------------------------------------
-- | [ctx.translate(angle)](https://www.w3schools.com/tags/canvas_translate.asp)
translate :: Coord -> Canvas ()
#ifdef WASM
translate (x,y) = emit OpTranslate [x,y]
#else
translate = call "translate"
#endif
-----------------------------------------------------------------------------
-- | [ctx.transform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_transform.asp)
transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
transform (a,b,c,d,e,f) = emit OpTransform [a,b,c,d,e,f]
#else
transform = call "transform"
#endif
-----------------------------------------------------------------------------
-- | [ctx.setTransform(a,b,c,d,e,f)](https://www.w3schools.com/tags/canvas_setTransform.asp)
setTransform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
setTransform (a,b,c,d,e,f) = emit OpSetTransform [a,b,c,d,e,f]
#else
setTransform = call "setTransform"
#endif
----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage :: (Image, Double, Double) -> Canvas ()
#ifdef WASM
drawImage (Image jsval, x, y) = do
  h <- liftIO (registerOrGetHandle_ffi jsval)
  emit OpDrawImage [fromIntegral h, x, y]
#else
drawImage = call "drawImage"
#endif
-----------------------------------------------------------------------------
-- | [ctx.drawImage(image,x,y)](https://www.w3schools.com/tags/canvas_drawImage.asp)
drawImage' :: (Image, Double, Double, Double, Double) -> Canvas ()
#ifdef WASM
drawImage' (Image jsval, x, y, w, h) = do
  handle <- liftIO (registerOrGetHandle_ffi jsval)
  emit OpDrawImage2 [fromIntegral handle, x, y, w, h]
#else
drawImage' = call "drawImage"
#endif
-----------------------------------------------------------------------------
-- | [ctx.createImageData(width,height)](https://www.w3schools.com/tags/canvas_createImageData.asp)
createImageData :: (Double, Double) -> Canvas ImageData
#ifdef WASM
createImageData args = flushCanvas >> call "createImageData" args
#else
createImageData = call "createImageData"
#endif
-----------------------------------------------------------------------------
-- | [ctx.getImageData(w,x,y,z)](https://www.w3schools.com/tags/canvas_getImageData.asp)
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
#ifdef WASM
getImageData args = flushCanvas >> call "getImageData" args
#else
getImageData = call "getImageData"
#endif
-----------------------------------------------------------------------------
-- | [imageData.data\[index\] = 255](https://www.w3schools.com/tags/canvas_imagedata_data.asp)
--
-- Mutates the t'ImageData' object's own backing array directly, not the
-- canvas context, so it never needs to flush the pending batch.
setImageData :: (ImageData, Int, Double) -> Canvas ()
setImageData (imgData, index, value) = liftIO $ do
   o <- imgData ! ("data" :: MisoString)
   (o <## index) value
-----------------------------------------------------------------------------
-- | [imageData.height](https://www.w3schools.com/tags/canvas_imagedata_height.asp)
--
-- Reads a property directly off the t'ImageData' object; doesn't touch the
-- canvas context, so it never needs to flush the pending batch.
height :: ImageData -> Canvas Double
height (ImageData imgData) = liftIO $ do
  fromJSValUnchecked =<< imgData ! ("height" :: MisoString)
-----------------------------------------------------------------------------
-- | [imageData.width](https://www.w3schools.com/tags/canvas_imagedata_width.asp)
--
-- Reads a property directly off the t'ImageData' object; doesn't touch the
-- canvas context, so it never needs to flush the pending batch.
width :: ImageData -> Canvas Double
width (ImageData imgData) = liftIO $
  fromJSValUnchecked =<< imgData ! ("width" :: MisoString)
-----------------------------------------------------------------------------
-- | [ctx.putImageData(imageData,x,y)](https://www.w3schools.com/tags/canvas_putImageData.asp)
--
-- Stays on the slow path: a given t'ImageData' is typically read back,
-- mutated, and put exactly once, so there's no repeated-reference benefit
-- to amortize the way there is for 'Gradient'\/'Pattern'\/'Image' — pooling
-- it would only add permanent pool growth for no batching win.
putImageData :: (ImageData, Double, Double) -> Canvas ()
#ifdef WASM
putImageData args = flushCanvas >> call "putImageData" args
#else
putImageData = call "putImageData"
#endif
-----------------------------------------------------------------------------
-- | [ctx.globalAlpha = 0.2](https://www.w3schools.com/tags/canvas_globalAlpha.asp)
globalAlpha :: Double -> Canvas ()
#ifdef WASM
globalAlpha v = emit OpGlobalAlpha [v]
#else
globalAlpha = set "globalAlpha"
#endif
-----------------------------------------------------------------------------
-- | [ctx.clip()](https://www.w3schools.com/tags/canvas_clip.asp)
clip :: () -> Canvas ()
#ifdef WASM
clip () = emit OpClip []
#else
clip = call "clip"
#endif
-----------------------------------------------------------------------------
-- | [ctx.save()](https://www.w3schools.com/tags/canvas_save.asp)
save :: () -> Canvas ()
#ifdef WASM
save () = emit OpSave []
#else
save = call "save"
#endif
-----------------------------------------------------------------------------
-- | [ctx.restore()](https://www.w3schools.com/tags/canvas_restore.asp)
restore :: () -> Canvas ()
#ifdef WASM
restore () = emit OpRestore []
#else
restore = call "restore"
#endif
-----------------------------------------------------------------------------
