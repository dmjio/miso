-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Canvas.FFI
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- One JavaScript import per straight-line @CanvasRenderingContext2D@
-- operation: a method call or a property assignment that returns nothing.
--
-- "Miso.Canvas" used to go through the generic 'Miso.DSL.#' and 'setField':
-- a method name marshalled to a JavaScript string, a property lookup, an
-- argument array, and a 'JSVal' handle for every argument and the result,
-- on each operation.  A direct import does one call with the numbers as
-- numbers.  On MicroHs this made the canvas benchmark in @sample-app@
-- about 3.5 times faster (78 to 22 ms per frame for 1000 rectangles).
--
-- Operations that return a value (gradients, patterns, image data) stay on
-- the generic path in "Miso.Canvas".  The native GHC backend has no
-- JavaScript FFI, so there these are the generic calls as before.
----------------------------------------------------------------------------
module Miso.Canvas.FFI
  ( jsString
  , clearRect
  , fillRect
  , strokeRect
  , rect
  , quadraticCurveTo
  , beginPath
  , closePath
  , fill
  , stroke
  , clip
  , save
  , restore
  , moveTo
  , lineTo
  , scale
  , translate
  , rotate
  , bezierCurveTo
  , transform
  , setTransform
  , arc
  , arcTo
  , fillText
  , strokeText
  , drawImage
  , drawImage4
  , putImageData
  , lineWidth
  , miterLimit
  , shadowBlur
  , shadowOffsetX
  , shadowOffsetY
  , globalAlpha
  , font
  , lineCap
  , lineJoin
  , direction
  , textAlign
  , textBaseline
  , globalCompositeOperation
  , shadowColor
  , fillStyle
  , strokeStyle
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
#if defined(GHCJS_NEW) || defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH) || defined(__MHS__)
import           Miso.DSL.FFI (JSVal, JSString)
#ifdef MISO_TEXT
import           Miso.DSL.FFI (textToJSString)
#endif
#else
import           Control.Monad (void)
import           Miso.DSL (JSVal, (#), setField)
#endif
-----------------------------------------------------------------------------
-- | A 'MisoString' as the string the imports take.
#if !(defined(GHCJS_NEW) || defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH) || defined(__MHS__))
jsString :: MisoString -> MisoString
jsString s = s
#elif defined(MISO_TEXT)
jsString :: MisoString -> JSString
jsString = textToJSString
#else
jsString :: MisoString -> JSString
jsString s = s
#endif
-----------------------------------------------------------------------------
#if defined(GHCJS_NEW)
-- The GHC JavaScript backend: the code is an arrow function.
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $1.clearRect($2,$3,$4,$5); })"
  clearRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $1.fillRect($2,$3,$4,$5); })"
  fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $1.strokeRect($2,$3,$4,$5); })"
  strokeRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $1.rect($2,$3,$4,$5); })"
  rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $1.quadraticCurveTo($2,$3,$4,$5); })"
  quadraticCurveTo :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.beginPath(); })"
  beginPath :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.closePath(); })"
  closePath :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.fill(); })"
  fill :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.stroke(); })"
  stroke :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.clip(); })"
  clip :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.save(); })"
  save :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1) => { $1.restore(); })"
  restore :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3) => { $1.moveTo($2,$3); })"
  moveTo :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3) => { $1.lineTo($2,$3); })"
  lineTo :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3) => { $1.scale($2,$3); })"
  scale :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3) => { $1.translate($2,$3); })"
  translate :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.rotate($2); })"
  rotate :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $1.bezierCurveTo($2,$3,$4,$5,$6,$7); })"
  bezierCurveTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $1.transform($2,$3,$4,$5,$6,$7); })"
  transform :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $1.setTransform($2,$3,$4,$5,$6,$7); })"
  setTransform :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $1.arc($2,$3,$4,$5,$6); })"
  arc :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $1.arcTo($2,$3,$4,$5,$6); })"
  arcTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4) => { $1.fillText($2,$3,$4); })"
  fillText :: JSVal -> JSString -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4) => { $1.strokeText($2,$3,$4); })"
  strokeText :: JSVal -> JSString -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4) => { $1.drawImage($2,$3,$4); })"
  drawImage :: JSVal -> JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $1.drawImage($2,$3,$4,$5,$6); })"
  drawImage4 :: JSVal -> JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2,$3,$4) => { $1.putImageData($2,$3,$4); })"
  putImageData :: JSVal -> JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.lineWidth = $2; })"
  lineWidth :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.miterLimit = $2; })"
  miterLimit :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.shadowBlur = $2; })"
  shadowBlur :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.shadowOffsetX = $2; })"
  shadowOffsetX :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.shadowOffsetY = $2; })"
  shadowOffsetY :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.globalAlpha = $2; })"
  globalAlpha :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.font = $2; })"
  font :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.lineCap = $2; })"
  lineCap :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.lineJoin = $2; })"
  lineJoin :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.direction = $2; })"
  direction :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.textAlign = $2; })"
  textAlign :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.textBaseline = $2; })"
  textBaseline :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.globalCompositeOperation = $2; })"
  globalCompositeOperation :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.shadowColor = $2; })"
  shadowColor :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.fillStyle = $2; })"
  fillStyle :: JSVal -> JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(($1,$2) => { $1.strokeStyle = $2; })"
  strokeStyle :: JSVal -> JSVal -> IO ()
#elif defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH) || defined(__MHS__)
-- GHCJS, the GHC wasm backend and MicroHs: the code is a statement.
foreign import javascript unsafe "$1.clearRect($2,$3,$4,$5)"
  clearRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.fillRect($2,$3,$4,$5)"
  fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.strokeRect($2,$3,$4,$5)"
  strokeRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.rect($2,$3,$4,$5)"
  rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.quadraticCurveTo($2,$3,$4,$5)"
  quadraticCurveTo :: JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.beginPath()"
  beginPath :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.closePath()"
  closePath :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.fill()"
  fill :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.stroke()"
  stroke :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.clip()"
  clip :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.save()"
  save :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.restore()"
  restore :: JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.moveTo($2,$3)"
  moveTo :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.lineTo($2,$3)"
  lineTo :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.scale($2,$3)"
  scale :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.translate($2,$3)"
  translate :: JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.rotate($2)"
  rotate :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.bezierCurveTo($2,$3,$4,$5,$6,$7)"
  bezierCurveTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.transform($2,$3,$4,$5,$6,$7)"
  transform :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.setTransform($2,$3,$4,$5,$6,$7)"
  setTransform :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.arc($2,$3,$4,$5,$6)"
  arc :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.arcTo($2,$3,$4,$5,$6)"
  arcTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.fillText($2,$3,$4)"
  fillText :: JSVal -> JSString -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.strokeText($2,$3,$4)"
  strokeText :: JSVal -> JSString -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.drawImage($2,$3,$4)"
  drawImage :: JSVal -> JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.drawImage($2,$3,$4,$5,$6)"
  drawImage4 :: JSVal -> JSVal -> Double -> Double -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.putImageData($2,$3,$4)"
  putImageData :: JSVal -> JSVal -> Double -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.lineWidth = $2"
  lineWidth :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.miterLimit = $2"
  miterLimit :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.shadowBlur = $2"
  shadowBlur :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.shadowOffsetX = $2"
  shadowOffsetX :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.shadowOffsetY = $2"
  shadowOffsetY :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.globalAlpha = $2"
  globalAlpha :: JSVal -> Double -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.font = $2"
  font :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.lineCap = $2"
  lineCap :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.lineJoin = $2"
  lineJoin :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.direction = $2"
  direction :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.textAlign = $2"
  textAlign :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.textBaseline = $2"
  textBaseline :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.globalCompositeOperation = $2"
  globalCompositeOperation :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.shadowColor = $2"
  shadowColor :: JSVal -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.fillStyle = $2"
  fillStyle :: JSVal -> JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.strokeStyle = $2"
  strokeStyle :: JSVal -> JSVal -> IO ()
#else
-- Native GHC: no JavaScript FFI, the generic DSL as before.
clearRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
clearRect ctx a0 a1 a2 a3 = void (ctx # "clearRect" $ (a0,a1,a2,a3))
-----------------------------------------------------------------------------
fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
fillRect ctx a0 a1 a2 a3 = void (ctx # "fillRect" $ (a0,a1,a2,a3))
-----------------------------------------------------------------------------
strokeRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
strokeRect ctx a0 a1 a2 a3 = void (ctx # "strokeRect" $ (a0,a1,a2,a3))
-----------------------------------------------------------------------------
rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
rect ctx a0 a1 a2 a3 = void (ctx # "rect" $ (a0,a1,a2,a3))
-----------------------------------------------------------------------------
quadraticCurveTo :: JSVal -> Double -> Double -> Double -> Double -> IO ()
quadraticCurveTo ctx a0 a1 a2 a3 = void (ctx # "quadraticCurveTo" $ (a0,a1,a2,a3))
-----------------------------------------------------------------------------
beginPath :: JSVal -> IO ()
beginPath ctx = void (ctx # "beginPath" $ ())
-----------------------------------------------------------------------------
closePath :: JSVal -> IO ()
closePath ctx = void (ctx # "closePath" $ ())
-----------------------------------------------------------------------------
fill :: JSVal -> IO ()
fill ctx = void (ctx # "fill" $ ())
-----------------------------------------------------------------------------
stroke :: JSVal -> IO ()
stroke ctx = void (ctx # "stroke" $ ())
-----------------------------------------------------------------------------
clip :: JSVal -> IO ()
clip ctx = void (ctx # "clip" $ ())
-----------------------------------------------------------------------------
save :: JSVal -> IO ()
save ctx = void (ctx # "save" $ ())
-----------------------------------------------------------------------------
restore :: JSVal -> IO ()
restore ctx = void (ctx # "restore" $ ())
-----------------------------------------------------------------------------
moveTo :: JSVal -> Double -> Double -> IO ()
moveTo ctx a0 a1 = void (ctx # "moveTo" $ (a0,a1))
-----------------------------------------------------------------------------
lineTo :: JSVal -> Double -> Double -> IO ()
lineTo ctx a0 a1 = void (ctx # "lineTo" $ (a0,a1))
-----------------------------------------------------------------------------
scale :: JSVal -> Double -> Double -> IO ()
scale ctx a0 a1 = void (ctx # "scale" $ (a0,a1))
-----------------------------------------------------------------------------
translate :: JSVal -> Double -> Double -> IO ()
translate ctx a0 a1 = void (ctx # "translate" $ (a0,a1))
-----------------------------------------------------------------------------
rotate :: JSVal -> Double -> IO ()
rotate ctx a0 = void (ctx # "rotate" $ a0)
-----------------------------------------------------------------------------
bezierCurveTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo ctx a0 a1 a2 a3 a4 a5 = void (ctx # "bezierCurveTo" $ (a0,a1,a2,a3,a4,a5))
-----------------------------------------------------------------------------
transform :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
transform ctx a0 a1 a2 a3 a4 a5 = void (ctx # "transform" $ (a0,a1,a2,a3,a4,a5))
-----------------------------------------------------------------------------
setTransform :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
setTransform ctx a0 a1 a2 a3 a4 a5 = void (ctx # "setTransform" $ (a0,a1,a2,a3,a4,a5))
-----------------------------------------------------------------------------
arc :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()
arc ctx a0 a1 a2 a3 a4 = void (ctx # "arc" $ (a0,a1,a2,a3,a4))
-----------------------------------------------------------------------------
arcTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()
arcTo ctx a0 a1 a2 a3 a4 = void (ctx # "arcTo" $ (a0,a1,a2,a3,a4))
-----------------------------------------------------------------------------
fillText :: JSVal -> MisoString -> Double -> Double -> IO ()
fillText ctx o a0 a1 = void (ctx # "fillText" $ (o,a0,a1))
-----------------------------------------------------------------------------
strokeText :: JSVal -> MisoString -> Double -> Double -> IO ()
strokeText ctx o a0 a1 = void (ctx # "strokeText" $ (o,a0,a1))
-----------------------------------------------------------------------------
drawImage :: JSVal -> JSVal -> Double -> Double -> IO ()
drawImage ctx o a0 a1 = void (ctx # "drawImage" $ (o,a0,a1))
-----------------------------------------------------------------------------
drawImage4 :: JSVal -> JSVal -> Double -> Double -> Double -> Double -> IO ()
drawImage4 ctx o a0 a1 a2 a3 = void (ctx # "drawImage" $ (o,a0,a1,a2,a3))
-----------------------------------------------------------------------------
putImageData :: JSVal -> JSVal -> Double -> Double -> IO ()
putImageData ctx o a0 a1 = void (ctx # "putImageData" $ (o,a0,a1))
-----------------------------------------------------------------------------
lineWidth :: JSVal -> Double -> IO ()
lineWidth ctx v = setField ctx "lineWidth" v
-----------------------------------------------------------------------------
miterLimit :: JSVal -> Double -> IO ()
miterLimit ctx v = setField ctx "miterLimit" v
-----------------------------------------------------------------------------
shadowBlur :: JSVal -> Double -> IO ()
shadowBlur ctx v = setField ctx "shadowBlur" v
-----------------------------------------------------------------------------
shadowOffsetX :: JSVal -> Double -> IO ()
shadowOffsetX ctx v = setField ctx "shadowOffsetX" v
-----------------------------------------------------------------------------
shadowOffsetY :: JSVal -> Double -> IO ()
shadowOffsetY ctx v = setField ctx "shadowOffsetY" v
-----------------------------------------------------------------------------
globalAlpha :: JSVal -> Double -> IO ()
globalAlpha ctx v = setField ctx "globalAlpha" v
-----------------------------------------------------------------------------
font :: JSVal -> MisoString -> IO ()
font ctx v = setField ctx "font" v
-----------------------------------------------------------------------------
lineCap :: JSVal -> MisoString -> IO ()
lineCap ctx v = setField ctx "lineCap" v
-----------------------------------------------------------------------------
lineJoin :: JSVal -> MisoString -> IO ()
lineJoin ctx v = setField ctx "lineJoin" v
-----------------------------------------------------------------------------
direction :: JSVal -> MisoString -> IO ()
direction ctx v = setField ctx "direction" v
-----------------------------------------------------------------------------
textAlign :: JSVal -> MisoString -> IO ()
textAlign ctx v = setField ctx "textAlign" v
-----------------------------------------------------------------------------
textBaseline :: JSVal -> MisoString -> IO ()
textBaseline ctx v = setField ctx "textBaseline" v
-----------------------------------------------------------------------------
globalCompositeOperation :: JSVal -> MisoString -> IO ()
globalCompositeOperation ctx v = setField ctx "globalCompositeOperation" v
-----------------------------------------------------------------------------
shadowColor :: JSVal -> MisoString -> IO ()
shadowColor ctx v = setField ctx "shadowColor" v
-----------------------------------------------------------------------------
fillStyle :: JSVal -> JSVal -> IO ()
fillStyle ctx v = setField ctx "fillStyle" v
-----------------------------------------------------------------------------
strokeStyle :: JSVal -> JSVal -> IO ()
strokeStyle ctx v = setField ctx "strokeStyle" v
#endif
-----------------------------------------------------------------------------
