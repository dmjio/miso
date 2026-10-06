-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Canvas.Draw
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Canvas drawing as a type class, so that one description of a frame can
-- be either executed through "Miso.Canvas" ('Miso.Canvas.Draw.Run.Run') or
-- compiled to a JavaScript function ('JSGen') that draws the whole frame in
-- one call.
--
-- A frame is drawn from an array of entities in wasm memory ('entityStride'
-- doubles each: x, y, size, r, g, b), the frame time, the measured fps and
-- the entity count.  Numbers are @r Double@ and have a 'Num' instance, so
-- literals and arithmetic read as usual; 'JSGen' folds static arithmetic.
--
-- @
-- scene :: (Draw r, Fractional (r Double)) => r ()
-- scene = fillStyleRGB 20 20 40 >. fillRect 0 0 800 600
--      >. forEntities (\\e -> fillStyleRGB (er e) (eg e) (eb e) >. fillRect (ex e) (ey e) (esize e) (esize e))
-- @
--
-- 'Miso.Canvas.Draw.Run.Run' is the general path: every operation is a
-- 'Miso.Canvas.Canvas' call and the entity loop runs in Haskell.  'JSGen' is
-- for the regular part of a scene: the generated loop runs in JavaScript over
-- memory Haskell filled.  It is meant to be run at compile time, by a splice
-- on the MicroHs @2ltt@ branch, but it is ordinary Haskell and works at run
-- time too.  The 'Run' instance is in its own module because it calls
-- JavaScript imports, which the staging type checker places at the object
-- level; keeping it apart leaves this module and 'JSGen' stage polymorphic.
----------------------------------------------------------------------------
module Miso.Canvas.Draw
  ( -- * The class
    Draw (..)
  , Entity (..)
  , Input (..)
  , entityStride
    -- * Generating JavaScript
  , JSGen
  , genFrame
  ) where
-----------------------------------------------------------------------------
import           Data.List (intercalate)
-----------------------------------------------------------------------------
-- | Per-frame inputs.
data Input
  = Time   -- ^ the animation frame timestamp (ms)
  | Fps    -- ^ the measured frame rate
  | Count  -- ^ the number of entities
-----------------------------------------------------------------------------
-- | An entity's fields, as the instance reads them.
data Entity r
  = Entity
  { ex, ey, esize, er, eg, eb :: r Double }
-----------------------------------------------------------------------------
-- | Doubles per entity in the array: x, y, size, r, g, b.
entityStride :: Int
entityStride = 6
-----------------------------------------------------------------------------
infixr 1 >.
-----------------------------------------------------------------------------
class Draw r where
  -- | Sequence two drawings.
  (>.) :: r () -> r () -> r ()
  -- | Draw nothing.
  done :: r ()
  -- | A frame input.
  input :: Input -> r Double
  -- | Floating point modulus with a non-negative result.
  fmod_ :: r Double -> r Double -> r Double
  -- | A string.  Text is 'String' here, not 'Miso.String.MisoString': on
  -- MicroHs a MisoString is a JavaScript string, which does not exist at
  -- compile time.
  str :: String -> r String
  -- | A number with the given decimals, as a string.
  showFixed :: Int -> r Double -> r String
  -- | Concatenate strings.
  cat :: [r String] -> r String
  fillStyleRGB :: r Double -> r Double -> r Double -> r ()
  fillRect :: r Double -> r Double -> r Double -> r Double -> r ()
  save :: r ()
  restore :: r ()
  translate :: r Double -> r Double -> r ()
  font :: String -> r ()
  fillText :: r String -> r Double -> r Double -> r ()
  -- | Draw every entity of the array.
  forEntities :: (Entity r -> r ()) -> r ()
-----------------------------------------------------------------------------
-- JSGen: generate a JavaScript frame function
-----------------------------------------------------------------------------
-- | A JavaScript value: a static number, an expression, a string literal,
-- or a string expression.
data Val = VNum Double | VExp String | VStr String | VStrE String
-----------------------------------------------------------------------------
-- | Statements so far, and the value (unused for @()@).
newtype JSGen a = JSGen ([String], Val)
-----------------------------------------------------------------------------
unit :: [String] -> JSGen ()
unit ss = JSGen (ss, VNum 0)
-----------------------------------------------------------------------------
val :: JSGen a -> Val
val (JSGen (_, v)) = v
-----------------------------------------------------------------------------
num :: Val -> String
num (VNum d)
  | d < 0 = "(" ++ show d ++ ")"
  | d == fromIntegral (truncate d :: Int) = show (truncate d :: Int)
  | otherwise = show d
num (VExp s) = s
num (VStr s) = show s
num (VStrE s) = s
-----------------------------------------------------------------------------
numG :: JSGen Double -> String
numG = num . val
-----------------------------------------------------------------------------
-- | Arithmetic, folded when both sides are static.
arith :: (Double -> Double -> Double) -> String -> JSGen Double -> JSGen Double -> JSGen Double
arith f _ a b | VNum x <- val a, VNum y <- val b = JSGen ([], VNum (f x y))
arith _ op a b = JSGen ([], VExp ("(" ++ numG a ++ " " ++ op ++ " " ++ numG b ++ ")"))
-----------------------------------------------------------------------------
instance Num (JSGen Double) where
  (+) = arith (+) "+"
  (-) = arith (-) "-"
  (*) = arith (*) "*"
  abs a = JSGen ([], VExp ("Math.abs(" ++ numG a ++ ")"))
  signum a = JSGen ([], VExp ("Math.sign(" ++ numG a ++ ")"))
  fromInteger = JSGen . (,) [] . VNum . fromInteger
-----------------------------------------------------------------------------
instance Fractional (JSGen Double) where
  (/) = arith (/) "/"
  fromRational = JSGen . (,) [] . VNum . fromRational
-----------------------------------------------------------------------------
-- | The JavaScript for the colour; static components fold into one literal.
rgb :: Val -> Val -> Val -> String
rgb (VNum r) (VNum g) (VNum b) = show ("rgb(" ++ num (VNum r) ++ "," ++ num (VNum g) ++ "," ++ num (VNum b) ++ ")")
rgb r g b = "'rgb(' + " ++ num r ++ " + ',' + " ++ num g ++ " + ',' + " ++ num b ++ " + ')'"
-----------------------------------------------------------------------------
instance Draw JSGen where
  JSGen (a, _) >. JSGen (b, _) = unit (a ++ b)
  done = unit []
  input Time = JSGen ([], VExp "t")
  input Fps = JSGen ([], VExp "fps")
  input Count = JSGen ([], VExp "count")
  fmod_ a m = JSGen ([], VExp ("(((" ++ numG a ++ " % " ++ numG m ++ ") + " ++ numG m ++ ") % " ++ numG m ++ ")"))
  str s = JSGen ([], VStr s)
  showFixed n a = JSGen ([], VStrE ("(" ++ numG a ++ ").toFixed(" ++ show n ++ ")"))
  cat parts = JSGen ([], VStrE (intercalate " + " (map (num . val) parts)))
  fillStyleRGB r g b = unit ["ctx.fillStyle = " ++ rgb (val r) (val g) (val b) ++ ";"]
  fillRect x y w h = unit ["ctx.fillRect(" ++ intercalate ", " (map numG [x, y, w, h]) ++ ");"]
  save = unit ["ctx.save();"]
  restore = unit ["ctx.restore();"]
  translate x y = unit ["ctx.translate(" ++ numG x ++ ", " ++ numG y ++ ");"]
  font f = unit ["ctx.font = " ++ show f ++ ";"]
  fillText s x y = unit ["ctx.fillText(" ++ num (val s) ++ ", " ++ numG x ++ ", " ++ numG y ++ ");"]
  forEntities body =
    unit $
      [ "for (let i = 0, o = base >> 3; i < count; i++, o += " ++ show entityStride ++ ") {" ]
      ++ map ("  " ++) stmts
      ++ [ "}" ]
    where
      field k = JSGen ([], VExp ("heap[o + " ++ show (k :: Int) ++ "]"))
      JSGen (stmts, _) = body (Entity (field 0) (field 1) (field 2) (field 3) (field 4) (field 5))
-----------------------------------------------------------------------------
-- | The source of a frame function @(ctx, heap, base, count, t, fps) => {...}@:
-- the context, the wasm memory as a @Float64Array@, the entity array's byte
-- address and count, and the inputs.
genFrame :: JSGen () -> String
genFrame (JSGen (stmts, _)) =
  unlines $
    [ "(ctx, heap, base, count, t, fps) => {" ]
    ++ map ("  " ++) stmts
    ++ [ "}" ]
-----------------------------------------------------------------------------
