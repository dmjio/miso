----------------------------------------------------------------------------
-- | A staged canvas scene: Haskell that runs in the compiler and emits the
-- JavaScript for one frame.
--
-- Everything here is meta level, in the two-level sense: it is executed by
-- a splice in "Main" (@~(codeString (renderFrame benchScene))@) and leaves
-- behind a string literal, the source of a JavaScript function
--
-- > (ctx, heap, base, count, t, fps) => { ... }
--
-- that draws a whole frame.  @heap@ is the wasm memory as a @Float64Array@
-- and @base@ the byte address of an array of entities ('stride' doubles
-- each), so the per-entity loop runs in JavaScript over memory that Haskell
-- filled once.  At run time Haskell makes one call per frame.
--
-- Numbers are partially static ('Sta' is known now, 'Dyn' is a JavaScript
-- expression), so constant colours and coordinates are folded into the
-- generated source.
----------------------------------------------------------------------------
module Scene
  ( -- * Rendering
    renderFrame
  , stride
    -- * The benchmark scene
  , benchScene
    -- * The DSL
  , Num_ (..)
  , Str_ (..)
  , Stmts
  , Entity (..)
  , add, sub, mul, fmod_
  , fillStyleRGB, fillRect, save, restore, translate, font, fillText
  , forEntities, cat
  ) where
----------------------------------------------------------------------------
import Data.List (intercalate)
----------------------------------------------------------------------------
-- | A JavaScript number: known at compile time, or an expression.
data Num_ = Sta Double | Dyn String
----------------------------------------------------------------------------
-- | A JavaScript string: a literal, or an expression.
data Str_ = SStr String | SDyn String
----------------------------------------------------------------------------
-- | JavaScript statements.
type Stmts = [String]
----------------------------------------------------------------------------
-- | An entity as seen from the generated loop: its fields are heap reads.
data Entity
  = Entity
  { ex, ey, esize, er, eg, eb :: Num_ }
----------------------------------------------------------------------------
-- | Doubles per entity in the array: x, y, size, r, g, b.
stride :: Int
stride = 6
----------------------------------------------------------------------------
num :: Num_ -> String
num (Sta d)
  | d < 0 = "(" ++ show d ++ ")"
  | otherwise = show d
num (Dyn s) = s
----------------------------------------------------------------------------
-- | An integer-valued static number prints without the ".0".
int :: Num_ -> String
int (Sta d) | d == fromIntegral (round d :: Int) = show (round d :: Int)
int n = num n
----------------------------------------------------------------------------
str :: Str_ -> String
str (SStr s) = show s     -- Haskell string syntax is valid JavaScript for plain text
str (SDyn s) = s
----------------------------------------------------------------------------
-- Arithmetic, folded when both sides are static.
add, sub, mul, fmod_ :: Num_ -> Num_ -> Num_
add (Sta a) (Sta b) = Sta (a + b)
add a b = Dyn ("(" ++ num a ++ " + " ++ num b ++ ")")
sub (Sta a) (Sta b) = Sta (a - b)
sub a b = Dyn ("(" ++ num a ++ " - " ++ num b ++ ")")
mul (Sta a) (Sta b) = Sta (a * b)
mul a b = Dyn ("(" ++ num a ++ " * " ++ num b ++ ")")
-- | Floating point modulus with a non-negative result.
fmod_ a b = Dyn ("(((" ++ num a ++ " % " ++ num b ++ ") + " ++ num b ++ ") % " ++ num b ++ ")")
----------------------------------------------------------------------------
cat :: [Str_] -> Str_
cat parts = SDyn (intercalate " + " (map str parts))
----------------------------------------------------------------------------
-- | @ctx.fillStyle = "rgb(r,g,b)"@; a static colour becomes one literal.
fillStyleRGB :: Num_ -> Num_ -> Num_ -> Stmts
fillStyleRGB (Sta r) (Sta g) (Sta b) =
  ["ctx.fillStyle = " ++ show ("rgb(" ++ int (Sta r) ++ "," ++ int (Sta g) ++ "," ++ int (Sta b) ++ ")") ++ ";"]
fillStyleRGB r g b =
  ["ctx.fillStyle = 'rgb(' + " ++ int r ++ " + ',' + " ++ int g ++ " + ',' + " ++ int b ++ " + ')';"]
----------------------------------------------------------------------------
fillRect :: Num_ -> Num_ -> Num_ -> Num_ -> Stmts
fillRect x y w h = ["ctx.fillRect(" ++ intercalate ", " (map num [x, y, w, h]) ++ ");"]
----------------------------------------------------------------------------
save, restore :: Stmts
save = ["ctx.save();"]
restore = ["ctx.restore();"]
----------------------------------------------------------------------------
translate :: Num_ -> Num_ -> Stmts
translate x y = ["ctx.translate(" ++ num x ++ ", " ++ num y ++ ");"]
----------------------------------------------------------------------------
font :: String -> Stmts
font f = ["ctx.font = " ++ show f ++ ";"]
----------------------------------------------------------------------------
fillText :: Str_ -> Num_ -> Num_ -> Stmts
fillText s x y = ["ctx.fillText(" ++ str s ++ ", " ++ num x ++ ", " ++ num y ++ ");"]
----------------------------------------------------------------------------
-- | Draw every entity of the array; the body is generated once and runs in
-- a JavaScript loop.
forEntities :: (Entity -> Stmts) -> Stmts
forEntities body =
  [ "for (let i = 0, o = base >> 3; i < count; i++, o += " ++ show stride ++ ") {" ]
  ++ map ("  " ++) (body entity)
  ++ [ "}" ]
  where
    field k = Dyn ("heap[o + " ++ show (k :: Int) ++ "]")
    entity = Entity (field 0) (field 1) (field 2) (field 3) (field 4) (field 5)
----------------------------------------------------------------------------
-- | The source of the frame function.
renderFrame :: Stmts -> String
renderFrame stmts =
  unlines $
    [ "(ctx, heap, base, count, t, fps) => {" ]
    ++ map ("  " ++) stmts
    ++ [ "}" ]
----------------------------------------------------------------------------
-- | The benchmark: the same frame as Main.drawScene, as generated code.
benchScene :: Stmts
benchScene = concat
  [ fillStyleRGB (Sta 20) (Sta 20) (Sta 40)
  , fillRect (Sta 0) (Sta 0) (Sta 800) (Sta 600)
  , save
  , translate (sub (fmod_ (mul t (Sta 0.05)) (Sta 40)) (Sta 20))
              (sub (fmod_ (mul t (Sta 0.03)) (Sta 40)) (Sta 20))
  , forEntities $ \e ->
      fillStyleRGB (er e) (eg e) (eb e)
      ++ fillRect (ex e) (ey e) (esize e) (esize e)
  , restore
  , fillStyleRGB (Sta 255) (Sta 255) (Sta 255)
  , font "16px monospace"
  , fillText (cat [SStr "fps: ", SDyn "fps.toFixed(1)", SStr "  rects: ", SDyn "count"]) (Sta 10) (Sta 24)
  ]
  where
    t = Dyn "t"
----------------------------------------------------------------------------
