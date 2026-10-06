----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE CPP                #-}
----------------------------------------------------------------------------
-- | A render intensive 2D canvas benchmark with an FPS counter.
--
-- Every frame draws N coloured rectangles, each with its own 'fillStyle'
-- (the shape of webcc's canvas benchmark: one style set and one fill per
-- rectangle, positions computed once up front).  The frame rate is
-- measured once a second, drawn on the canvas, and logged to the console
-- as @fps: N ms/frame: T rects: M@ so it can be read by automated tests
-- (see bench.mjs).  @?rects=N@ in the URL sets the count.
--
-- @?mode=@ selects the renderer:
--
-- * @canvas@ (default): 'drawScene', hand written against "Miso.Canvas".
-- * @run@: 'Scene.benchScene' executed through the 'Run' instance of
--   'Draw', i.e. "Miso.Canvas" calls with the entity loop in Haskell.
-- * @staged@ (needs @-DSTAGED@, a MicroHs with the 2ltt branch, @make
--   mhs-staged@): the same 'Scene.benchScene' compiled to a JavaScript frame
--   function at compile time by the 'JSGen' instance; Haskell makes one
--   call per frame.
--
-- @run@ and @staged@ read the rectangles from an array in wasm memory.
module Main where
----------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
#ifdef __MHS__
import           Data.Double (doubleToInt)
#endif
----------------------------------------------------------------------------
import           Miso
import           Miso.Canvas (Canvas)
import qualified Miso.Canvas as Canvas
import qualified Miso.CSS.Color as Color
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.String (ms)
import           Miso.Subscription.Canvas (canvasSub)
import           Control.Monad.Reader (ask)
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (pokeElemOff)
import           System.IO.Unsafe (unsafePerformIO)
import           Miso.Canvas.Draw
import           Miso.Canvas.Draw.Run
import           Scene (benchScene)
#ifdef STAGED
import           Staged (codeString)
#endif
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _count :: Int
    -- ^ rectangles drawn per frame
  , _rects :: [Rect]
    -- ^ the first @_count@ rectangles of the fixed layout, 'mkRects'
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Set the rectangle count; the layout follows.
setCount :: Int -> Model -> Model
setCount n m = m { _count = n, _rects = mkRects n }
----------------------------------------------------------------------------
data Rect
  = Rect
  { rectX, rectY, rectSize :: !Double
  , rectColor :: !Color.Color
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- | The first @n@ rectangles of a fixed pseudo random layout.
mkRects :: Int -> [Rect]
mkRects n = take n (go 1)
  where
    -- A small linear congruential generator (Int is 32 bits on wasm32).
    go :: Int -> [Rect]
    go seed =
      let s1 = next seed; s2 = next s1; s3 = next s2; s4 = next s3; s5 = next s4; s6 = next s5
          x = fromIntegral (s1 `mod` 780)
          y = fromIntegral (s2 `mod` 580)
          size = fromIntegral (6 + s3 `mod` 18)
          c = Color.rgb (s4 `mod` 256) (s5 `mod` 256) (s6 `mod` 256)
      in Rect x y size c : go s6
    next s = (s * 75 + 74) `mod` 65537
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = InitCanvas DOMRef
  | StopCanvas
  | MoreRects
  | FewerRects
----------------------------------------------------------------------------
-- | Frame statistics, owned by the draw loop (not the model, so that
-- measuring the frame rate never triggers a virtual DOM diff).
data Stats
  = Stats
  { statFrames   :: !Int
    -- ^ frames since the last fps update
  , statLastTime :: !Double
    -- ^ timestamp (ms) of the last fps update
  , statFps      :: !Double
  }
----------------------------------------------------------------------------
canvasWidth, canvasHeight :: Double
canvasWidth  = 800
canvasHeight = 600
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = do
  stats <- newIORef (Stats 0 0 0)
  n <- initialRects
  mode <- renderMode
  startApp defaultEvents (app stats mode n)
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
app :: IORef Stats -> Mode -> Int -> App Model Action
app stats mode n = component (setCount n (Model 0 [])) (updateModel stats mode) viewModel
----------------------------------------------------------------------------
updateModel :: IORef Stats -> Mode -> Action -> Effect context props Model Action
updateModel stats mode = \case
  InitCanvas domRef ->
    startSub "canvas" $ canvasSub domRef "2d" $ case mode of
      ModeCanvas -> drawScene stats
      ModeRun -> drawRun stats
      ModeStaged -> drawStaged stats
  StopCanvas ->
    stopSub "canvas"
  MoreRects ->
    modify $ \m -> setCount (2 * _count m) m
  FewerRects ->
    modify $ \m -> setCount (max 1 (_count m `div` 2)) m
----------------------------------------------------------------------------
viewModel :: Model -> View () () Model Action
viewModel m =
  H.div_ []
  [ H.div_ []
    [ H.button_ [ H.onClick FewerRects ] [ "fewer rects" ]
    , text (" " <> ms (_count m) <> " rects per frame ")
    , H.button_ [ H.onClick MoreRects ] [ "more rects" ]
    ]
  , H.canvas_
    [ onCreatedWith InitCanvas
    , onDestroyed StopCanvas
    , P.width_ (ms canvasWidth)
    , P.height_ (ms canvasHeight)
    , P.id_ "bench"
    ]
    []
  ]
----------------------------------------------------------------------------
-- | One frame: clear, draw every rectangle (the whole layout drifts with
-- the timestamp so that something visibly moves), then the fps counter.
drawScene :: IORef Stats -> Double -> Model -> Canvas ()
drawScene stats t m = do
  Canvas.fillStyle (Canvas.color (Color.rgb 20 20 40))
  Canvas.fillRect (0, 0, canvasWidth, canvasHeight)
  Canvas.save ()
  Canvas.translate (fmod (t * 0.05) 40 - 20, fmod (t * 0.03) 40 - 20)
  forM_ (_rects m) $ \(Rect x y size c) -> do
    Canvas.fillStyle (Canvas.color c)
    Canvas.fillRect (x, y, size, size)
  Canvas.restore ()
  let n = _count m
  fps <- liftIO (tick stats t n)
  Canvas.fillStyle (Canvas.color Color.white)
  Canvas.font "16px monospace"
  Canvas.fillText ("fps: " <> ms (tenths fps) <> "  rects: " <> ms n, 10, 24)
----------------------------------------------------------------------------
-- | Count a frame; once a second recompute the fps and log it.
tick :: IORef Stats -> Double -> Int -> IO Double
tick stats t n = do
  Stats frames lastTime fps <- readIORef stats
  let frames' = frames + 1
      elapsed = t - lastTime
  if lastTime == 0
    then do
      writeIORef stats (Stats 0 t 0)
      pure 0
    else if elapsed >= 1000
      then do
        let fps' = fromIntegral frames' * 1000 / elapsed
        writeIORef stats (Stats 0 t fps')
        consoleLog $ "fps: " <> ms (tenths fps')
                  <> " ms/frame: " <> ms (round (elapsed / fromIntegral frames') :: Int)
                  <> " rects: " <> ms n
        pure fps'
      else do
        writeIORef stats (Stats frames' lastTime fps)
        pure fps
----------------------------------------------------------------------------
-- | Round to one decimal place.
tenths :: Double -> Double
tenths x = fromIntegral (round (x * 10) :: Int) / 10
----------------------------------------------------------------------------
-- | Floating point modulus.  MicroHs's 'floor' goes through 'Integer'
-- (hundreds of microseconds per call), so use the primitive conversion there.
fmod :: Double -> Double -> Double
fmod x m = x - fromIntegral (toInt (x / m)) * m
  where
#ifdef __MHS__
    toInt :: Double -> Int
    toInt d = let i = doubleToInt d in if fromIntegral i > d then i - 1 else i
#else
    toInt :: Double -> Int
    toInt = floor
#endif
----------------------------------------------------------------------------
-- | Which renderer @?mode=@ asks for.
data Mode = ModeCanvas | ModeRun | ModeStaged

renderMode :: IO Mode
renderMode = do
  m <- js_modeParam
  pure $ case m of
    1 -> ModeRun
#ifdef STAGED
    2 -> ModeStaged
#endif
    _ -> ModeCanvas

foreign import javascript unsafe "(function(m){ return m === 'run' ? 1 : m === 'staged' ? 2 : 0; })(new URLSearchParams(location.search).get('mode'))"
  js_modeParam :: IO Int
----------------------------------------------------------------------------
-- | The entity array for the 'run' and 'staged' renderers, rebuilt when the
-- model's rectangle count changes: the array and its count.  (Keyed on the
-- count, not the list: comparing 10000 rectangles every frame would cost
-- more Haskell time than the frame itself.)
entities :: IORef (Maybe (Ptr Double, Int))
entities = unsafePerformIO (newIORef Nothing)
{-# NOINLINE entities #-}

-- | The array for the model's rectangles.
currentEntities :: Model -> IO (Ptr Double, Int)
currentEntities m = do
  st <- readIORef entities
  case st of
    Just (a, n) | n == _count m -> pure (a, n)
    _ -> do
      case st of
        Just (a, _) -> free a
        Nothing -> pure ()
      (a, n) <- fillEntities (_rects m)
      writeIORef entities (Just (a, n))
      pure (a, n)

-- | Write the rectangles into a fresh array of 'entityStride' doubles each.
fillEntities :: [Rect] -> IO (Ptr Double, Int)
fillEntities rs = do
  let n = length rs
  arr <- mallocBytes (max 1 n * entityStride * 8)
  let go _ [] = pure ()
      go i (Rect x y size c : rest) = do
        let (r, g, b) = case c of
              Color.RGB r' g' b' -> (r', g', b')
              Color.RGBA r' g' b' _ -> (r', g', b')
              _ -> (0, 0, 0)
            o = i * entityStride
        pokeElemOff arr o x
        pokeElemOff arr (o + 1) y
        pokeElemOff arr (o + 2) size
        pokeElemOff arr (o + 3) (fromIntegral r)
        pokeElemOff arr (o + 4) (fromIntegral g)
        pokeElemOff arr (o + 5) (fromIntegral b)
        go (i + 1) rest
  go 0 rs
  pure (arr, n)
----------------------------------------------------------------------------
-- | 'benchScene' through the 'Run' instance: Miso.Canvas calls, loop in Haskell.
drawRun :: IORef Stats -> Double -> Model -> Canvas ()
drawRun stats t m = do
  (arr, n) <- liftIO (currentEntities m)
  fps <- liftIO (tick stats t n)
  runDraw benchScene (RunEnv arr n t fps)
----------------------------------------------------------------------------
#ifdef STAGED
-- | The frame function's source, generated by the compiler: the splice runs
-- 'genFrame' at compile time and leaves a string literal behind.
frameSource :: String
frameSource = ~(codeString (genFrame benchScene))

-- | 'benchScene' through the 'JSGen' instance: one JavaScript call per frame.
drawStaged :: IORef Stats -> Double -> Model -> Canvas ()
drawStaged stats t m = do
  ctx <- ask
  liftIO $ do
    (arr, n) <- currentEntities m
    fn <- readIORef frameFn >>= \mf -> case mf of
      Just f -> pure f
      Nothing -> do
        f <- eval (ms frameSource)
        writeIORef frameFn (Just f)
        pure f
    fps <- tick stats t n
    js_frame fn ctx arr n t fps

frameFn :: IORef (Maybe JSVal)
frameFn = unsafePerformIO (newIORef Nothing)
{-# NOINLINE frameFn #-}

-- frame(ctx, heap, base, count, t, fps); Module.HEAPF64 is current after memory growth.
foreign import javascript unsafe "$1($2, Module.HEAPF64, $3, $4, $5, $6)"
  js_frame :: JSVal -> JSVal -> Ptr Double -> Int -> Double -> Double -> IO ()
#else
drawStaged :: IORef Stats -> Double -> Model -> Canvas ()
drawStaged = drawScene
#endif
----------------------------------------------------------------------------
-- | Initial rectangle count: @?rects=N@ in the URL, else 1000.
initialRects :: IO Int
#ifdef __MHS__
initialRects = do
  n <- js_rectsParam
  pure (if n > 0 then n else 1000)

foreign import javascript unsafe "Number(new URLSearchParams(location.search).get('rects')) || 0"
  js_rectsParam :: IO Int
#else
initialRects = pure 1000
#endif
----------------------------------------------------------------------------
