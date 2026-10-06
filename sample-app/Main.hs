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
-- Built with @-DSTAGED@ (a MicroHs with the 2ltt branch, @make mhs-staged@)
-- the app also has a staged renderer, selected with @?mode=staged@: the
-- frame is a JavaScript function generated at compile time from "Scene",
-- the rectangles live in an array in wasm memory, and Haskell makes one
-- call per frame.
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
import           Miso.Lens
import           Miso.String (ms)
import           Miso.Subscription.Canvas (canvasSub)
#ifdef STAGED
import           Control.Monad.Reader (ask)
import           Data.Word (Word8)
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Ptr (Ptr, castPtr)
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign.Storable (pokeElemOff)
import           Staged (codeString)
import           Scene (renderFrame, benchScene, stride)
#endif
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _rects :: [Rect]
    -- ^ rectangles drawn per frame
  } deriving (Show, Eq)
----------------------------------------------------------------------------
rects :: Lens Model [Rect]
rects = lens _rects $ \record field -> record { _rects = field }
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
  staged <- stagedMode
  startApp defaultEvents (app stats staged n)
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
app :: IORef Stats -> Bool -> Int -> App Model Action
app stats staged n = component (Model (mkRects n)) (updateModel stats staged) viewModel
----------------------------------------------------------------------------
updateModel :: IORef Stats -> Bool -> Action -> Effect context props Model Action
updateModel stats staged = \case
  InitCanvas domRef
#ifdef STAGED
    | staged -> startSub "canvas" $ stagedCanvasSub domRef stats
#endif
    | otherwise -> startSub "canvas" $ canvasSub domRef "2d" (drawScene stats)
  StopCanvas ->
    stopSub "canvas"
  MoreRects ->
    rects %= \rs -> mkRects (2 * length rs)
  FewerRects ->
    rects %= \rs -> mkRects (max 1 (length rs `div` 2))
----------------------------------------------------------------------------
viewModel :: Model -> View () () Model Action
viewModel m =
  H.div_ []
  [ H.div_ []
    [ H.button_ [ H.onClick FewerRects ] [ "fewer rects" ]
    , text (" " <> ms (length (m ^. rects)) <> " rects per frame ")
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
  forM_ (m ^. rects) $ \(Rect x y size c) -> do
    Canvas.fillStyle (Canvas.color c)
    Canvas.fillRect (x, y, size, size)
  Canvas.restore ()
  let n = length (m ^. rects)
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
-- | @?mode=staged@ in the URL selects the staged renderer (when built in).
stagedMode :: IO Bool
#ifdef STAGED
stagedMode = (/= 0) <$> js_stagedParam

foreign import javascript unsafe "new URLSearchParams(location.search).get('mode') === 'staged' ? 1 : 0"
  js_stagedParam :: IO Int
#else
stagedMode = pure False
#endif
----------------------------------------------------------------------------
#ifdef STAGED
-- | The frame function's source, generated by the compiler: the splice runs
-- 'renderFrame' at compile time and leaves a string literal behind.
frameSource :: String
frameSource = ~(codeString (renderFrame benchScene))

-- | The staged renderer.  The entity array is rebuilt when the model's
-- rectangles change; each frame is one JavaScript call.
stagedCanvasSub :: DOMRef -> IORef Stats -> Sub Model Action
stagedCanvasSub domRef stats = canvasSub domRef "2d" draw
  where
    draw t m = do
      ctx <- Canvas.canvasContextRef <$> ask
      liftIO $ do
        st <- readIORef stagedState
        (frameFn, arr, n) <- case st of
          Just (f, a, rs, n) | rs == m ^. rects -> pure (f, a, n)
          _ -> do
            f <- case st of
              Just (f, a, _, _) -> free a >> pure f
              Nothing -> eval (ms frameSource)
            (a, n) <- fillEntities (m ^. rects)
            writeIORef stagedState (Just (f, a, m ^. rects, n))
            pure (f, a, n)
        fps <- tick stats t n
        js_frame frameFn ctx arr n t fps

-- | Frame function, entity array, the rectangles it was built from, their count.
stagedState :: IORef (Maybe (JSVal, Ptr Word8, [Rect], Int))
stagedState = unsafePerformIO (newIORef Nothing)
{-# NOINLINE stagedState #-}

-- | Write the rectangles into a fresh array of 'stride' doubles each.
fillEntities :: [Rect] -> IO (Ptr Word8, Int)
fillEntities rs = do
  let n = length rs
  arr <- mallocBytes (max 1 n * stride * 8)
  let go _ [] = pure ()
      go i (Rect x y size c : rest) = do
        let (r, g, b) = case c of
              Color.RGB r' g' b' -> (r', g', b')
              Color.RGBA r' g' b' _ -> (r', g', b')
              _ -> (0, 0, 0)
            o = i * stride
        pokeElemOff arr o x
        pokeElemOff arr (o + 1) y
        pokeElemOff arr (o + 2) size
        pokeElemOff arr (o + 3) (fromIntegral r)
        pokeElemOff arr (o + 4) (fromIntegral g)
        pokeElemOff arr (o + 5) (fromIntegral b)
        go (i + 1) rest
  go 0 rs
  pure (castPtrW arr, n)
  where
    castPtrW :: Ptr Double -> Ptr Word8
    castPtrW = castPtr

-- frame(ctx, heap, base, count, t, fps); Module.HEAPF64 is current after memory growth.
foreign import javascript unsafe "$1($2, Module.HEAPF64, $3, $4, $5, $6)"
  js_frame :: JSVal -> JSVal -> Ptr Word8 -> Int -> Double -> Double -> IO ()
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
