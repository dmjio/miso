-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad               (replicateM_)
import           Language.Javascript.JSaddle (JSM, liftJSM)
-----------------------------------------------------------------------------
import           Miso
import           Miso.Canvas
import qualified Miso.Canvas as Canvas
import           Miso.String
import           Miso.Style
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
type Model = (Double, Double)
-----------------------------------------------------------------------------
data Action
  = GetTime
  | SetTime Model
-----------------------------------------------------------------------------
baseUrl :: MisoString
baseUrl = "https://7b40c187-5088-4a99-9118-37d20a2f875e.mdnplay.dev/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_animations/"
-----------------------------------------------------------------------------
main :: IO ()
main = run (startComponent app)
  where
    app :: Component (Double, Double) Action
    app = (component (0.0, 0.0) updateModel view_) { initialAction = Just GetTime }

    view_  m =
      div_
      [ id_ "Canvas grid" ]
      [ Canvas.canvas
        [ width_ "300"
        , height_ "300"
        ]
        initCanvas
        (canvasDraw m n)
      | n <- [ 1 :: Int .. 8 ]
      ]
-----------------------------------------------------------------------------
initCanvas :: DOMRef -> Canvas (Image, Image, Image)
initCanvas _ = liftJSM $ do
  sun <- newImage (baseUrl <> "canvas_sun.png")
  moon <- newImage (baseUrl <> "canvas_moon.png")
  earth <- newImage (baseUrl <> "canvas_earth.png")
  pure (sun, moon, earth)
-----------------------------------------------------------------------------
canvasDraw
  :: (Double, Double)
  -> Int
  -> (Image, Image, Image)
  -> Canvas ()
canvasDraw (millis', secs') n (sun, moon, earth) = do
   let
     secs = secs' + fromIntegral n
     millis = millis' + fromIntegral n
   globalCompositeOperation DestinationOver
   clearRect (0,0,300,300)
   fillStyle $ Canvas.color (rgba 0 0 0 0.6)
   strokeStyle $ Canvas.color (rgba 0 153 255 0.4)
   save ()
   translate (150, 150)
   rotate ((((2 * pi) / 60) * secs) + (((2 * pi) / 60000) * millis))
   translate (105,0)
   fillRect (0 ,-12, 50, 24)
   drawImage (earth, -12, -12)
   save ()
   rotate ((((2 * pi) / 6) * secs) + (((2 * pi) / 6000) * millis))
   translate (0,28.5)
   drawImage (moon, -3.5, -3.5)
   replicateM_ 2 (restore ())
   beginPath ()
   arc (150, 150, 105, 0, pi * 2)
   stroke ()
   drawImage' (sun, 0, 0, 300, 300)
-----------------------------------------------------------------------------
newTime :: JSM (Double, Double)
newTime = do
  date <- newDate
  (,) <$> getMilliseconds date <*> getSeconds date
-----------------------------------------------------------------------------
updateModel
  :: Action
  -> Effect Model Action
updateModel GetTime =
  io (SetTime <$> newTime)
updateModel (SetTime m) =
  m <# pure GetTime
-----------------------------------------------------------------------------
