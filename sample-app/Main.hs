----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           GHC.Generics (Generic)
----------------------------------------------------------------------------
import           Miso
import           Miso.Canvas
import qualified Miso.CSS.Color as Color
import qualified Miso.FFI as FFI
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.Reload
import           Miso.String (ms)
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io_ (consoleLog "Hello world")
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: props -> Model -> View Model Action
viewModel _ x =
  vfrag
    [ H.button_ [ H.onClick AddOne ] [ text "+" ]
    , text $ ms (x ^. counter)
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.br_ []
    , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
    , H.br_ []
    , canvasDemo (x ^. counter)
    ]
----------------------------------------------------------------------------
-- | Per-canvas state, built once when the element is created: a reusable
-- gradient and image, so drawing each frame only ever refers to their
-- batching handles instead of recreating them.
data CanvasState = CanvasState
  { csGradient :: Gradient
  , csImage    :: FFI.Image
  } deriving (Generic)
----------------------------------------------------------------------------
instance ToJSVal CanvasState
instance FromJSVal CanvasState
----------------------------------------------------------------------------
-- | Exercises the batched 'Miso.Canvas' API in one 'draw' call: numeric
-- path\/transform ops, 'Color.rgba' and 'Color.hex' colors, a 'Gradient'
-- (handle pool), an 'Image' (handle pool + 'drawImage'), an enum setter
-- ('lineJoin'), and text (the one remaining slow path).
canvasDemo :: Int -> View Model Action
canvasDemo n =
  canvas
    [ P.width_ "300", P.height_ "300" ]
    initCanvas
    (drawCanvas n)
----------------------------------------------------------------------------
initCanvas :: DOMRef -> Canvas CanvasState
initCanvas _ = do
  g <- createLinearGradient (0, 0, 300, 0)
  addColorStop (0, Color.hex "ff7f50") g
  addColorStop (1, Color.hex "6495ed") g
  img <- liftIO (FFI.newImage imgDataUri)
  pure (CanvasState g img)
  where
    imgDataUri =
      "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='40' height='40'%3E%3Ccircle cx='20' cy='20' r='18' fill='white' stroke='black'/%3E%3C/svg%3E"
----------------------------------------------------------------------------
drawCanvas :: Int -> CanvasState -> Canvas ()
drawCanvas n (CanvasState g img) = do
  clearRect (0, 0, 300, 300)
  fillStyle (color (Color.rgba 240 240 240 1))
  fillRect (0, 0, 300, 300)
  fillStyle (gradient g)
  fillRect (10, 10, 280, 60)
  save ()
  translate (150, 160)
  rotate (fromIntegral n * (pi / 12))
  lineWidth 4
  lineJoin LineJoinRound
  strokeStyle (color (Color.hex "6495ed"))
  beginPath ()
  arc (0, 0, 50, 0, 2 * pi)
  stroke ()
  fillStyle (color Color.cornflowerblue)
  beginPath ()
  arc (0, 0, 20, 0, 2 * pi)
  fill ()
  restore ()
  drawImage (img, 130, 240)
  font "16px sans-serif"
  textAlign TextAlignCenter
  fillStyle (color (Color.hex "333"))
  fillText ("clicks: " <> ms n, 150, 290)
----------------------------------------------------------------------------
