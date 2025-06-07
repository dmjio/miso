{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bool
import Data.Function

import Miso
import Miso.String
import qualified Miso.Style as CSS

data Action
  = GetArrows !Arrows
  | Time !Double
  | WindowCoords !(Int, Int)
  | Start

spriteFrames :: [MisoString]
spriteFrames = ["0 0", "-74px 0", "-111px 0", "-148px 0", "-185px 0", "-222px 0", "-259px 0", "-296px 0"]

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do
    time <- now
    let m = mario{time = time}
    startComponent (defaultComponent m updateMario display)
      { subs =
          [ arrowsSub GetArrows
          , windowCoordsSub WindowCoords
          ]
      , initialAction = Just Start
      }

data Model = Model
    { x :: !Double
    , y :: !Double
    , vx :: !Double
    , vy :: !Double
    , dir :: !Direction
    , time :: !Double
    , delta :: !Double
    , arrows :: !Arrows
    , window :: !(Int, Int)
    }
    deriving (Show, Eq)

data Direction
    = L
    | R
    deriving (Show, Eq)

mario :: Model
mario =
    Model
        { x = 0
        , y = 0
        , vx = 0
        , vy = 0
        , dir = R
        , time = 0
        , delta = 0
        , arrows = Arrows 0 0
        , window = (0, 0)
        }

updateMario :: Action -> Effect Model Action
updateMario Start = get >>= step
updateMario (GetArrows arrs) = do
  modify newModel
  step =<< get
    where
      newModel m = m { arrows = arrs }
updateMario (Time newTime) = do
  modify newModel
  step =<< get
    where
      newModel m = m
        { delta = (newTime - time m) / 20
        , time = newTime
        }
updateMario (WindowCoords coords) = do
  modify newModel
  step =<< get
    where
      newModel m = m { window = coords }

step :: Model -> Effect Model Action
step m@Model{..} = k <# Time <$> now
  where
    k =
        m
            & gravity delta
            & jump arrows
            & walk arrows
            & physics delta

jump :: Arrows -> Model -> Model
jump Arrows{..} m@Model{..} =
    if arrowY > 0 && vy == 0
        then m{vy = 6}
        else m

gravity :: Double -> Model -> Model
gravity dt m@Model{..} =
    m{vy = if y > 0 then vy - (dt / 4) else 0}

physics :: Double -> Model -> Model
physics dt m@Model{..} =
    m
        { x = x + dt * vx
        , y = max 0 (y + dt * vy)
        }

walk :: Arrows -> Model -> Model
walk Arrows{..} m@Model{..} =
    m
        { vx = fromIntegral arrowX
        , dir =
            if
                | arrowX < 0 -> L
                | arrowX > 0 -> R
                | otherwise -> dir
        }

display :: Model -> View action
display m@Model{..} = marioImage
  where
    (h, w) = window
    groundY = 62 - (fromIntegral (fst window) / 2)
    marioImage =
        div_
            [ height_ $ ms h
            , width_ $ ms w
            ]
            [ nodeHtml "style" [] ["@keyframes play { 100% { background-position: -296px; } }"]
            , div_ [CSS.style_ (marioStyle m groundY)] []
            ]

marioStyle :: Model -> Double -> [CSS.Style]
marioStyle Model{..} gy =
        [ ("transform", matrix dir x $ abs (y + gy))
        , ("display", "block")
        , ("width", "37px")
        , ("height", "37px")
        , ("background-color", "transparent")
        , ("background-image", "url(imgs/mario.png)")
        , ("background-repeat", "no-repeat")
        , ("background-position", spriteFrames !! frame)
        , bool mempty ("animation", "play 0.8s steps(8) infinite") (y == 0 && vx /= 0)
        ]
  where
    frame
        | y > 0 = 1
        | otherwise = 0

matrix :: Direction -> Double -> Double -> MisoString
matrix dir x y =
    "matrix("
        <> (if dir == L then "-1" else "1")
        <> ",0,0,1,"
        <> ms x
        <> ","
        <> ms y
        <> ")"
