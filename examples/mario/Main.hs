{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf        #-}
module Main where

import Miso
import Data.Monoid
import Miso.String
import Data.Function
import qualified Data.Map as M

data Action
  = GetArrows Arrows
  | Time Double
  | WindowCoords (Int,Int)

foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

main :: IO ()
main = do
    time <- now
    let m = mario { time = time }
    startApp App { model = m, ..}
  where
    update = updateMario
    view   = display
    events = defaultEvents
    subs   = [ keyboardSub (GetArrows . toArrows)
             , windowSub WindowCoords
             ]

data Model = Model
    { x :: Double
    , y :: Double
    , vx :: Double
    , vy :: Double
    , dir :: Direction
    , time :: Double
    , delta :: Double
    , arrows :: Arrows
    , window :: (Int,Int)
    } deriving (Show, Eq)

data Direction
  = L
  | R
  deriving (Show,Eq)

mario :: Model
mario = Model
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = R
    , time = 0
    , delta = 0
    , arrows = Arrows 0 0
    , window = (0,0)
    }

updateMario :: Action -> Model -> Effect Model Action
updateMario (GetArrows arrs) m = step newModel
  where
    newModel = m { arrows = arrs }
updateMario (Time newTime) m = step newModel
  where
    newModel = m { delta = (newTime - time m) / 20
                 , time = newTime
                 }
updateMario (WindowCoords coords) m = step newModel
  where
    newModel = m { window = coords }

step :: Model -> Effect Model Action
step m@Model{..} = k <# do Time <$> now
  where
    k = m & gravity delta
          & jump arrows
          & walk arrows
          & physics delta

jump :: Arrows -> Model -> Model
jump Arrows{..} m@Model{..} =
    if arrowY > 0 && vy == 0
      then m { vy = 6 }
      else m

gravity :: Double -> Model -> Model
gravity dt m@Model{..} =
  m { vy = if y > 0 then vy - (dt / 4) else 0 }

physics :: Double -> Model -> Model
physics dt m@Model{..} =
  m { x = x + dt * vx
    , y = max 0 (y + dt * vy)
    }

walk :: Arrows -> Model -> Model
walk Arrows{..} m@Model{..} =
  m { vx = fromIntegral arrowX
    , dir = if | arrowX < 0 -> L
               | arrowX > 0 -> R
               | otherwise -> dir
    }

display :: Model -> View action
display m@Model{..} = marioImage
  where
    verb = if | y > 0 -> "jump"
              | vx /= 0 -> "walk"
              | otherwise -> "stand"
    d = case dir of
            L -> "left"
            R -> "right"
    src  = "imgs/"<> verb <> "/" <> d <> ".gif"
    groundY = 62 - (fromIntegral (fst window) / 2)
    marioImage =
      div_ [ height_ (pack (show (fst window)))
           , height_ (pack (show (snd window)))
           ] [ img_ [ height_ "37"
                    , width_ "37"
                    , src_ src
                    , style_ (marioStyle m groundY)
                    ] [] ]

marioStyle :: Model -> Double -> M.Map MisoString MisoString
marioStyle Model {..} gy =
  M.fromList [ ("transform", matrix x (y + gy) )
             , ("display", "block")
             ]

matrix :: Double -> Double -> MisoString
matrix x y =
  "matrix(1,0,0,1,"
     <> pack (show x)
     <> ","
     <> pack (show (abs y))
     <> ")"
