{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import           Data.Bool
import           Data.Function
import qualified Data.Map      as M
import           Data.Monoid

import           Miso
import           Miso.String

data Action
  = GetArrows !Arrows
  | Time !Double
  | WindowCoords !(Int,Int)
  | NoOp

spriteFrames :: [MisoString]
spriteFrames = ["0 0", "-74px 0","-111px 0","-148px 0","-185px 0","-222px 0","-259px 0","-296px 0"]

main :: IO ()
main = do
    time <- now
    let m = mario { time = time }
    startApp App { model = m, initialAction = NoOp, ..}
  where
    update = updateMario
    view   = display
    events = defaultEvents
    subs   = [ arrowsSub GetArrows
             , windowSub WindowCoords
             ]
    mountPoint = Nothing

data Model = Model
    { x :: !Double
    , y :: !Double
    , vx :: !Double
    , vy :: !Double
    , dir :: !Direction
    , time :: !Double
    , delta :: !Double
    , arrows :: !Arrows
    , window :: !(Int,Int)
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

updateMario :: Action -> Model -> Effect Action Model
updateMario NoOp m = step m
updateMario (GetArrows arrs) m = noEff newModel
  where
    newModel = m { arrows = arrs }
updateMario (Time newTime) m = step newModel
  where
    newModel = m { delta = (newTime - time m) / 20
                 , time = newTime
                 }
updateMario (WindowCoords coords) m = noEff newModel
  where
    newModel = m { window = coords }

step :: Model -> Effect Action Model
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
    (h,w) = window
    groundY = 62 - (fromIntegral (fst window) / 2)
    marioImage =
      div_ [ height_ $ ms h
           , width_ $ ms w
           ] [ div_ [ style_ (marioStyle m groundY) ] [] ]

marioStyle :: Model -> Double -> M.Map MisoString MisoString
marioStyle Model {..} gy =
  M.fromList [ ("transform", matrix dir x $ abs (y + gy) )
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
    frame | y > 0 = 1
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
