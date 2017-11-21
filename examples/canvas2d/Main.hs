{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import GHCJS.Types
import JavaScript.Web.Canvas

import Miso
import Miso.String

type Model = (Double, Double)

data Action
  = NoOp
  | GetTime
  | SetTime Model

main :: IO ()
main = do
  [sun, moon, earth] <- replicateM 3 newImage
  setSrc sun "https://mdn.mozillademos.org/files/1456/Canvas_sun.png"
  setSrc moon "https://mdn.mozillademos.org/files/1443/Canvas_moon.png"
  setSrc earth "https://mdn.mozillademos.org/files/1429/Canvas_earth.png"
  startApp App { initialAction = GetTime
               , update = updateModel (sun,moon,earth)
               , ..
               }
  where
    view _ = canvas_ [ id_ "canvas"
                     , width_ "300"
                     , height_ "300"
                     ] []
    model  = (0.0, 0.0)
    subs   = []
    events = defaultEvents
    mountPoint = Nothing -- default to body

updateModel
  :: (Image,Image,Image)
  -> Action
  -> Model
  -> Effect Action Model
updateModel _ NoOp m = noEff m
updateModel _ GetTime m = m <# do
  date <- newDate
  (s,m') <- (,) <$> getSecs date <*> getMillis date
  pure $ SetTime (s,m')
updateModel (sun,moon,earth) (SetTime m@(secs,millis)) _ = m <# do
  ctx <- getCtx
  setGlobalCompositeOperation ctx
  clearRect 0 0 300 300 ctx
  fillStyle 0 0 0 0.6 ctx
  strokeStyle 0 153 255 0.4 ctx
  save ctx
  translate 150 150 ctx
  flip rotate ctx $ (((2 * pi) / 60) * secs) + (((2 * pi) / 60000) * millis)
  translate 105 0 ctx
  fillRect 0 (-12) 50 24 ctx
  drawImage' earth (-12) (-12) ctx
  save ctx
  flip rotate ctx $ (((2 * pi) / 6) * secs) + (((2 * pi) / 6000) * millis)
  translate 0 28.5 ctx
  drawImage' moon (-3.5) (-3.5) ctx
  replicateM_ 2 (restore ctx)
  beginPath ctx
  arc 150 150 105 0 (pi * 2) False ctx
  stroke ctx
  drawImage sun 0 0 300 300 ctx
  pure GetTime

foreign import javascript unsafe "$1.globalCompositeOperation = 'destination-over';"
  setGlobalCompositeOperation :: Context -> IO ()

foreign import javascript unsafe "$4.drawImage($1,$2,$3);"
  drawImage' :: Image -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO Context

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "$r = new Date();"
  newDate :: IO JSVal

foreign import javascript unsafe "$r = $1.getSeconds();"
  getSecs :: JSVal -> IO Double

foreign import javascript unsafe "$r = $1.getMilliseconds();"
  getMillis :: JSVal -> IO Double

