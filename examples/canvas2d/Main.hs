{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.State
import GHCJS.Types
import JavaScript.Web.Canvas

import Miso hiding (translate)
import Miso.String

type Model = (Double, Double)

data Action
    = GetTime
    | SetTime Model

main :: IO ()
main = run $ do
    [sun, moon, earth] <- replicateM 3 newImage
    setSrc sun "https://7b40c187-5088-4a99-9118-37d20a2f875e.mdnplay.dev/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_animations/canvas_sun.png"
    setSrc moon "https://7b40c187-5088-4a99-9118-37d20a2f875e.mdnplay.dev/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_animations/canvas_moon.png"
    setSrc earth "https://7b40c187-5088-4a99-9118-37d20a2f875e.mdnplay.dev/en-US/docs/Web/API/Canvas_API/Tutorial/Basic_animations/canvas_earth.png"
    startApp (app sun moon earth) { initialAction = Just GetTime }
  where
    app sun moon earth =
      defaultApp (0.0, 0.0) (updateModel (sun, moon, earth)) view
    view _ =
      canvas_
        [ id_ "canvas"
        , width_ "300"
        , height_ "300"
        ]
        []

updateModel
  :: (Image, Image, Image)
  -> Action
  -> Effect Model Action ()
updateModel _ GetTime = do
  m <- get
  m <# do
    date <- newDate
    (s, m') <- (,) <$> getSecs date <*> getMillis date
    pure $ SetTime (s, m')
updateModel (sun, moon, earth) (SetTime m@(secs, millis)) =
    m <# do
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

#ifdef GHCJS_NEW
foreign import javascript unsafe "((x) => { x.globalCompositeOperation = 'destination-over'; })"
  setGlobalCompositeOperation :: Context -> IO ()

foreign import javascript unsafe "((x, y, z, w) => { w.drawImage(x,y,z); })"
  drawImage' :: Image -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe "(() => { return document.getElementById('canvas').getContext('2d'); })"
  getCtx :: IO Context

foreign import javascript unsafe "(() => { return new Image(); })"
  newImage :: IO Image

foreign import javascript unsafe "((x, y) => { x.src = y; })"
  setSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "(() => { return new Date(); })"
  newDate :: IO JSVal

foreign import javascript unsafe "((x) => { return x.getSeconds(); })"
  getSecs :: JSVal -> IO Double

foreign import javascript unsafe "((x) => { return x.getMilliseconds(); })"
  getMillis :: JSVal -> IO Double
#endif

#ifdef GHCJS_OLD
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
#endif
