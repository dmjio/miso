{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Monad

import Miso
import Miso.String

import Data.IORef

import GHCJS.Types

data Action
  = GetTime
  | Init
  | SetTime !Double

foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

foreign import javascript unsafe "$r = new THREE.Scene();"
  newScene :: IO JSVal

foreign import javascript unsafe "$r = new THREE.BoxGeometry( $1, $2, $3 );"
  newBoxGeometry :: Int -> Int -> Int -> IO JSVal

foreign import javascript unsafe "$r = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );"
  newCamera :: IO JSVal

foreign import javascript unsafe "$r = new THREE.Mesh( $1, $2 );"
  newMesh :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "$r = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );"
  newMeshBasicMaterial :: IO JSVal

foreign import javascript unsafe "$r = new THREE.WebGLRenderer({canvas:$1});"
  newRenderer :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.setSize( window.innerWidth, window.innerHeight );"
  setSize :: JSVal -> IO ()

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: MisoString -> IO JSVal

foreign import javascript unsafe "$1.add($2);"
  addToScene :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.position.z = $2;"
  cameraZ :: JSVal -> Int -> IO ()

foreign import javascript unsafe "$1.rotation.x = $2;"
  rotateX :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.rotation.y = $2;"
  rotateY :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.render($2,$3);"
  render :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.position.z = $2;"
  positionCamera :: JSVal -> Double -> IO ()

data Context = Context
  { rotateCube :: IO ()
  , renderScene :: IO ()
  }

initContext :: IORef Context -> IO ()
initContext ref = do
  canvas <- getElementById "canvas"
  scene <- newScene
  camera <- newCamera
  renderer <- newRenderer canvas
  setSize renderer
  cube <- join $ newMesh
    <$> newBoxGeometry 1 1 1
    <*> newMeshBasicMaterial
  addToScene scene cube
  positionCamera camera 5
  writeIORef ref Context {
    rotateCube = do
      rotateX cube 1.0
      rotateY cube 1.0
  , renderScene =
      render renderer scene camera
  }

main :: IO ()
main = do
  ref <- newIORef $ Context (pure ()) (pure ())
  m <- now
  startApp App { model = m
               , initialAction = Init
               , update = updateModel ref
               , ..
               }
    where
      events = defaultEvents
      view   = viewModel
      subs   = []

viewModel :: Double -> View action
viewModel _ = div_ [] [
    canvas_ [ id_ "canvas"
            , width_ "400"
            , height_ "300"
            ] []
  ]

updateModel
  :: IORef Context
  -> Action
  -> Double
  -> Effect Action Double
updateModel ref Init m = m <# do
  initContext ref
  pure GetTime

updateModel ref GetTime m = m <# do
  Context {..} <- readIORef ref
  putStrLn "foo"
  rotateCube
  renderScene
  SetTime <$> now

updateModel _ (SetTime m) _ =
  m <# pure GetTime



