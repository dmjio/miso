{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad
import           Data.IORef
import qualified Data.Map      as M
import           GHCJS.Types

import           Miso
import           Miso.String

data Action
  = GetTime
  | Init
  | SetTime !Double

withStats :: JSVal -> IO () -> IO ()
withStats stats m = do
  statsBegin stats >> m
  statsEnd stats

data Context = Context
  { rotateCube :: IO ()
  , renderScene :: IO ()
  , stats :: JSVal
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
  stats <- newStats
  statsContainer <- getElementById "stats"
  addStatsToDOM statsContainer stats
  writeIORef ref Context {
    stats = stats
  , rotateCube = do
      rotateX cube 0.1
      rotateY cube 0.1
  , renderScene =
      render renderer scene camera
  }

main :: IO ()
main = do
  stats <- newStats
  ref <- newIORef $ Context (pure ()) (pure ()) stats
  m <- now
  startApp App { model = m
               , initialAction = Init
               , update = updateModel ref
               , mountPoint = Nothing
               , ..
               }
    where
      events = defaultEvents
      view   = viewModel
      subs   = []

viewModel :: Double -> View action
viewModel _ = div_ [] [
    div_ [ id_ "stats"
         , style_ $ M.singleton "position" "absolute"
         ] []
  , canvas_ [ id_ "canvas"
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
  withStats stats $ do
    rotateCube
    renderScene
  SetTime <$> now

updateModel _ (SetTime m) _ =
  m <# pure GetTime

foreign import javascript unsafe "$r = new Stats();"
  newStats :: IO JSVal

foreign import javascript unsafe "$1.begin();"
  statsBegin :: JSVal -> IO ()

foreign import javascript unsafe "$1.end();"
  statsEnd :: JSVal -> IO ()

foreign import javascript unsafe "$1.showPanel(0);"
  showPanel :: JSVal -> IO ()

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

foreign import javascript unsafe "$r = new THREE.WebGLRenderer({canvas:$1, antialias : true});"
  newRenderer :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.setSize( window.innerWidth, window.innerHeight );"
  setSize :: JSVal -> IO ()

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: MisoString -> IO JSVal

foreign import javascript unsafe "$1.add($2);"
  addToScene :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.position.z = $2;"
  cameraZ :: JSVal -> Int -> IO ()

foreign import javascript unsafe "$1.rotation.x += $2;"
  rotateX :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.rotation.y += $2;"
  rotateY :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.render($2, $3);"
  render :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.position.z = $2;"
  positionCamera :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$1.appendChild( $2.domElement );"
  addStatsToDOM :: JSVal -> JSVal -> IO ()
