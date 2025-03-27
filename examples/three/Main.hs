{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map as M

import Language.Javascript.JSaddle hiding ((<#))

import Miso

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
    cube <-
        join $
            newMesh
                <$> newBoxGeometry 1 1 1
                <*> newMeshBasicMaterial
    addToScene scene cube
    positionCamera camera 5
    stats <- newStats
    statsContainer <- getElementById "stats"
    addStatsToDOM statsContainer stats
    writeIORef
        ref
        Context
            { stats = stats
            , rotateCube = do
                rotateX cube 0.1
                rotateY cube 0.1
            , renderScene =
                render renderer scene camera
            }

main :: IO ()
main = run $ do
    stats <- newStats
    ref <- newIORef $ Context (pure ()) (pure ()) stats
    m <- now
    startApp (defaultApp m (updateModel ref) viewModel)
      { initialAction = Just Init
      }

viewModel :: Double -> View action
viewModel _ =
    div_
        []
        [ div_
            [ id_ "stats"
            , style_ $ M.singleton "position" "absolute"
            ]
            []
        , canvas_
            [ id_ "canvas"
            , width_ "400"
            , height_ "300"
            ]
            []
        ]

updateModel ::
    IORef Context ->
    Action ->
    Effect Double Action ()
updateModel ref Init = do
  io (initContext ref)
  issue GetTime
updateModel ref GetTime = do
  io $ do
    Context{..} <- liftIO (readIORef ref)
    withStats stats $ do
      rotateCube
      renderScene
  scheduleIO (SetTime <$> now)
updateModel _ (SetTime m) = do
  noEff m
  issue GetTime

#ifdef GHCJS_NEW
foreign import javascript unsafe "(() => { return new Stats(); })"
  newStats :: IO JSVal

foreign import javascript unsafe "((x) => { x.begin(); })"
  statsBegin :: JSVal -> IO ()

foreign import javascript unsafe "((x) => { x.end(); })"
  statsEnd :: JSVal -> IO ()

foreign import javascript unsafe "((x) => { x.showPanel(0); })"
  showPanel :: JSVal -> IO ()

foreign import javascript unsafe "(() => { return new THREE.Scene();})"
  newScene :: IO JSVal

foreign import javascript unsafe "((x,y,z) => { return new THREE.BoxGeometry(x,y,z); })"
  newBoxGeometry :: Int -> Int -> Int -> IO JSVal

foreign import javascript unsafe "(() => { return new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 ); })"
  newCamera :: IO JSVal

foreign import javascript unsafe "((x,y) => { return new THREE.Mesh( x, y ); })"
  newMesh :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "(() => { return new THREE.MeshBasicMaterial( { color: 0x00ff00 } ); })"
  newMeshBasicMaterial :: IO JSVal

foreign import javascript unsafe "((x) => { return new THREE.WebGLRenderer({canvas:x, antialias : true}); })"
  newRenderer :: JSVal -> IO JSVal

foreign import javascript unsafe "((x) => { x.setSize( window.innerWidth, window.innerHeight ); })"
  setSize :: JSVal -> IO ()

foreign import javascript unsafe "((x, y) => { x.add(y); })"
  addToScene :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((x, y) => { x.position.z = y; })"
  cameraZ :: JSVal -> Int -> IO ()

foreign import javascript unsafe "((a, y) => { a.rotation.x += y; })"
  rotateX :: JSVal -> Double -> IO ()

foreign import javascript unsafe "((x, a) => { x.rotation.y += a; })"
  rotateY :: JSVal -> Double -> IO ()

foreign import javascript unsafe "((x, y, z) => { x.render(y, z); })"
  render :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "((x, y) => { x.position.z = y; })"
  positionCamera :: JSVal -> Double -> IO ()

foreign import javascript unsafe "((x, y) => { x.appendChild( y.domElement ); })"
  addStatsToDOM :: JSVal -> JSVal -> IO ()
#endif

#ifdef GHCJS_OLD
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
#endif
