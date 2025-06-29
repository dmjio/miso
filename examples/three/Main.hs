{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Language.Javascript.JSaddle

import           Miso hiding ((<#))
import           Miso.String
import           Miso.Style ((=:))
import qualified Miso.Style as CSS

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

data Action
  = GetTime
  | Init
  | SetTime !Double

withStats :: Stats -> JSM () -> JSM ()
withStats stats action = do
  statsBegin stats
  action
  statsEnd stats

data Context = Context
  { rotateCube :: JSM ()
  , renderScene :: JSM ()
  , stats :: Stats
  }

initContext :: IORef Context -> JSM ()
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
    liftIO $ writeIORef
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
  ref <- liftIO $ newIORef $ Context (pure ()) (pure ()) stats
  m <- now
  startComponent @"app" (defaultComponent m (updateModel ref) viewModel)
    { initialAction = Just Init
    }

viewModel :: Double -> View action
viewModel _ =
    div_
        []
        [ div_
            [ id_ "stats"
            , CSS.style_ [ "position" =: "absolute" ]
            ]
            []
        , canvas_
            [ id_ "canvas"
            , width_ "400"
            , height_ "300"
            ]
            []
        ]

updateModel
  :: IORef Context
  -> Action
  -> Effect Double Action
updateModel ref Init = do
  io $ do
    initContext ref
    pure GetTime
updateModel ref GetTime = do
  io $ do
    Context{..} <- liftIO (readIORef ref)
    withStats stats $ do
      rotateCube
      renderScene
    SetTime <$> now
updateModel _ (SetTime m) =
  pure GetTime #> m

newtype Stats = Stats { unStats :: JSVal }
  deriving (MakeObject)

newStats :: JSM Stats
newStats = Stats <$> new (jsg @MisoString "Stats") ([] :: [MisoString])

newtype Renderer = Renderer { unRenderer :: JSVal }
  deriving (MakeObject)

newRenderer :: JSVal -> JSM Renderer
newRenderer e = do
  renderer <- jsg ("THREE" :: MisoString) ! ("WebGLRenderer" :: MisoString)
  o <- create
  set "canvas" e o
  set "antialias" True o 
  Renderer <$> new renderer [o]

newtype Scene = Scene { unScene :: JSVal }
  deriving (MakeObject)

newScene :: JSM Scene
newScene = do
  scene <- jsg ("THREE" :: MisoString) ! ("Scene" :: MisoString)
  Scene <$> new scene ([] :: [MisoString])

newtype BoxGeometry = BoxGeometry { unBoxGeometry :: JSVal }
  deriving (MakeObject)

newBoxGeometry :: Double -> Double -> Double -> JSM BoxGeometry
newBoxGeometry x y z = do
  boxGeometry <- jsg ("THREE" :: MisoString) ! ("BoxGeometry" :: MisoString)
  BoxGeometry <$> new boxGeometry [x,y,z]

newtype Camera = Camera { unCamera :: JSVal }
  deriving (MakeObject)

newCamera :: JSM Camera
newCamera = do
  camera <- jsg ("THREE" :: MisoString) ! ("PerspectiveCamera" :: MisoString)
  Just w <- fromJSVal =<< jsg ("window" :: MisoString) ! ("innerWidth" :: MisoString)
  Just h <- fromJSVal =<< jsg ("window" :: MisoString) ! ("innerHeight" :: MisoString)
  Camera <$> new camera [75.0, w / h, 0.1, 1000 :: Double ]

newtype Mesh = Mesh { unMesh :: JSVal }
  deriving (ToJSVal, MakeObject)

newMesh :: ToJSVal material => BoxGeometry -> material -> JSM Mesh
newMesh (BoxGeometry box) m = do
  mesh <- jsg ("THREE" :: MisoString) ! ("Mesh" :: MisoString)
  material <- toJSVal m
  Mesh <$> new mesh [box, material]

newtype MeshBasicMaterial = MeshBasicMaterial { unMeshBasicMaterial :: JSVal }
  deriving (MakeObject, ToJSVal)

newMeshBasicMaterial :: JSM MeshBasicMaterial
newMeshBasicMaterial = do
  material <- jsg ("THREE" :: MisoString) ! ("MeshBasicMaterial" :: MisoString)
  o <- create
  set "color" ("green" :: MisoString) o
  MeshBasicMaterial <$> new material [o]

statsBegin :: Stats -> JSM ()
statsBegin stats = void $ do
  stats # ("begin" :: MisoString) $ ([] :: [JSVal])

statsEnd :: Stats -> JSM ()
statsEnd stats = void $ do
  stats # ("end" :: MisoString) $ ([] :: [JSVal])

render :: Renderer -> Scene -> Camera -> JSM ()
render renderer (Scene scene) (Camera camera) = do
  void $ do renderer # ("render" :: MisoString) $ [scene, camera]

addStatsToDOM :: JSVal -> Stats -> JSM ()
addStatsToDOM e stats = do
  child <- stats ! ("domElement" :: MisoString)
  void $ e # ("appendChild" :: MisoString) $ [child]

setSize :: Renderer -> JSM ()
setSize e = do
  w <- jsg ("window" :: MisoString) ! ("innerWidth" :: MisoString)
  h <- jsg ("window" :: MisoString) ! ("innerHeight" :: MisoString)
  void $ e # ("setSize" :: MisoString) $ [w,h]

addToScene :: Scene -> Mesh -> JSM ()
addToScene x y = do
  void $ x # ("add" :: MisoString) $ [y]

rotateY :: Mesh -> Double -> JSM ()
rotateY mesh v = do
  Just y <- fromJSVal =<< (mesh ! ("rotation" :: MisoString) ! ("y" :: MisoString))
  void $ (mesh ! ("rotation" :: MisoString) <# ("y" :: MisoString)) (y + v)

rotateX :: Mesh -> Double -> JSM ()
rotateX mesh v = do
  Just x <- fromJSVal =<< (mesh ! ("rotation" :: MisoString) ! ("x" :: MisoString))
  void $ (mesh ! ("rotation" :: MisoString) <# ("x" :: MisoString)) (x + v)

positionCamera :: Camera -> Double -> JSM ()
positionCamera camera v = void $ do
  (camera ! ("position" :: MisoString) <# ("z" :: MisoString)) v
