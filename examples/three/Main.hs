-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           GHC.Generics (Generic)
import           Control.Monad
import           Data.Function
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso hiding ((<#))
import qualified Miso.FFI as FFI
import           Miso.String
import           Miso.Style ((=:))
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Action
  = GetTime
  | SetTime !Double
-----------------------------------------------------------------------------
withStats :: Stats -> Three () -> Three ()
withStats stats action = do
  stats & statsBegin
  action
  stats & statsEnd 
-----------------------------------------------------------------------------
data Context = Context
  { cube :: Mesh
  , scene :: Scene
  , stats :: Stats
  , renderer :: Renderer
  , camera :: Camera
  } deriving (Generic, FromJSVal, ToJSVal)
-----------------------------------------------------------------------------
initialize :: DOMRef -> Three Context
initialize domRef = do
  scene <- newScene
  camera <- newCamera
  renderer <- newRenderer domRef
  renderer & setSize
  geometry <- newBoxGeometry 1 1 1
  basicMaterial <- newMeshBasicMaterial
  cube <- newMesh geometry basicMaterial
  (scene & addToScene) cube
  positionCamera camera 5
  stats <- newStats
  statsContainer <- getElementById "stats"
  addStatsToDOM statsContainer stats
  pure Context {..}
-----------------------------------------------------------------------------
draw :: Context -> Three ()
draw Context {..} = withStats stats $ do
  rotateX cube 0.1
  rotateY cube 0.1
  render renderer scene camera
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startComponent (component 0 updateModel viewModel)
  { initialAction = Just GetTime
  }
-----------------------------------------------------------------------------
viewModel :: Double -> View action
viewModel _ = div_ []
  [ div_
    [ id_ "stats"
    , CSS.style_ [ "position" =: "absolute" ]
    ]
    []
  , canvas
    [ width_ "400"
    , height_ "300"
    ]
    initialize
    draw
  ]
-----------------------------------------------------------------------------
updateModel
  :: Action
  -> Effect Double Action
updateModel GetTime =
  io (SetTime <$> now)
updateModel (SetTime m) =
  pure GetTime #> m
-----------------------------------------------------------------------------
newtype Stats = Stats { unStats :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newStats :: Three Stats
newStats = Stats <$> new (jsg @MisoString "Stats") ([] :: [MisoString])
-----------------------------------------------------------------------------
newtype Renderer = Renderer { unRenderer :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newRenderer :: JSVal -> Three Renderer
newRenderer e = do
  renderer <- jsg ("THREE" :: MisoString) ! ("WebGLRenderer" :: MisoString)
  o <- create
  set "canvas" e o
  set "antialias" True o 
  Renderer <$> new renderer [o]
-----------------------------------------------------------------------------
newtype Scene = Scene { unScene :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newScene :: Three Scene
newScene = do
  scene <- jsg ("THREE" :: MisoString) ! ("Scene" :: MisoString)
  Scene <$> new scene ([] :: [MisoString])
-----------------------------------------------------------------------------
newtype BoxGeometry = BoxGeometry { unBoxGeometry :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newBoxGeometry :: Double -> Double -> Double -> Three BoxGeometry
newBoxGeometry x y z = do
  boxGeometry <- jsg ("THREE" :: MisoString) ! ("BoxGeometry" :: MisoString)
  BoxGeometry <$> new boxGeometry [x,y,z]
-----------------------------------------------------------------------------
newtype Camera = Camera { unCamera :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newCamera :: Three Camera
newCamera = do
  camera <- jsg ("THREE" :: MisoString) ! ("PerspectiveCamera" :: MisoString)
  Just w <- fromJSVal =<< jsg ("window" :: MisoString) ! ("innerWidth" :: MisoString)
  Just h <- fromJSVal =<< jsg ("window" :: MisoString) ! ("innerHeight" :: MisoString)
  Camera <$> new camera [75.0, w / h, 0.1, 1000 :: Double ]
-----------------------------------------------------------------------------
newtype Mesh = Mesh { unMesh :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newMesh :: ToJSVal material => BoxGeometry -> material -> Three Mesh
newMesh (BoxGeometry box) m = do
  mesh <- jsg ("THREE" :: MisoString) ! ("Mesh" :: MisoString)
  material <- toJSVal m
  Mesh <$> new mesh [box, material]
-----------------------------------------------------------------------------
newtype MeshBasicMaterial = MeshBasicMaterial { unMeshBasicMaterial :: JSVal }
  deriving newtype (MakeObject, ToJSVal)
  deriving anyclass (FromJSVal)
  deriving stock Generic
-----------------------------------------------------------------------------
newMeshBasicMaterial :: Three MeshBasicMaterial
newMeshBasicMaterial = do
  material <- jsg ("THREE" :: MisoString) ! ("MeshBasicMaterial" :: MisoString)
  o <- create
  set "color" ("green" :: MisoString) o
  MeshBasicMaterial <$> new material [o]
-----------------------------------------------------------------------------
statsBegin :: Stats -> Three ()
statsBegin stats = void $ do
  stats # ("begin" :: MisoString) $ ([] :: [JSVal])
-----------------------------------------------------------------------------
statsEnd :: Stats -> Three ()
statsEnd stats = void $ stats # ("end" :: MisoString) $ ([] :: [JSVal])
-----------------------------------------------------------------------------
render :: Renderer -> Scene -> Camera -> Three ()
render renderer (Scene scene) (Camera camera) = do
  void $ do renderer # ("render" :: MisoString) $ [scene, camera]
-----------------------------------------------------------------------------
addStatsToDOM :: JSVal -> Stats -> Three ()
addStatsToDOM e stats = do
  child <- stats ! ("domElement" :: MisoString)
  void $ e # ("appendChild" :: MisoString) $ [child]
-----------------------------------------------------------------------------
setSize :: Renderer -> Three ()
setSize renderer = do
  w <- windowInnerWidth
  h <- windowInnerHeight
  void $ renderer # ("setSize" :: MisoString) $ [w,h]
-----------------------------------------------------------------------------
addToScene :: Scene -> Mesh -> Three ()
addToScene x y = do
  void $ x # ("add" :: MisoString) $ [y]
-----------------------------------------------------------------------------
rotateY :: Mesh -> Double -> Three ()
rotateY mesh v = do
  y <- fromJSValUnchecked =<< (mesh ! ("rotation" :: MisoString) ! ("y" :: MisoString))
  void $ (mesh ! ("rotation" :: MisoString) <# ("y" :: MisoString)) (y + v)
-----------------------------------------------------------------------------
rotateX :: Mesh -> Double -> Three ()
rotateX mesh v = do
  x <- fromJSValUnchecked =<< (mesh ! ("rotation" :: MisoString) ! ("x" :: MisoString))
  void $ (mesh ! ("rotation" :: MisoString) <# ("x" :: MisoString)) (x + v)
-----------------------------------------------------------------------------
positionCamera :: Camera -> Double -> Three ()
positionCamera camera = camera ! ("position" :: MisoString) <# ("z" :: MisoString)
-----------------------------------------------------------------------------
type Three = JSM
-----------------------------------------------------------------------------
-- | dmj: This belongs in its own library package called `miso-three`, using three.hs
-- We'll keep this here for now since this example is based on an old three.js
canvas
  :: forall action canvasState
   . (FromJSVal canvasState, ToJSVal canvasState)
  => [ Attribute action ]
  -> (DOMRef -> Three canvasState)
  -- ^ Init function, takes 'DOMRef' as arg, returns canvas init. state.
  -> (canvasState -> Three ())
  -- ^ Callback to render graphics using this canvas' context, takes init state as arg.
  -> View action
canvas attributes initialize_ draw_ = node HTML "canvas" attrs []
  where
    attrs :: [ Attribute action ]
    attrs = initCallback : drawCallack : attributes

    initCallback :: Attribute action
    initCallback = Event $ \_ o _ _ -> do
      flip (FFI.set "onCreated") o =<< do
        FFI.syncCallback1 $ \domRef -> do
          initialState <- initialize_ domRef
          FFI.set "state" initialState (Object domRef)

    drawCallack :: Attribute action
    drawCallack = Event $ \_ o _ _ -> do
      flip (FFI.set "draw") o =<< do
        FFI.syncCallback1 $ \domRef -> do
          state <- fromJSValUnchecked =<< domRef ! ("state" :: MisoString)
          draw_ state
-----------------------------------------------------------------------------
