-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
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
import           Control.Monad (void)
import           GHC.Generics (Generic)
import           Data.Function
import           Language.Javascript.JSaddle hiding (new)
-----------------------------------------------------------------------------
import           Miso hiding ((<#))
import qualified Miso.Canvas as Canvas
import           Miso.Style ((=:))
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
import qualified THREE.BoxGeometry
import           THREE.Internal (Three, (.=), x, y, z, (!.), (+=), (^.), (!..))
import qualified THREE.Light
import qualified THREE.Mesh
import qualified THREE.MeshBasicMaterial
import qualified THREE.Object3D
import qualified THREE.PerspectiveCamera
import qualified THREE.PointLight
import qualified THREE.Scene
import qualified THREE.Vector3
import qualified THREE.WebGLRenderer
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Context
  = Context
  { cube :: THREE.Mesh.Mesh
  , scene :: THREE.Scene.Scene
  , camera :: THREE.PerspectiveCamera.PerspectiveCamera
  , renderer :: THREE.WebGLRenderer.WebGLRenderer
  } deriving (Generic, ToJSVal, FromJSVal)
-----------------------------------------------------------------------------
data Action
  = GetTime
  | SetTime !Double
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startComponent app { initialAction = Just GetTime }
-----------------------------------------------------------------------------
app :: Component Double Action
app = component 0 update_ $ \_ -> div_
  []
  [ div_
    [ id_ "stats"
    , CSS.style_
      [ "position" =: "absolute"
      ]
    ]
    []
  , Canvas.canvas_
    [ width_ "400"
    , height_ "300"
    ]
    initContext
    draw
  ] where
      update_ = \case
        GetTime ->
          io (SetTime <$> now)
        SetTime time ->
          pure GetTime #> time
-----------------------------------------------------------------------------
initContext :: DOMRef -> Three Context
initContext canvasRef = do
  width <- windowInnerWidth
  height <- windowInnerHeight
  let value = realToFrac (width `div` height)
  light <- THREE.PointLight.new
  light & THREE.Light.intensity .= 300
  light ^. THREE.Object3D.position !.. THREE.Vector3.setXYZ 8 8 8
  scene <- THREE.Scene.new
  void $ scene & THREE.Object3D.add light
  camera <- THREE.PerspectiveCamera.new (75.0, value, 0.1, 1000)
  renderer <- THREE.WebGLRenderer.new (Just canvasRef)
  renderer & THREE.WebGLRenderer.setSize (width, height, True)
  geometry <- THREE.BoxGeometry.new (10,10,10)
  material <- THREE.MeshBasicMaterial.new Nothing
  material & THREE.MeshBasicMaterial.color .= "#000fff"
  cube <- THREE.Mesh.new (geometry,material)
  _  <- scene & THREE.Object3D.add cube
  camera & THREE.Object3D.position !. z .= 300
  pure Context {..}
-----------------------------------------------------------------------------
draw :: Context -> Three ()
draw Context {..} = do
  cube & THREE.Object3D.rotation !. x += 0.1
  cube & THREE.Object3D.rotation !. y += 0.1
  renderer & THREE.WebGLRenderer.render (scene, camera)
-----------------------------------------------------------------------------
