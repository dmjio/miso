-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Data.Function
import           GHC.Generics
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso hiding ((<#))
import qualified Miso.Canvas as Canvas
-----------------------------------------------------------------------------
import           THREE.Internal (Three, (.=), x, y, z, (!.), (+=))
import qualified THREE.Scene
import qualified THREE.PerspectiveCamera
import qualified THREE.WebGLRenderer
import qualified THREE.Mesh
import qualified THREE.Object3D
import qualified THREE.MeshBasicMaterial
import qualified THREE.BoxGeometry
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
main :: IO ()
main = run (startComponent app)
-----------------------------------------------------------------------------
app :: Component Double Action
app = component 0 update_ (const three_)
  where
    update_ = \case
      GetTime ->
        io (SetTime <$> now)
      SetTime time ->
        put time
-----------------------------------------------------------------------------
data Action
  = GetTime
  | SetTime !Double
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Implemnetation of custom canvas w/ initialize and draw
-- 'initialize' is called once only, and the result is passed into 'draw'
-- 'draw' is called in a tight loop continuously.
--
three_ :: View Action
three_ = Canvas.canvas [] initialize draw
  where
    initialize :: DOMRef -> Three Context
    initialize canvasRef = do
      width <- windowInnerWidth
      height <- windowInnerHeight
      let value = realToFrac (width `div` height)
      scene <- THREE.Scene.new
      camera <- THREE.PerspectiveCamera.new (75.0, value, 0.1, 1000)
      renderer <- THREE.WebGLRenderer.new (Just (Object canvasRef))
      renderer & THREE.WebGLRenderer.setSize (width, height, True)
      geometry <- THREE.BoxGeometry.new (10,10,10,Nothing,Nothing,Nothing)
      material <- THREE.MeshBasicMaterial.new Nothing
      material & THREE.MeshBasicMaterial.color .= "#000fff"
      cube <- THREE.Mesh.new (geometry,material)
      _  <- scene & THREE.Object3D.add cube
      camera & THREE.Object3D.position !. z .= 300
      pure Context {..}

    draw :: Context -> Three ()
    draw Context {..} = do
      cube & THREE.Object3D.rotation !. x += 0.1
      cube & THREE.Object3D.rotation !. y += 0.1
      renderer & THREE.WebGLRenderer.render (scene, camera)
-----------------------------------------------------------------------------
