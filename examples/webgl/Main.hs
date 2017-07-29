{-# LANGUAGE RecordWildCards #-}
module Main where

import GHCJS.Three
import GHCJS.Types
import JavaScript.Web.AnimationFrame

import Miso
import Miso.String

data Action
  = InitWebGL
  | Mouse (Int,Int)
  | Step
  | NoOp

data Model
  = Model
  { x :: Int
  , y :: Int
  } deriving (Eq)

data Context
  = Context
  { renderScene :: IO ()
  , updateCube :: (Int,Int) -> IO ()
  , addToDOM :: IO ()
  }

-- | Note, we never change model, so there we never redraw
-- This menas we need to rAF manually
updateModel :: Context -> Action -> Model -> Effect Model Action
updateModel Context {..} action m = m <#
  case action of
    InitWebGL -> do
      addToDOM
      pure Step
    Step -> do
      renderScene
      waitForAnimationFrame
      pure Step
    Mouse coords -> do
      updateCube coords
      renderScene
      waitForAnimationFrame
      pure Step

main :: IO ()
main = do
    ctx <- initContext
    startApp App { initialAction = InitWebGL
                 , update = updateModel ctx
                 , ..
                 }
  where
    model = Model 0 0
    view = const $ div_ [ id_ $ pack "container" ] []
    subs = [ mouseSub Mouse ]
    events = defaultEvents

initContext :: IO Context
initContext = do
   scene <- mkScene
   camera <- mkPerspectiveCamera 45 1 0.1 1000
   renderer <- mkWebGLRenderer []
   setSize 500 500 renderer
   geo <- mkBoxGeometry 1 1 1
   mat <- mkMeshNormalMaterial
   c <- mkColor 0.2 0.3 0.5
   setColor c mat
   cube <- mkMesh geo mat
   add (toGLNode cube) (toGLNode scene)
   cp <- position camera
   setPosition cp {v3z = 5} camera
   pure Context {
     renderScene = render scene camera renderer
   , addToDOM = do
       Just (Element glElem) <- domElement renderer
       c <- getContainer
       appendChild c glElem
   , updateCube = \(x,y) -> do
       cubeR <- rotation cube
       setRotation cubeR { eX = eX cubeR + fromIntegral x
                         , eY = eY cubeR + fromIntegral y
                         } cube
       render scene camera renderer
   }

foreign import
 javascript unsafe "$r = document.getElementById('container');"
   getContainer :: IO JSVal

foreign import
  javascript unsafe "$1.appendChild($2);"
    appendChild :: JSVal -> JSVal -> IO ()
