{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
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
  { uri :: URI
  } deriving (Eq)

data Context
  = Context
  { updateScene :: Maybe (Int,Int) -> IO ()
  , addToDOM :: IO ()
  }

-- | Note, we never change the model, so therefore we never redraw or rAF
-- This means we need to rAF manually
updateModel :: Context -> Action -> Model -> Effect Model Action
updateModel Context {..} action m = m <#
  case action of
    InitWebGL -> do
      putStrLn "adding to dom"
      addToDOM
      pure Step
    Step -> do
      putStrLn "stepping"
      updateScene Nothing
      pure Step
    Mouse coords -> do
--      print coords
--      updateScene (Just coords)
      pure Step

-- | HasURI typeclass
instance HasURI Model where
  lensURI = makeLens getter setter
    where
      getter = uri
      setter = \m u -> m { uri = u }

main :: IO ()
main = do
    uri <- getCurrentURI
    !ctx <- initContext
    addToDOM ctx
    startApp App { initialAction = InitWebGL
                 , update = updateModel ctx
                 , model = Model uri
                 , ..
                 }
  where
    view (Model _) = div_ [ id_ $ pack "container" ] []
    subs = [ mouseSub Mouse ]
    events = defaultEvents

initContext :: IO Context
initContext = do
   putStrLn "1"
   scene <- mkScene
   putStrLn "2"
   camera <- mkPerspectiveCamera 45 1 0.1 1000
   putStrLn "3"
   renderer <- mkWebGLRenderer []
   putStrLn "4"
   setSize 500 500 renderer
   putStrLn "5"
   geo <- mkBoxGeometry 1 1 1
   putStrLn "6"
   mat <- mkMeshNormalMaterial
   putStrLn "7"
   c <- mkColor 0.2 0.3 0.5
   putStrLn "8"
   setColor c mat
   putStrLn "9"
   cube <- mkMesh geo mat
   putStrLn "10"
   add (toGLNode cube) (toGLNode scene)
   putStrLn "11"
   cp <- position camera
   putStrLn "12"
   setPosition cp {v3z = 5} camera
   putStrLn "13"
   pure Context {
     addToDOM = do
       Just (Element glElem) <- domElement renderer
       c <- getContainer (pack "container")
       mapM_ clog [c, glElem]
       c `appendChild` glElem
   , updateScene = \maybeCoords -> do
       waitForAnimationFrame
       cubeR <- rotation cube
       setRotation cubeR { eX = eX cubeR + 0.01
                         , eY = eY cubeR + 0.01
                         } cube
       render scene camera renderer
   }

foreign import
 javascript unsafe "$r = document.getElementById($1);"
   getContainer :: MisoString -> IO JSVal

foreign import
 javascript unsafe "console.log($1);"
   clog :: JSVal -> IO ()

foreign import
  javascript unsafe "$1.appendChild($2);"
    appendChild :: JSVal -> JSVal -> IO ()
