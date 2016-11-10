{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.FPS ( FPS (..), startFPS ) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.JSString                 (JSString)
import qualified Data.JSString                 as JS
import           Data.String
import           JavaScript.Web.AnimationFrame

import           Miso.Event.Delegate
import           Miso.Html
import           Miso.Html.Diff
import           Miso.Signal
import           Miso.Types

data FPS = FPS {
    fps      :: Double
  , fpsString :: JSString
  , previous :: Double
  , elapsed  :: Double
  , frame :: Integer
  } deriving (Show, Eq)

startFPS
  :: ( HasAction action model stepConfig
     , Eq model
     ) => model
       -> (FPS -> model -> View action)
       -> (action -> model -> Effect action model)
       -> Settings stepConfig action
       -> IO ()
startFPS initialModel view update Settings{..} = do
  fpsRef <- initFPS
  let mergedSignals = mergeManySignals (fst defaultSignal : extraSignals)
  initialFPS <- readIORef fpsRef
  initialVTree <- runView (view initialFPS initialModel)
  Nothing `diff` (Just initialVTree)
  vTreeRef <- newIORef initialVTree
  void . forkIO $ delegator vTreeRef events
  step <- start $ foldp stepConfig update initialModel mergedSignals
  forever $ do
    fps <- updateFPS fpsRef
    step >>= \model -> do
      newVTree <- runView $ view fps (fromChanged model)
      oldVTree <- readIORef vTreeRef
      Just oldVTree `diff` Just newVTree
      writeIORef vTreeRef newVTree

initFPS :: IO (IORef FPS)
initFPS = newIORef $ FPS 0.0 mempty 0.0 0.0 0

updateFPS :: IORef FPS -> IO FPS
updateFPS ref = do
  new <- waitForAnimationFrame
  FPS {..} <- readIORef ref
  let newFPS = FPS {
       fps = 1000 / elapsed
     , elapsed = new - previous
     , previous = new
     , frame = frame + 1
     , fpsString = JS.take 5 $ fromString $ show fps
     }
  writeIORef ref newFPS
  pure newFPS


