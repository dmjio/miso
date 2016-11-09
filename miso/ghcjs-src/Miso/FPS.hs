{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.FPS where

import Control.Concurrent
import Control.Monad
import JavaScript.Web.AnimationFrame

import Miso.Signal
import Miso.Types

import Numeric

data FPS = FPS {
   fps :: Signal Double
 }

fpsSignal :: IO FPS
fpsSignal = do
  ref <- newMVar []
  (fps, sink) <- signal
  startFRP ref sink
  pure FPS {..}

startFRP :: MVar [Double]
         -> (Double -> IO a)
         -> IO ()
startFRP ref sink = void . forkIO . forever $ do
    current <- waitForAnimationFrame
    result <- readMVar ref
    if length result == 150
      then do
        modifyMVar_ ref $ \xs -> do
          let newWindow = current : init xs
          newWindow <$ sink (getFPS newWindow)
      else
        if length result < 150
          then do
            modifyMVar_ ref $ \xs -> pure (current : xs)
          else do
            putStrLn "shouldn't get here.."
            print (length result)
            pure ()

secs :: Int -> Int
secs = (*1000000)

diffs :: [Double] -> [Double]
diffs times = zipWith subtract (tail times) times

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)

getFPS :: [Double] -> Double
getFPS xs =
  let x = recip . (/1000) . avg . diffs $ xs
  in read $ showFFloat (Just 2) x ""


