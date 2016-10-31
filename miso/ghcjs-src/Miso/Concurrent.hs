{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Concurrent ( Notify (..), createNotify )where

import Control.Concurrent.MVar

data Notify = Notify {
    draw :: IO ()
  , notify :: IO ()
  }

createNotify :: IO Notify
createNotify = do
  skipChan <- newSkipChan
  pure $ Notify
    (getSkipChan skipChan)
    (putSkipChan skipChan ())

data SkipChan a =
  SkipChan (MVar (a, [MVar ()])) (MVar ())

newSkipChan :: IO (SkipChan a)
newSkipChan = do
  sem <- newEmptyMVar
  main <- newMVar (undefined, [sem])
  return (SkipChan main sem)

putSkipChan :: SkipChan a -> a -> IO ()
putSkipChan (SkipChan main _) v = do
  (_, sems) <- takeMVar main
  putMVar main (v, [])
  mapM_ (\sem -> putMVar sem ()) sems

getSkipChan :: SkipChan a -> IO a
getSkipChan (SkipChan main sem) = do
  takeMVar sem
  (v, sems) <- takeMVar main
  putMVar main (v, sem:sems)
  return v

