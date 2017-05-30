{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Concurrent (
    Notify (..)
  , newNotify
  , EventWriter (..)
  , newEventWriter
  ) where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

data EventWriter action = EventWriter {
    writeEvent :: action -> IO ()
  , getEvent :: IO action
  }

newEventWriter :: IO () -> IO (EventWriter m)
newEventWriter notify' = do
  chan <- newChan
  pure $ EventWriter (write chan) (readChan chan)
    where
      write chan event =
        void . forkIO $ do
          writeChan chan event
          notify'

data Notify = Notify {
    wait :: IO ()
  , notify :: IO ()
  }

newNotify :: IO Notify
newNotify = do
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

