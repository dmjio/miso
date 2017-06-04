{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Concurrent
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Concurrent (
    Notify (..)
  , newNotify
  , EventWriter (..)
  , newEventWriter
  ) where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

-- | Concurrent API for receiving events and writing to an event sink
data EventWriter action = EventWriter {
    writeEvent :: action -> IO ()
  , getEvent :: IO action
  }

-- | Creates a new `EventWriter`
newEventWriter :: IO () -> IO (EventWriter m)
newEventWriter notify' = do
  chan <- newChan
  pure $ EventWriter (write chan) (readChan chan)
    where
      write chan event =
        void . forkIO $ do
          writeChan chan event
          notify'

-- | Concurrent API for `SkipChan` implementation
data Notify = Notify {
    wait :: IO ()
  , notify :: IO ()
  }

-- | Create a new `Notify`
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

