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

import Control.Concurrent hiding (readChan)
import Control.Concurrent.BoundedChan
import Control.Monad

-- | Concurrent API for receiving events and writing to an event sink
data EventWriter action = EventWriter {
    writeEvent :: action -> IO ()
  , getEvent :: IO action
  }

-- | Creates a new `EventWriter`
newEventWriter :: IO () -> IO (EventWriter m)
newEventWriter notify' = do
  chan <- newBoundedChan 50
  pure $ EventWriter (write chan) (readChan chan)
    where
      write chan event =
        void . forkIO $ do
          void $ tryWriteChan chan $! event
          notify'

-- | Concurrent API for `SkipChan` implementation
data Notify = Notify {
    wait :: IO ()
  , notify :: IO ()
  }

-- | Create a new `Notify`
newNotify :: IO Notify
newNotify = do
  mvar <- newMVar ()
  pure $ Notify
   (takeMVar mvar)
   (() <$ do tryPutMVar mvar $! ())

