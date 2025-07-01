-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Concurrent
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Concurrent
  ( -- * Syncrhonization primitives
    Waiter (..)
  , waiter
  , Topic
  , newTopic
  , copyTopic
  , writeTopic
  , readTopic
  ) where
-----------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson
-----------------------------------------------------------------------------
-- | Synchronization primitive for event loop
data Waiter
  = Waiter
  { wait :: IO ()
    -- ^ Blocks on MVar
  , serve :: IO ()
    -- ^ Unblocks threads waiting on MVar
  }
-----------------------------------------------------------------------------
-- | Creates a new 'Waiter' 
waiter :: IO Waiter
waiter = do
  mvar <- newEmptyMVar
  pure Waiter
    { wait = takeMVar mvar
    , serve = do
        _ <- tryPutMVar mvar ()
        pure ()
    }
-----------------------------------------------------------------------------
-- | Publish / Subscribe concurrency primitive
type Topic = TChan Value
-----------------------------------------------------------------------------
newTopic :: IO Topic
newTopic = newBroadcastTChanIO
-----------------------------------------------------------------------------
copyTopic :: Topic -> IO Topic
copyTopic topic = atomically (dupTChan topic)
-----------------------------------------------------------------------------
writeTopic :: Topic -> Value -> IO ()
writeTopic topic value = atomically (writeTChan topic value)
-----------------------------------------------------------------------------
readTopic :: Topic -> IO Value
readTopic topic = atomically (readTChan topic)
-----------------------------------------------------------------------------
