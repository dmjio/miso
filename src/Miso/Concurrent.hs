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
  ( Waiter (..)
  , waiter
  , emptyWaiter
  ) where

import Control.Concurrent

data Waiter
  = Waiter
  { wait :: IO ()
  , serve :: IO ()
  }

-- | Create a new 'Waiter'
waiter :: IO Waiter
waiter = do
  mvar <- newMVar ()
  pure Waiter
    { wait = takeMVar mvar
    , serve = do
        _ <- tryPutMVar mvar ()
        pure ()
    }

emptyWaiter :: IO Waiter
emptyWaiter = do
  mvar <- newEmptyMVar
  pure Waiter
    { wait = takeMVar mvar
    , serve = do
        _ <- tryPutMVar mvar ()
        pure ()
    }

