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
module Miso.Concurrent (
    Notify (..)
  , newNotify
  , newEmptyNotify
  ) where

import Control.Concurrent

-- | Concurrent API for SkipChan implementation
data Notify = Notify {
    wait :: IO ()
  , notify :: IO ()
  }

-- | Create a new 'Notify'
newNotify :: IO Notify
newNotify = do
  mvar <- newMVar ()
  pure $ Notify
   (takeMVar mvar)
   (() <$ do tryPutMVar mvar $! ())

-- | Create a new 'Notify'
newEmptyNotify :: IO Notify
newEmptyNotify = do
  mvar <- newEmptyMVar
  pure $ Notify
   (takeMVar mvar)
   (() <$ do tryPutMVar mvar $! ())

