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
  ( Waiter (..)
  , waiter
  ) where
-----------------------------------------------------------------------------
import Control.Concurrent
-----------------------------------------------------------------------------
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
