-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Random
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A PRNG for use in Component and the Component test infrastructure.
--
-- Uses the splitmix32 algorithm under the hood.
--
----------------------------------------------------------------------------
module Miso.Random
  ( -- ** Types
    StdGen (..)
  , Seed
    -- ** Functions
  , newStdGen
  , mkStdGen
  , next
  , replicateRM
    -- ** Globals
  , globalStdGen
  ) where
-----------------------------------------------------------------------------
import           Data.Tuple (swap)
import           Control.Monad.State (state, runState)
import           Control.Monad (replicateM)
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
-----------------------------------------------------------------------------
import           Miso.DSL
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | t'StdGen' holds a JS t'Function'.
newtype StdGen = StdGen Function
-----------------------------------------------------------------------------
-- | An initial 'Seed' value, useful for simulations or reproducing test failures
type Seed = Int
-----------------------------------------------------------------------------
-- | Like 'Miso.Random.newStdGen' but takes a t'Seed' as an argument and is pure.
mkStdGen :: Seed -> StdGen
mkStdGen seed = StdGen $ Function $ unsafePerformIO $ FFI.splitmix32 (fromIntegral seed)
-----------------------------------------------------------------------------
-- | Create a new t'StdGen', defaulting to a random t'Seed'.
newStdGen :: IO StdGen
newStdGen = do
  seed <- FFI.getRandomValue
  StdGen . Function <$> FFI.splitmix32 seed
-----------------------------------------------------------------------------
-- | Get the next t'StdGen', extracting the value, useful with t'State'.
next :: StdGen -> (Double, StdGen)
next (StdGen func) = unsafePerformIO $ do
  result <- apply func ()
  pure (result, StdGen func)
-----------------------------------------------------------------------------
-- | Global 'StdGen', used by 'replicateRM' and others.
globalStdGen :: IORef StdGen
{-# NOINLINE globalStdGen #-}
globalStdGen = unsafePerformIO $ do
  seed <- floor . (*1e7) <$> FFI.mathRandom
  newIORef (mkStdGen seed)
-----------------------------------------------------------------------------
-- | Generate n amount of random numbers. Uses the global PRNG 'globalStdGen'.
--
-- @
-- replicateRM 10 :: IO [Double]
-- @
--
replicateRM :: Int -> IO [Double]
replicateRM n = do
  atomicModifyIORef globalStdGen $ \gen -> do
    swap $ flip runState gen $ replicateM n (state next)
-----------------------------------------------------------------------------
