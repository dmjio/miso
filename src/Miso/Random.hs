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
-- Uses the splitmix64 algorithm.
--
-- This module is meant to be used for simulations and producing randomized
-- data structures. This is not meant to be used in contexts where cryptographic
-- security is paramount (e.g. passwords).
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
  , getStdGen
  , randomR
    -- ** Globals
  , globalStdGen
  ) where
-----------------------------------------------------------------------------
import           Data.Bits
import           Data.Word (Word64)
import           Data.Tuple (swap)
import           Control.Monad.State (state, runState)
import           Control.Monad (replicateM)
import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
-----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | t'StdGen' holds a JS t'Function'.
newtype StdGen = StdGen Seed
-----------------------------------------------------------------------------
-- | An initial 'Seed' value, useful for simulations or reproducing test failures
type Seed = Word64
-----------------------------------------------------------------------------
randomR :: (Int, Int) -> StdGen -> (Int, StdGen)
randomR (low, high) gen
  | low > high = randomR (high, low) gen
  | low == high = (low, gen)
  | otherwise = (low + scaled, newGen)
  where
    (val, newGen) = next gen
    range = high - low + 1
    scaled = floor (val * fromIntegral range)
-----------------------------------------------------------------------------
-- | Like 'Miso.Random.newStdGen' but takes a t'Seed' as an argument and is pure.
mkStdGen :: Seed -> StdGen
mkStdGen = StdGen
-----------------------------------------------------------------------------
-- | Create a new t'StdGen', defaulting to a random t'Seed'.
newStdGen :: IO StdGen
newStdGen = mkStdGen <$> newSeed
-----------------------------------------------------------------------------
newSeed :: IO Seed
newSeed = floor . (maxSafeInteger*) <$> FFI.mathRandom
-----------------------------------------------------------------------------
-- | Number.MAX_SAFE_INTEGER in JS.
maxSafeInteger :: Double
maxSafeInteger = 9007199254740991.0
-----------------------------------------------------------------------------
-- | Get the next t'StdGen', extracting the value, useful with t'State'.
next :: StdGen -> (Double, StdGen)
next (StdGen seed) = fmap mkStdGen (splitmix seed)
-----------------------------------------------------------------------------
-- | Global 'StdGen', used by 'replicateRM' and others.
globalStdGen :: IORef StdGen
{-# NOINLINE globalStdGen #-}
globalStdGen = unsafePerformIO (newIORef =<< mkStdGen <$> newSeed)
-----------------------------------------------------------------------------
-- | Read the `globalStdGen`
getStdGen :: IO StdGen
getStdGen = readIORef globalStdGen
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
splitmix :: Word64 -> (Double, Word64)
splitmix seed = (fromIntegral (mixed `shiftR` 11) * doubleUlp, nextSeed)
  where
    doubleUlp = 1.0 / 2.0**53
    nextSeed = seed + 0x9e3779b97f4a7c15
    z1 = (seed `xor` (seed `shiftR` 30)) * 0xbf58476d1ce4e5b9
    z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    mixed = z2 `xor` (z2 `shiftR` 31)
-----------------------------------------------------------------------------
