-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Random
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Random" provides a pseudo-random number generator for miso
-- components and their test infrastructure. It is built on the
-- <https://prng.di.unimi.it/ SplitMix32> algorithm, implemented as a
-- stateful JavaScript function stored in a 'Miso.DSL.Function'.
--
-- Two usage styles are available:
--
-- * __Explicit generator__ — pass a 'StdGen' through your code using
--   'next' (analogous to @System.Random@).
-- * __Global generator__ — use 'replicateRM' or access 'globalStdGen'
--   directly for fire-and-forget random values.
--
-- = Quick start
--
-- @
-- import "Miso.Random"
--
-- -- Explicit generator
-- example :: IO ()
-- example = do
--   gen         <- 'newStdGen'
--   let (v, g') = 'next' gen     -- v :: Double in [0, 1)
--   print v
--
-- -- Global generator (convenience)
-- tenValues :: IO [Double]
-- tenValues = 'replicateRM' 10
--
-- -- Reproducible seed for tests
-- deterministicGen :: 'StdGen'
-- deterministicGen = 'mkStdGen' 42
-- @
--
-- = Seeding
--
-- * 'newStdGen' seeds from @crypto.getRandomValues()@ — cryptographically
--   random, non-reproducible.
-- * 'mkStdGen' takes an explicit 'Seed' (@Int@) — reproducible, useful for
--   property tests or simulations.
-- * 'globalStdGen' is seeded once at module load time from @Math.random()@.
--
-- = See also
--
-- * 'Miso.FFI.Internal.splitmix32' — the raw JS PRNG primitive
-- * 'Miso.FFI.Internal.getRandomValue' — @crypto.getRandomValues()@ used for seeding
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
  , setStdGen
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
mkStdGen
  :: Seed
  -- ^ Initial seed value; identical seeds produce identical sequences
  -> StdGen
mkStdGen seed = StdGen $ Function $ unsafePerformIO $ FFI.splitmix32 (fromIntegral seed)
-----------------------------------------------------------------------------
-- | Create a new t'StdGen', defaulting to a random t'Seed'.
newStdGen :: IO StdGen
newStdGen = do
  seed <- FFI.getRandomValue
  StdGen . Function <$> FFI.splitmix32 seed
-----------------------------------------------------------------------------
-- | Get the next t'StdGen', extracting the value, useful with t'State'.
next
  :: StdGen
  -- ^ Current generator state
  -> (Double, StdGen)
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
-- | Read the `globalStdGen`
getStdGen :: IO StdGen
getStdGen = readIORef globalStdGen
-----------------------------------------------------------------------------
-- | Set the `globalStdGen`
setStdGen
  :: StdGen
  -- ^ New generator to install as the global PRNG
  -> IO ()
setStdGen = atomicWriteIORef globalStdGen
-----------------------------------------------------------------------------
-- | Generate n amount of random numbers. Uses the global PRNG 'globalStdGen'.
--
-- @
-- replicateRM 10 :: IO [Double]
-- @
--
replicateRM
  :: Int
  -- ^ Number of random 'Double' values to generate in @[0, 1)@
  -> IO [Double]
replicateRM n = do
  atomicModifyIORef globalStdGen $ \gen -> do
    swap $ flip runState gen $ replicateM n (state next)
-----------------------------------------------------------------------------
