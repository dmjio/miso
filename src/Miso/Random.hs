-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Random
-- Copyright   :  (C) 2016-2026 David M. Johnson
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
-- | Like 'newStdGen' but takes an explicit t'Seed' and is pure.
-- Useful for reproducible tests or simulations.
mkStdGen
  :: Seed
  -- ^ Initial seed value
  -> StdGen
mkStdGen seed = StdGen $ Function $ unsafePerformIO $ FFI.splitmix32 (fromIntegral seed)
-----------------------------------------------------------------------------
-- | Create a new t'StdGen', defaulting to a random t'Seed'.
newStdGen :: IO StdGen
newStdGen = do
  seed <- FFI.getRandomValue
  StdGen . Function <$> FFI.splitmix32 seed
-----------------------------------------------------------------------------
-- | Advance the generator and extract the next random 'Double' in @[0, 1)@.
-- Returns the value paired with the (same, stateful) generator.
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
-- | Replace the 'globalStdGen' with a new generator.
setStdGen
  :: StdGen
  -- ^ New generator to store globally
  -> IO ()
setStdGen = atomicWriteIORef globalStdGen
-----------------------------------------------------------------------------
-- | Generate @n@ random 'Double' values in @[0, 1)@ using the global PRNG 'globalStdGen'.
--
-- @
-- values <- replicateRM 10  -- [Double], length 10
-- @
--
replicateRM
  :: Int
  -- ^ Number of random values to generate
  -> IO [Double]
replicateRM n = do
  atomicModifyIORef globalStdGen $ \gen -> do
    swap $ flip runState gen $ replicateM n (state next)
-----------------------------------------------------------------------------
