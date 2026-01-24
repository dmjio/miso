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
  , new
  , newStdGen
  , newStdGen'
  , next
  ) where
-----------------------------------------------------------------------------
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
-- | Like 'Miso.Random.newStdGen' but takes a t'Seed' as an argument.
newStdGen' :: Seed -> IO StdGen
newStdGen' seed = StdGen . Function <$> FFI.splitmix32 (fromIntegral seed)
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
