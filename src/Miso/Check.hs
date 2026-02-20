-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Check
-- Copyright   :  (C) 2025
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  -
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A property-based testing module for Miso applications.
--
-- Uses Miso.Random for generating random test data.
--
-- This is meant to be used internal for the `miso-tests` library.
--
----------------------------------------------------------------------------
module Miso.Check
  ( -- ** Core Types
    Gen
    -- ** Typeclass
  , Arbitrary(..)
    -- ** Generators
  , choose
  , generate
  , elements
  , oneof
  ) where
-----------------------------------------------------------------------------
import           Data.IORef
import           Control.Monad.State
-----------------------------------------------------------------------------
import qualified Miso.Random as R
-----------------------------------------------------------------------------
-- | A generator for random values - now using State StdGen
type Gen a = State R.StdGen a
-----------------------------------------------------------------------------
-- | Run a generator with the global random generator
generate :: Gen a -> IO a
generate s = do
  (x, gen) <- runState s <$> R.getStdGen
  atomicWriteIORef R.globalStdGen gen
  pure x
-----------------------------------------------------------------------------
-- | Choose a random integer between two bounds (inclusive)
choose :: (Int, Int) -> Gen Int
choose range = state (R.randomR range)
-----------------------------------------------------------------------------
-- | Choose a random element from a list
elements :: [a] -> Gen a
elements [] = error "Miso.Check.elements: empty list"
elements xs = state $ \gen ->
  let (idx, gen') = R.randomR (0, length xs - 1) gen
  in (xs !! idx, gen')
-----------------------------------------------------------------------------
-- | Randomly select one of the given generators
oneof :: [Gen a] -> Gen a
oneof [] = error "Miso.Check.oneof: empty list"
oneof gs = do
  idx <- choose (0, length gs - 1)
  gs !! idx
-----------------------------------------------------------------------------
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink _ = []
-----------------------------------------------------------------------------
instance Arbitrary Bool where
  arbitrary = elements [True, False]
  shrink True = [False]
  shrink False = []
-----------------------------------------------------------------------------
instance Arbitrary Int where
  arbitrary = choose (minBound, maxBound)
-----------------------------------------------------------------------------
