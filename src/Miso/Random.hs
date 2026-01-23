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
-- | 'StdGen' holds a JS 'Function'.
newtype StdGen = StdGen Function
-----------------------------------------------------------------------------
-- | An initial Seed value, useful for simulations or reproducing test failures
type Seed = Double
-----------------------------------------------------------------------------
-- | Like 'Miso.Random.newStdGen' but takes a 'Seed' as an argument.
newStdGen' :: Seed -> IO StdGen
newStdGen' seed = StdGen . Function <$> FFI.splitmix32 (Just seed)
-----------------------------------------------------------------------------
-- | Create a new t'StdGen', defaulting to a random seed.
newStdGen :: IO StdGen
newStdGen = StdGen . Function <$> FFI.splitmix32 Nothing
-----------------------------------------------------------------------------
-- | Get the next 'StdGen', extracting the value, useful with 'State'.
next :: StdGen -> (Double, StdGen)
next (StdGen func) = unsafePerformIO $ do
  result <- apply func ()
  pure (result, StdGen func)
-----------------------------------------------------------------------------
