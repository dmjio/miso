-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Natural
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The parts of GHC's "GHC.Natural" used by miso, for MicroHs.
-----------------------------------------------------------------------------
module GHC.Natural (Natural, naturalToInteger, naturalFromInteger) where
-----------------------------------------------------------------------------
import Numeric.Natural (Natural)
-----------------------------------------------------------------------------
naturalToInteger :: Natural -> Integer
naturalToInteger = toInteger
-----------------------------------------------------------------------------
naturalFromInteger :: Integer -> Natural
naturalFromInteger = fromInteger
-----------------------------------------------------------------------------
