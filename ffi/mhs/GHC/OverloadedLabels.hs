-----------------------------------------------------------------------------
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.OverloadedLabels
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The 'IsLabel' class for MicroHs, which has no OverloadedLabels syntax.
-- Instances can still be defined and used via 'fromLabel' with a type application.
-----------------------------------------------------------------------------
module GHC.OverloadedLabels (IsLabel (..)) where
-----------------------------------------------------------------------------
import Data.TypeLits (Symbol)
-----------------------------------------------------------------------------
class IsLabel (x :: Symbol) a where
  fromLabel :: a
-----------------------------------------------------------------------------
