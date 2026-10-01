-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A minimal stand-in for GHC's "GHC.StaticPtr" when building with MicroHs,
-- which has no StaticPointers extension.  Static pointers cannot be created
-- (there is no @static@ syntax), so only the types and accessors exist.
-----------------------------------------------------------------------------
module GHC.StaticPtr
  ( StaticPtr
  , StaticKey
  , staticKey
  , deRefStaticPtr
  , unsafeLookupStaticPtr
  , staticPtrKeys
  ) where
-----------------------------------------------------------------------------
import GHC.Fingerprint (Fingerprint (..))
-----------------------------------------------------------------------------
type StaticKey = Fingerprint
-----------------------------------------------------------------------------
data StaticPtr a = StaticPtr StaticKey a
-----------------------------------------------------------------------------
staticKey :: StaticPtr a -> StaticKey
staticKey (StaticPtr k _) = k
-----------------------------------------------------------------------------
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr _ a) = a
-----------------------------------------------------------------------------
unsafeLookupStaticPtr :: StaticKey -> IO (Maybe (StaticPtr a))
unsafeLookupStaticPtr _ = pure Nothing
-----------------------------------------------------------------------------
staticPtrKeys :: IO [StaticKey]
staticPtrKeys = pure []
-----------------------------------------------------------------------------
