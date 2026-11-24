-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StaticPtr
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- GHC's "GHC.StaticPtr" for MicroHs, on top of MicroHs's own static pointers
-- ("Mhs.StaticPtr", the @static@ keyword).  The pointer type is MicroHs's, so
-- @static e@ has the type 'StaticPtr' exported here.
--
-- MicroHs names a static pointer by a string that is the same in every run
-- of the program; the 'StaticKey' is a 128 bit 'Fingerprint' of that name, as
-- in GHC.  Both threads of a Lynx app run the same program, so a key made on
-- one thread finds the same pointer on the other.
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
import           Data.Bits (xor)
import           Data.Char (ord)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Word (Word64)
import           GHC.Fingerprint (Fingerprint (..))
import           Mhs.StaticPtr (StaticPtr, deRefStaticPtr, staticPtrName, staticPtrNames)
import qualified Mhs.StaticPtr as Mhs
-----------------------------------------------------------------------------
type StaticKey = Fingerprint
-----------------------------------------------------------------------------
staticKey :: StaticPtr a -> StaticKey
staticKey = nameKey . staticPtrName
-----------------------------------------------------------------------------
-- | Unsafe, as in GHC: nothing checks that the pointer has type @a@.
unsafeLookupStaticPtr :: StaticKey -> IO (Maybe (StaticPtr a))
unsafeLookupStaticPtr k = pure (M.lookup k keyTable >>= Mhs.unsafeLookupStaticPtr)
-----------------------------------------------------------------------------
staticPtrKeys :: IO [StaticKey]
staticPtrKeys = pure (M.keys keyTable)
-----------------------------------------------------------------------------
-- | The name of every static pointer in the program, by its key.
keyTable :: M.Map StaticKey String
keyTable = M.fromList [ (nameKey n, n) | n <- staticPtrNames ]
{-# NOINLINE keyTable #-}
-----------------------------------------------------------------------------
-- | Two 64 bit FNV-1a hashes of the name, one of it forwards and one of it
-- backwards.
nameKey :: String -> StaticKey
nameKey n = Fingerprint (fnv1a n) (fnv1a (reverse n))
-----------------------------------------------------------------------------
fnv1a :: String -> Word64
fnv1a = foldl' step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3
-----------------------------------------------------------------------------
