-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Set
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Data.Set" is a Haskell wrapper around the JavaScript
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set Set>
-- object. Values of type @'Set' a@ live in JavaScript memory; all
-- operations run in 'IO' and mutate the underlying JS set in place.
--
-- Unlike 'Data.Set.Set', elements do not require an 'Ord' instance —
-- any type with a 'Miso.DSL.ToJSVal' instance can be stored, and equality
-- is determined by JavaScript's
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness SameValueZero>
-- algorithm.
--
-- Use this module when you need to share a set with a browser API or a
-- third-party JavaScript library. For pure Haskell processing, prefer
-- 'Data.Set.Set'.
--
-- Import qualified to avoid clashing with 'Prelude':
--
-- @
-- import qualified "Miso.Data.Set" as S
-- @
--
-- = Quick start
--
-- @
-- import qualified "Miso.Data.Set" as S
--
-- example :: IO ()
-- example = do
--   s  <- S.'fromList' [1, 2, 3 :: Int]
--   S.'insert' 4 s
--   b  <- S.'member' 2 s   -- True
--   n  <- S.'size' s       -- 4
--   S.'delete' 1 s
--   pure ()
-- @
--
-- = Operations
--
-- * __Construction__: 'new', 'fromList'
-- * __Access__: 'member', 'size'
-- * __Mutation__: 'insert', 'delete', 'clear'
-- * __Set algebra__: 'union', 'intersection', 'difference', 'isSubset', 'isSuperset', 'isDisjoint'
--
-- = See also
--
-- * "Miso.Data.Array" — mutable JS 'Miso.Data.Array.Array'
-- * "Miso.Data.Map" — mutable JS 'Miso.Data.Map.Map'
-- * "Miso.DSL" — 'Miso.DSL.ToJSVal' \/ 'Miso.DSL.FromJSVal' used by element types
-----------------------------------------------------------------------------
module Miso.Data.Set
  ( -- * Type
    Set
    -- * Construction
  , new
  , fromList
    -- * Operations
  , insert
  , member
  , clear
  , size
  , delete
  , union
  , intersection
  , difference
  , isSubset
  , isSuperset
  , isDisjoint
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void, forM_)
import           Prelude hiding (lookup)
-----------------------------------------------------------------------------
import           Miso.DSL (jsg, JSVal, ToJSVal, FromJSVal, (!))
import qualified Miso.DSL as DSL
import           Miso.FFI (callFunction)
-----------------------------------------------------------------------------
newtype Set key = Set JSVal deriving (FromJSVal, ToJSVal)
-----------------------------------------------------------------------------
-- | Constructs a new JS [Set](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) in t'IO'.
--
new :: IO (Set key)
new = Set <$> DSL.new (jsg "Set") ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Inserts a value into the t'Set'.
insert
  :: ToJSVal key
  => key
  -- ^ Element to add
  -> Set key
  -- ^ Set to mutate
  -> IO ()
insert key (Set m) = do
  _ <- callFunction m "add" [key]
  pure ()
-----------------------------------------------------------------------------
-- | Empties the t'Set'.
clear :: Set key -> IO ()
clear (Set m) = void (callFunction m "clear" ())
-----------------------------------------------------------------------------
-- | Return the size of t'Set'.
size :: Set key -> IO Int
size (Set m) = DSL.fromJSValUnchecked =<< m ! "size"
-----------------------------------------------------------------------------
-- | Checks existence of 'key' in t'Set', returns t'Bool.
member
  :: ToJSVal key
  => key
  -- ^ Element to test for membership
  -> Set key
  -- ^ Set to query
  -> IO Bool
member key (Set m) = DSL.fromJSValUnchecked =<< callFunction m "has" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Removes an element from the t'Set', returns 'True' if it existed.
delete
  :: ToJSVal key
  => key
  -- ^ Element to remove
  -> Set key
  -- ^ Set to mutate
  -> IO Bool
delete key (Set m) = DSL.fromJSValUnchecked =<< callFunction m "delete" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Construct a t'Set' from a list of elements.
fromList
  :: ToJSVal key
  => [key]
  -- ^ Elements to populate the new set with
  -> IO (Set key)
fromList xs = do
  m <- new
  forM_ xs $ \k ->
    insert k m
  pure m
-----------------------------------------------------------------------------
-- | The union of two t'Set'
union
  :: ToJSVal key
  => Set key -- ^ First set
  -> Set key -- ^ Second set
  -> IO (Set key)
union (Set x) (Set y) = Set <$> callFunction x "union" [y]
-----------------------------------------------------------------------------
-- | The intersection of two t'Set'
intersection
  :: ToJSVal key
  => Set key -- ^ First set
  -> Set key -- ^ Second set
  -> IO (Set key)
intersection (Set x) (Set y) = Set <$> callFunction x "intersection" [y]
-----------------------------------------------------------------------------
-- | The symmetric difference of two t'Set'
difference
  :: ToJSVal key
  => Set key -- ^ First set
  -> Set key -- ^ Second set
  -> IO (Set key)
difference (Set x) (Set y) = Set <$> callFunction x "symmetricDifference" [y]
-----------------------------------------------------------------------------
-- | Checks if one t'Set' is a subset of another t'Set'
isSubset
  :: ToJSVal key
  => Set key -- ^ Candidate subset
  -> Set key -- ^ Potential superset
  -> IO Bool
isSubset (Set x) (Set y) = DSL.fromJSValUnchecked =<<
  callFunction x "isSubsetOf" [y]
-----------------------------------------------------------------------------
-- | Checks if one t'Set' is a superset of another t'Set'
isSuperset
  :: ToJSVal key
  => Set key -- ^ Candidate superset
  -> Set key -- ^ Potential subset
  -> IO Bool
isSuperset (Set x) (Set y) = DSL.fromJSValUnchecked =<<
  callFunction x "isSupersetOf" [y]
-----------------------------------------------------------------------------
-- | Checks if one t'Set' is disjoint from another t'Set'
isDisjoint
  :: ToJSVal key
  => Set key -- ^ First set
  -> Set key -- ^ Second set (disjoint means they share no elements)
  -> IO Bool
isDisjoint (Set x) (Set y) = DSL.fromJSValUnchecked =<<
  callFunction x "isDisjointFrom" [y]
-----------------------------------------------------------------------------
