-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Set
-- Copyright   :  (C) 2016-2026 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mutable 'Set' data structure in 'IO'.
--
-- A JavaScript [Set](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set). This is a convenience for manipulating JavaScript data structures from Haskell.
--
-- We recommend using this module qualified.
--
-- > import qualified Miso.Data.Set as M
--
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
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/add>
--
-- Inserts a value into the t'Set'.
insert
  :: ToJSVal key
  => key
  -- ^ Value to add
  -> Set key
  -- ^ Set to mutate
  -> IO ()
insert key (Set m) = do
  _ <- callFunction m "add" [key]
  pure ()
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/clear>
--
-- Empties the t'Set'.
clear
  :: Set key
  -- ^ Set to clear
  -> IO ()
clear (Set m) = void (callFunction m "clear" ())
-----------------------------------------------------------------------------
-- | Return the size of t'Set'.
size
  :: Set key
  -- ^ Set to measure
  -> IO Int
size (Set m) = DSL.fromJSValUnchecked =<< m ! "size"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/has>
--
-- Tests whether the t'Set' contains a given value.
member
  :: ToJSVal key
  => key
  -- ^ Value to search for
  -> Set key
  -- ^ Set to query
  -> IO Bool
member key (Set m) = DSL.fromJSValUnchecked =<< callFunction m "has" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/delete>
--
-- Removes a value from the t'Set'.
-- Returns 'True' if the value was present, 'False' otherwise.
delete
  :: ToJSVal key
  => key
  -- ^ Value to remove
  -> Set key
  -- ^ Set to mutate
  -> IO Bool
delete key (Set m) = DSL.fromJSValUnchecked =<< callFunction m "delete" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Construct a t'Set' from a list of values.
fromList
  :: ToJSVal key
  => [key]
  -- ^ Initial values
  -> IO (Set key)
fromList xs = do
  m <- new
  forM_ xs $ \k ->
    insert k m
  pure m
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/union>
--
-- Returns a new t'Set' containing all elements from both sets.
union
  :: ToJSVal key
  => Set key
  -- ^ First set
  -> Set key
  -- ^ Second set
  -> IO (Set key)
union (Set x) (Set y) = Set <$> callFunction x "union" [y]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/intersection>
--
-- Returns a new t'Set' containing only elements present in both sets.
intersection
  :: ToJSVal key
  => Set key
  -- ^ First set
  -> Set key
  -- ^ Second set
  -> IO (Set key)
intersection (Set x) (Set y) = Set <$> callFunction x "intersection" [y]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/symmetricDifference>
--
-- Returns a new t'Set' containing elements in either set but not both.
difference
  :: ToJSVal key
  => Set key
  -- ^ First set
  -> Set key
  -- ^ Second set
  -> IO (Set key)
difference (Set x) (Set y) = Set <$> callFunction x "symmetricDifference" [y]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/isSubsetOf>
--
-- Returns 'True' if every element of the first t'Set' is also in the second.
isSubset
  :: ToJSVal key
  => Set key
  -- ^ Candidate subset
  -> Set key
  -- ^ Superset to check against
  -> IO Bool
isSubset (Set x) (Set y) = DSL.fromJSValUnchecked =<<
  callFunction x "isSubsetOf" [y]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/isSupersetOf>
--
-- Returns 'True' if every element of the second t'Set' is also in the first.
isSuperset
  :: ToJSVal key
  => Set key
  -- ^ Candidate superset
  -> Set key
  -- ^ Subset to check against
  -> IO Bool
isSuperset (Set x) (Set y) = DSL.fromJSValUnchecked =<<
  callFunction x "isSupersetOf" [y]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/isDisjointFrom>
--
-- Returns 'True' if the two sets share no elements.
isDisjoint
  :: ToJSVal key
  => Set key
  -- ^ First set
  -> Set key
  -- ^ Second set
  -> IO Bool
isDisjoint (Set x) (Set y) = DSL.fromJSValUnchecked =<<
  callFunction x "isDisjointFrom" [y]
-----------------------------------------------------------------------------
