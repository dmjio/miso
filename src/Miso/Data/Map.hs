-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Map
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Data.Map" is a Haskell wrapper around the JavaScript
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map Map>
-- object. Values of type @'Map' k v@ live in JavaScript memory; all
-- operations run in 'IO' and mutate the underlying JS map in place.
--
-- Unlike 'Data.Map.Strict.Map', keys do not require an 'Ord' instance —
-- any type with a 'Miso.DSL.ToJSVal' instance can serve as a key, including
-- JS objects and numeric values that compare by identity.
--
-- Use this module when you need to share a key-value structure with a browser
-- API or a third-party JavaScript library. For pure Haskell processing,
-- prefer 'Data.Map.Strict.Map'.
--
-- Import qualified to avoid clashing with 'Prelude':
--
-- @
-- import qualified "Miso.Data.Map" as M
-- @
--
-- = Quick start
--
-- @
-- import qualified "Miso.Data.Map" as M
--
-- example :: IO ()
-- example = do
--   m <- M.'fromList' [(\"a\", 1), (\"b\", 2 :: Int)]
--   M.'insert' \"c\" 3 m
--   v <- M.'lookup' \"b\" m   -- Just 2
--   n <- M.'size' m          -- 3
--   M.'delete' \"a\" m
--   pure ()
-- @
--
-- = Operations
--
-- * __Construction__: 'new', 'fromList'
-- * __Access__: 'lookup', 'has', 'size'
-- * __Mutation__: 'insert', 'delete', 'clear'
--
-- = See also
--
-- * "Miso.Data.Array" — mutable JS 'Miso.Data.Array.Array'
-- * "Miso.Data.Set" — mutable JS 'Miso.Data.Set.Set'
-- * "Miso.DSL" — 'Miso.DSL.ToJSVal' \/ 'Miso.DSL.FromJSVal' used by key and value types
-----------------------------------------------------------------------------
module Miso.Data.Map
  ( -- * Type
    Map
    -- * Construction
  , new
  , fromList
    -- * Operations
  , insert
  , lookup
  , clear
  , size
  , has
  , delete
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void, forM_)
import           Prelude hiding (lookup)
-----------------------------------------------------------------------------
import           Miso.DSL (jsg, JSVal, ToJSVal, FromJSVal, (!))
import qualified Miso.DSL as DSL
import           Miso.FFI (callFunction)
-----------------------------------------------------------------------------
newtype Map key value = Map JSVal deriving (FromJSVal, ToJSVal)
-----------------------------------------------------------------------------
-- | Constructs a new JS [Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) in t'IO'.
--
new :: IO (Map key value)
new = Map <$> DSL.new (jsg "Map") ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Inserts a value into the t'Map' by key.
insert
  :: (ToJSVal key, ToJSVal value)
  => key
  -- ^ Key to associate the value with
  -> value
  -- ^ Value to store
  -> Map key value
  -- ^ Map to mutate
  -> IO ()
insert key value (Map m) = do
  _ <- callFunction m "set" (key, value)
  pure ()
-----------------------------------------------------------------------------
-- | Finds a value in the t'Map' by key.
lookup
  :: (ToJSVal key, FromJSVal value)
  => key
  -- ^ Key to look up
  -> Map key value
  -- ^ Map to query
  -> IO (Maybe value)
lookup key (Map m) = DSL.fromJSValUnchecked =<< callFunction m "get" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Empties the t'Map'.
clear :: Map key value -> IO ()
clear (Map m) = void (callFunction m "clear" ())
-----------------------------------------------------------------------------
-- | Return the size of t'Map'.
size :: Map key value -> IO Int
size (Map m) = DSL.fromJSValUnchecked =<< m ! "size"
-----------------------------------------------------------------------------
-- | Checks existence of a value by 'key', returns t'Bool.
has
  :: ToJSVal key
  => key
  -- ^ Key to test for membership
  -> Map key value
  -- ^ Map to query
  -> IO Bool
has key (Map m) = DSL.fromJSValUnchecked =<< callFunction m "has" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Removes an entry from the t'Map', returns 'True' if the key existed.
delete
  :: ToJSVal key
  => key
  -- ^ Key to remove
  -> Map key value
  -- ^ Map to mutate
  -> IO Bool
delete key (Map m) = DSL.fromJSValUnchecked =<< callFunction m "delete" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Construct a t'Map' from a list of key value pairs.
fromList
  :: (ToJSVal key, ToJSVal value)
  => [(key, value)]
  -- ^ Key-value pairs to populate the new map with
  -> IO (Map key value)
fromList xs = do
  m <- new
  forM_ xs $ \(k,v) ->
    insert k v m
  pure m
-----------------------------------------------------------------------------
