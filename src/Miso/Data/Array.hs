-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Array
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Data.Array" is a Haskell wrapper around the JavaScript
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array Array>
-- object. Values of type @'Array' a@ live in JavaScript memory; all
-- operations run in 'IO' and mutate the underlying JS array in place.
--
-- Use this module when you need to pass a JS-native array to a browser API
-- or a third-party JavaScript library. For pure Haskell data processing,
-- prefer ordinary lists or 'Data.Map.Strict.Map'.
--
-- Import qualified to avoid clashing with 'Prelude':
--
-- @
-- import qualified "Miso.Data.Array" as A
-- @
--
-- = Quick start
--
-- @
-- import qualified "Miso.Data.Array" as A
--
-- example :: IO ()
-- example = do
--   arr <- A.'fromList' [10, 20, 30 :: Int]
--   A.'push' 40 arr
--   v   <- A.'lookup' 2 arr    -- Just 30
--   n   <- A.'size' arr        -- 4
--   xs  <- A.'toList' arr      -- [10, 20, 30, 40]
--   pure ()
-- @
--
-- = Operations
--
-- * __Construction__: 'new', 'fromList', 'singleton'
-- * __Deconstruction__: 'toList'
-- * __Access__: 'lookup', '(!?)', 'member', 'size', 'null'
-- * __Mutation__: 'insert', 'push', 'pop', 'shift', 'unshift', 'splice', 'reverse'
--
-- = See also
--
-- * "Miso.Data.Map" — mutable JS 'Miso.Data.Map.Map'
-- * "Miso.Data.Set" — mutable JS 'Miso.Data.Set.Set'
-- * "Miso.DSL" — 'Miso.DSL.ToJSVal' \/ 'Miso.DSL.FromJSVal' used by element types
-----------------------------------------------------------------------------
module Miso.Data.Array
  ( -- * Type
    Array
    -- * Construction
  , new
  , fromList
    -- * Deconstruction
  , toList
    -- * Operations
  , insert
  , push
  , member
  , size
  , splice
  , singleton
  , pop
  , shift
  , unshift
  , null
  , lookup
  , (!?)
  , reverse
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void, forM, forM_)
import           Prelude hiding (lookup, null, reverse)
-----------------------------------------------------------------------------
import           Miso.DSL (jsg, JSVal, ToObject, ToJSVal, FromJSVal, (!))
import qualified Miso.DSL as DSL
import           Miso.FFI (callFunction)
import           Miso.String (ms, unpack)
-----------------------------------------------------------------------------
newtype Array value = Array JSVal deriving (FromJSVal, ToJSVal, ToObject)
-----------------------------------------------------------------------------
-- | Constructs a new JS [Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array) in t'IO'.
--
new :: IO (Array value)
new = Array <$> DSL.new (jsg "Array") ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Inserts a value into the t'Array' by value.
insert
  :: ToJSVal value
  => Int
  -- ^ Index at which to insert the value (0-based)
  -> value
  -- ^ Value to store at that index
  -> Array value
  -- ^ Array to mutate
  -> IO ()
insert key value (Array m) = do
  _ <- DSL.Object m DSL.<## key $ value
  pure ()
-----------------------------------------------------------------------------
-- | Appends a value to the end of the t'Array'.
push
  :: ToJSVal value
  => value
  -- ^ Value to append
  -> Array value
  -- ^ Array to mutate
  -> IO ()
push value (Array m) = do
  _ <- callFunction m "push" [value]
  pure ()
-----------------------------------------------------------------------------
-- | Look up a value in the array by key.
lookup
  :: FromJSVal value
  => Int
  -- ^ 0-based index to look up
  -> Array value
  -- ^ Array to query
  -> IO (Maybe value)
lookup key m = DSL.fromJSValUnchecked =<< m DSL.!! key
-----------------------------------------------------------------------------
-- | Look up a value in the array by index, throwing if out of bounds.
(!?)
  :: FromJSVal value
  => Int
  -- ^ 0-based index to look up
  -> Array value
  -- ^ Array to query
  -> IO value
(!?) key m =
  lookup key m >>= \case
    Nothing ->
      error ("(!?) index out of bounds: " <> unpack (ms key))
    Just value ->
      pure value
-----------------------------------------------------------------------------
-- | Return the size of t'Array'.
size :: Array value -> IO Int
size (Array m) = DSL.fromJSValUnchecked =<< m ! "length"
-----------------------------------------------------------------------------
-- | Return the null of t'Array'.
null :: Array value -> IO Bool
null m = (== 0) <$> size m
-----------------------------------------------------------------------------
-- | Checks existence of 'value' in t'Array', returns t'Bool.
member
  :: ToJSVal value
  => value
  -- ^ Value to search for (uses JavaScript @SameValueZero@ equality)
  -> Array value
  -- ^ Array to search
  -> IO Bool
member value (Array m) = DSL.fromJSValUnchecked =<< callFunction m "includes" =<< DSL.toJSVal value
-----------------------------------------------------------------------------
-- | Splices an array. See [splice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice).
splice
  :: ToJSVal value
  => Int
  -- ^ Start index (0-based) at which to begin the splice
  -> Int
  -- ^ Number of elements to remove starting at @start@
  -> [value]
  -- ^ Elements to insert at @start@ after the removal
  -> Array value
  -- ^ Array to mutate in place
  -> IO (Array value)
splice start deleteCount xs (Array m) = do
  s <- DSL.toJSVal start
  d <- DSL.toJSVal deleteCount
  args <- mapM DSL.toJSVal xs
  Array <$> do callFunction m "splice" $ [s,d] ++ args
-----------------------------------------------------------------------------
-- | Construct a t'Array' from a list of values.
fromList
  :: ToJSVal value
  => [value]
  -- ^ Elements to populate the new array with (in order)
  -> IO (Array value)
fromList xs = do
  m <- new
  forM_ (zip [0..] xs) $ \(k,v) ->
    insert k v m
  pure m
-----------------------------------------------------------------------------
-- | Converts an t'Array' to a list.
toList :: FromJSVal value => Array value -> IO [value]
toList m = do
  len <- subtract 1 <$> size m
  forM [0..len] (!? m)
-----------------------------------------------------------------------------
-- | Creates a new Array with a single element.
--
singleton
  :: ToJSVal a
  => a
  -- ^ The single element for the new array
  -> IO (Array a)
singleton x = fromList [x]
-----------------------------------------------------------------------------
-- | Removes the last element from an array and returns it.
--
-- Returns 'Nothing' if the t'Array' is empty.
--
pop :: FromJSVal a => Array a -> IO (Maybe a)
pop (Array arr) = DSL.fromJSValUnchecked =<< callFunction arr "pop" ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Removes the first element from an array and returns it.
--
shift :: FromJSVal a => Array a -> IO (Maybe a)
shift (Array arr) = DSL.fromJSValUnchecked =<< callFunction arr "shift" ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Adds one or more elements to the beginning of an array.
--
unshift
  :: ToJSVal a
  => a
  -- ^ Element to prepend at index 0
  -> Array a
  -- ^ Array to mutate
  -> IO Int
unshift x (Array arr) = DSL.fromJSValUnchecked =<< callFunction arr "unshift" [x]
-----------------------------------------------------------------------------
-- | Reverses an array in-place.
--
reverse :: Array a -> IO ()
reverse (Array arr) = void $ callFunction arr "reverse" ([] :: [JSVal])
-----------------------------------------------------------------------------
