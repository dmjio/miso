-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Array
-- Copyright   :  (C) 2016-2026 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mutable 'Array' data structure in 'IO'.
--
-- A JavaScript [Array](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array). This is a convenience for manipulating JavaScript data structures from Haskell.
--
-- We recommend using this module qualified.
--
-- > import qualified Miso.Data.Array as M
--
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
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array>
--
-- Sets the element at the given index.
insert
  :: ToJSVal value
  => Int
  -- ^ Zero-based index to write to
  -> value
  -- ^ Value to store at the index
  -> Array value
  -- ^ Array to mutate
  -> IO ()
insert key value (Array m) = do
  _ <- (DSL.Object m) DSL.<## key $ value
  pure ()
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push>
--
-- Appends a value to the end of the t'Array'.
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
-- | Look up an element by zero-based index, returning 'Nothing' if out of bounds.
lookup
  :: FromJSVal value
  => Int
  -- ^ Zero-based index to read
  -> Array value
  -- ^ Array to read from
  -> IO (Maybe value)
lookup key m = DSL.fromJSValUnchecked =<< m DSL.!! key
-----------------------------------------------------------------------------
-- | Look up an element by zero-based index, throwing an error if out of bounds.
(!?)
  :: FromJSVal value
  => Int
  -- ^ Zero-based index to read
  -> Array value
  -- ^ Array to read from
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
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes>
--
-- Tests whether the t'Array' contains a given value.
member
  :: ToJSVal value
  => value
  -- ^ Value to search for
  -> Array value
  -- ^ Array to check
  -> IO Bool
member value (Array m) = DSL.fromJSValUnchecked =<< callFunction m "includes" =<< DSL.toJSVal value
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice>
--
-- Removes and/or inserts elements at a given position, returning the removed elements.
splice
  :: ToJSVal value
  => Int
  -- ^ Start index (zero-based) at which to begin modifying the array
  -> Int
  -- ^ Number of existing elements to remove
  -> [value]
  -- ^ Elements to insert at the start position
  -> Array value
  -- ^ Array to splice in-place
  -> IO (Array value)
splice start deleteCount xs (Array m) = do
  s <- DSL.toJSVal start
  d <- DSL.toJSVal deleteCount
  args <- mapM DSL.toJSVal xs
  Array <$> do callFunction m "splice" $ [s,d] ++ args
-----------------------------------------------------------------------------
-- | Construct a t'Array' from a Haskell list.
fromList
  :: ToJSVal value
  => [value]
  -- ^ Initial elements
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
singleton :: ToJSVal a => a -> IO (Array a)
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
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift>
--
-- Adds an element to the beginning of the t'Array', returning the new length.
unshift
  :: ToJSVal a
  => a
  -- ^ Element to prepend
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
