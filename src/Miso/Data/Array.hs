-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Array
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
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
-- | Inserts a value into the t'Array' by value.
insert :: ToJSVal value => Int -> value -> Array value -> IO ()
insert key value (Array m) = do
  _ <- (DSL.Object m) DSL.<## key $ value
  pure ()
-----------------------------------------------------------------------------
-- | Inserts a value into the t'Array' by value.
push :: ToJSVal value => value -> Array value -> IO ()
push value (Array m) = do
  _ <- callFunction m "push" [value]
  pure ()
-----------------------------------------------------------------------------
-- | Look up a value in the array by key.
lookup :: FromJSVal value => Int -> Array value -> IO (Maybe value)
lookup key m = DSL.fromJSValUnchecked =<< m DSL.!! key
-----------------------------------------------------------------------------
-- | Look up a value in the array by key.
(!?) :: FromJSVal value => Int -> Array value -> IO value
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
member :: ToJSVal value => value -> Array value -> IO Bool
member value (Array m) = DSL.fromJSValUnchecked =<< callFunction m "includes" =<< DSL.toJSVal value
-----------------------------------------------------------------------------
-- | Splices an array. See [splice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice).
splice :: ToJSVal value => Int -> Int -> [value] -> Array value -> IO (Array value)
splice start deleteCount xs (Array m) = do
  s <- DSL.toJSVal start
  d <- DSL.toJSVal deleteCount
  args <- mapM DSL.toJSVal xs
  Array <$> do callFunction m "splice" $ [s,d] ++ args
-----------------------------------------------------------------------------
-- | Construct a t'Array' from a list of value value pairs.
fromList :: ToJSVal value => [value] -> IO (Array value)
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
-- | Adds one or more elements to the beginning of an array.
--
unshift :: ToJSVal a => a -> Array a -> IO Int
unshift x (Array arr) = DSL.fromJSValUnchecked =<< callFunction arr "unshift" [x]
-----------------------------------------------------------------------------
-- | Reverses an array in-place.
--
reverse :: Array a -> IO ()
reverse (Array arr) = void $ callFunction arr "reverse" ([] :: [JSVal])
-----------------------------------------------------------------------------
