-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Map
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mutable 'Map' data structure in 'IO'.
--
-- A JavaScript [Map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map). This is a convenience for manipulating JavaScript data structures from Haskell.
--
-- We recommend using this module qualified.
--
-- > import qualified Miso.Data.Map as M
--
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
insert :: (ToJSVal key, ToJSVal value) => key -> value -> Map key value -> IO ()
insert key value (Map m) = do
  _ <- callFunction m "set" (key, value)
  pure ()
-----------------------------------------------------------------------------
-- | Finds a value in the t'Map' by key.
lookup :: (ToJSVal key, FromJSVal value) => key -> Map key value -> IO (Maybe value)
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
has :: ToJSVal key => key -> Map key value -> IO Bool
has key (Map m) = DSL.fromJSValUnchecked =<< callFunction m "has" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Removes an entry from a list, returns if the value was removed as t'Bool'.
delete :: ToJSVal key => key -> Map key value -> IO Bool
delete key (Map m) = DSL.fromJSValUnchecked =<< callFunction m "delete" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Construct a t'Map' from a list of key value pairs.
fromList :: (ToJSVal key, ToJSVal value) => [(key, value)] -> IO (Map key value)
fromList xs = do
  m <- new
  forM_ xs $ \(k,v) ->
    insert k v m
  pure m
-----------------------------------------------------------------------------
