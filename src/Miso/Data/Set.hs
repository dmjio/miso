-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Data.Set
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
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
-- | Inserts a value into the t'Set' by key.
insert :: ToJSVal key => key -> Set key -> IO ()
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
member :: ToJSVal key => key -> Set key -> IO Bool
member key (Set m) = DSL.fromJSValUnchecked =<< callFunction m "has" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Removes an entry from a list, returns if the value was removed as t'Bool'.
delete :: ToJSVal key => key -> Set key -> IO Bool
delete key (Set m) = DSL.fromJSValUnchecked =<< callFunction m "delete" =<< DSL.toJSVal key
-----------------------------------------------------------------------------
-- | Construct a t'Set' from a list of key value pairs.
fromList :: ToJSVal key => [key] -> IO (Set key)
fromList xs = do
  m <- new
  forM_ xs $ \k ->
    insert k m
  pure m
-----------------------------------------------------------------------------
-- | The union of two t'Set'
union :: ToJSVal key => Set key -> Set key -> IO (Set key)
union (Set x) (Set y) = Set <$> callFunction x "union" [y]
-----------------------------------------------------------------------------
-- | The intersection of two t'Set'
intersection :: ToJSVal key => Set key -> Set key -> IO (Set key)
intersection (Set x) (Set y) = Set <$> callFunction x "intersection" [y]
-----------------------------------------------------------------------------
-- | The symmetric difference of two t'Set'
difference :: ToJSVal key => Set key -> Set key -> IO (Set key)
difference (Set x) (Set y) = Set <$> callFunction x "symmetricDifference" [y]
-----------------------------------------------------------------------------
