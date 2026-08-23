-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON.Types
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.JSON.Types" defines the two core types used throughout miso's JSON
-- support:
--
-- * 'Value' — an
--   <https://www.json.org/ RFC 8259>-compliant JSON value. Used as the
--   intermediate representation in event decoders
--   ("Miso.Event.Decoder") and in 'Miso.DSL.ToJSVal' \/ 'Miso.DSL.FromJSVal'
--   marshalling.
--
-- * 'Result' — a lightweight error monad (@'Success' a | 'Error' 'Miso.String.MisoString'@)
--   used by JSON parsers to report decode failures. It has full
--   'Functor', 'Applicative', 'Monad', 'MonadFail', 'Alternative',
--   'Foldable', and 'Traversable' instances.
--
-- This module was ported from <https://github.com/dmjio/json-test> by
-- <https://github.com/ners @ners>.
--
-- = Value constructors
--
-- @
-- data 'Value'
--   = 'Number' Double          -- JSON number
--   | t'Bool'   Bool            -- JSON boolean
--   | t'String' 'Miso.String.MisoString'   -- JSON string
--   | 'Array'  ['Value']       -- JSON array
--   | t'Object' ('Miso.JSON.Types.Object')  -- JSON object (Map MisoString Value)
--   | 'Null'                   -- JSON null
-- @
--
-- = See also
--
-- * "Miso.JSON" — top-level re-export hub; 'Miso.JSON.FromJSON', 'Miso.JSON.ToJSON', @(@.:@)@, 'Miso.JSON.withObject'
-- * "Miso.JSON.Parser" — pure server-side JSON decoder ('Miso.JSON.Parser.decodePure')
-- * "Miso.JSON.Lexer" — tokenizer used by the parser
-- * "Miso.Event.Decoder" — uses 'Value' and 'Result' via 'Miso.JSON.Parser'
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
module Miso.JSON.Types
  ( -- * Types
    Value (..)
  , Result (..)
  , Pair
  , Object
  ) where
----------------------------------------------------------------------------
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus(..), ap)
#ifdef AESON
import Data.Aeson.Types (Value (..), Pair, Object)
#else
import Data.Map.Strict (Map)
import Data.String (IsString(fromString))
#endif
----------------------------------------------------------------------------
import Miso.String (MisoString, toMisoString)
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail (..))
#endif
----------------------------------------------------------------------------
#ifndef AESON
-- | A parsed JSON value.
--
-- The JSON data model: numbers are 'Double', objects are keyed by
-- t'Miso.String.MisoString', and 'Null' is explicit. An 'IsString' instance
-- makes string literals usable directly as a t'Value'.
data Value
  = Number Double
  | Bool Bool
  | String MisoString
  | Array [Value]
  | Object (Map MisoString Value)
  | Null
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance IsString Value where
  fromString = String . fromString
----------------------------------------------------------------------------
-- | A single key\/value member of a JSON object, as produced by
-- 'Miso.JSON..=' and consumed by 'Miso.JSON.object'.
type Pair = (MisoString, Value)
----------------------------------------------------------------------------
-- | A JSON object: its members keyed by name.
type Object = Map MisoString Value
#endif
----------------------------------------------------------------------------
-- | The outcome of decoding a t'Value' into a Haskell type.
--
-- 'Error' carries a human-readable message describing where decoding failed.
data Result a
  = Success a
  | Error MisoString
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Error err) = Error err
  {-# INLINE fmap #-}
----------------------------------------------------------------------------
instance Applicative Result where
  pure  = Success
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
----------------------------------------------------------------------------
instance Monad Result where
  return = pure
  {-# INLINE return #-}
  Success a >>= k = k a
  Error err >>= _ = Error err
  {-# INLINE (>>=) #-}
----------------------------------------------------------------------------
instance MonadFail Result where
  fail err = Error $ toMisoString err
  {-# INLINE fail #-}
----------------------------------------------------------------------------
instance Alternative Result where
  empty = mzero
  {-# INLINE empty #-}
  (<|>) = mplus
  {-# INLINE (<|>) #-}
----------------------------------------------------------------------------
instance MonadPlus Result where
  mzero = fail "mzero"
  {-# INLINE mzero #-}
  mplus a@(Success _) _ = a
  mplus _ b             = b
  {-# INLINE mplus #-}
----------------------------------------------------------------------------
instance Semigroup (Result a) where
  (<>) = mplus
  {-# INLINE (<>) #-}
----------------------------------------------------------------------------
instance Monoid (Result a) where
  mempty  = fail "mempty"
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
----------------------------------------------------------------------------
instance Foldable Result where
  foldMap _ (Error _)   = mempty
  foldMap f (Success y) = f y
  {-# INLINE foldMap #-}
----------------------------------------------------------------------------
  foldr _ z (Error _)   = z
  foldr f z (Success y) = f y z
  {-# INLINE foldr #-}
----------------------------------------------------------------------------
instance Traversable Result where
  traverse _ (Error err) = pure (Error err)
  traverse f (Success a) = Success <$> f a
  {-# INLINE traverse #-}
----------------------------------------------------------------------------
