-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON.Types
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Types for the @Miso.JSON@ module and JSON specification.
--
-- This was ported from <https://github.com/dmjio/json-test> by [@ners](https://github.com/ners)
--
----------------------------------------------------------------------------
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
import Data.Map.Strict (Map)
----------------------------------------------------------------------------
import Data.String (IsString(fromString))
import Miso.String (MisoString, toMisoString)
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail (..))
#endif
----------------------------------------------------------------------------
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
type Pair = (MisoString, Value)
----------------------------------------------------------------------------
type Object = Map MisoString Value
----------------------------------------------------------------------------
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
