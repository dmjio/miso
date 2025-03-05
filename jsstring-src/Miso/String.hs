{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.String
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String
  ( ToMisoString (..)
  , FromMisoString (..)
  , fromMisoString
  , MisoString
  , module Data.Monoid
  , ms
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Prelude hiding (null)

import           Miso.FFI

-- | String type swappable based on compiler
type MisoString = JSString

-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . fromMisoString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)

-- | Convenience class for creating `MisoString` from other string-like types
class ToMisoString str where
  toMisoString :: str -> MisoString
  default toMisoString :: Show str => str -> MisoString
  toMisoString = ms . show

instance ToMisoString Value

-- | Class used to parse a 'MisoString'. Like a safe 'Read' for 'MisoString'
class FromMisoString t where
  -- | Reads a `MisoString`
  fromMisoStringEither :: MisoString -> Either String t

-- | Reads a 'MisoString', throws an error when decoding
-- fails. Use `fromMisoStringEither` for as a safe alternative.
fromMisoString :: FromMisoString a => MisoString -> a
fromMisoString s =
  case fromMisoStringEither s of
    Left err -> error err
    Right x  -> x

-- | Convenience function, shorthand for `toMisoString`
ms :: ToMisoString str => str -> MisoString
ms = toMisoString

instance ToMisoString MisoString where
  toMisoString = id
instance ToMisoString String where
  toMisoString = toJSString
instance ToMisoString T.Text where
  toMisoString = textToJSString
instance ToMisoString LT.Text where
  toMisoString = toMisoString . LT.unpack
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
instance ToMisoString Float where
  toMisoString = toJSString . show
instance ToMisoString Double where
  toMisoString = toJSString . show
instance ToMisoString Int where
  toMisoString = toJSString . show
instance ToMisoString Word where
  toMisoString = toJSString . show
instance FromMisoString MisoString where
  fromMisoStringEither = Right
instance FromMisoString String where
  fromMisoStringEither = Right . fromJSString
instance FromMisoString T.Text where
  fromMisoStringEither = Right . textFromJSString
instance FromMisoString LT.Text where
  fromMisoStringEither = Right . LT.pack . fromMisoString
instance FromMisoString B.ByteString where
  fromMisoStringEither = fmap T.encodeUtf8 . fromMisoStringEither
instance FromMisoString BL.ByteString where
  fromMisoStringEither = fmap LT.encodeUtf8 . fromMisoStringEither
instance FromMisoString Float where
  fromMisoStringEither = parseFloat
instance FromMisoString Double where
  fromMisoStringEither = parseDouble
instance FromMisoString Int where
  fromMisoStringEither = parseInt
instance FromMisoString Word where
  fromMisoStringEither = parseWord
