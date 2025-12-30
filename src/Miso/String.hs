-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
#ifdef VANILLA
  , module Data.Text
#else
  , module Data.JSString
#endif
  , ms
  ) where
----------------------------------------------------------------------------
import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
#ifdef VANILLA
import           Data.Text hiding (show, elem)
#else
import           Data.JSString
#ifdef GHCJS_BOTH
import           Data.JSString.Text
#endif
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
----------------------------------------------------------------------------
#ifdef VANILLA
type MisoString = Text
#else
type MisoString = JSString
#endif
----------------------------------------------------------------------------
-- | Convenience class for creating `MisoString` from other string-like types
class ToMisoString str where
  -- | Convert a type into 'MisoString'
  toMisoString :: str -> MisoString
----------------------------------------------------------------------------
-- | Class used to parse a 'MisoString'. Like a safe 'Read' for 'MisoString'
class FromMisoString t where
  fromMisoStringEither :: MisoString -> Either String t
----------------------------------------------------------------------------
-- | Reads a 'MisoString', throws an error when decoding
-- fails. Use `fromMisoStringEither` as a safe alternative.
fromMisoString :: FromMisoString a => MisoString -> a
fromMisoString s =
  case fromMisoStringEither s of
    Left err -> error err
    Right x  -> x
----------------------------------------------------------------------------
-- | Convenience function, shorthand for `toMisoString`
ms :: ToMisoString str => str -> MisoString
ms = toMisoString
----------------------------------------------------------------------------
instance ToMisoString a => ToMisoString (Maybe a) where
  toMisoString = \case
    Nothing -> mempty
    Just x -> ms x
----------------------------------------------------------------------------
instance ToMisoString Char where
  toMisoString = singleton
----------------------------------------------------------------------------
instance ToMisoString IOException where
  toMisoString = ms . show
----------------------------------------------------------------------------
#ifndef VANILLA
instance ToMisoString MisoString where
  toMisoString = id
#endif
----------------------------------------------------------------------------
instance ToMisoString SomeException where
  toMisoString = ms . show
----------------------------------------------------------------------------
instance ToMisoString String where
  toMisoString = pack
----------------------------------------------------------------------------
instance ToMisoString LT.Text where
  toMisoString = ms . LT.toStrict
----------------------------------------------------------------------------
instance ToMisoString T.Text where
#ifdef VANILLA
  toMisoString = id
#else
  toMisoString = textToJSString
#endif
----------------------------------------------------------------------------
instance ToMisoString B.ByteString where
  toMisoString = ms . T.decodeUtf8
----------------------------------------------------------------------------
instance ToMisoString BL.ByteString where
  toMisoString = ms . LT.decodeUtf8
----------------------------------------------------------------------------
instance ToMisoString B.Builder where
  toMisoString = ms . B.toLazyByteString
----------------------------------------------------------------------------
instance ToMisoString Float where
  toMisoString = ms . show
----------------------------------------------------------------------------
instance ToMisoString Double where
  toMisoString = ms . show
----------------------------------------------------------------------------
instance ToMisoString Int where
  toMisoString = ms . show
----------------------------------------------------------------------------
instance ToMisoString Word where
  toMisoString = ms . show
----------------------------------------------------------------------------
#ifndef VANILLA
instance FromMisoString MisoString where
  fromMisoStringEither = Right
#endif
----------------------------------------------------------------------------
instance FromMisoString T.Text where
#ifdef VANILLA
  fromMisoStringEither = Right
#else
  fromMisoStringEither = Right . textFromJSString
#endif
----------------------------------------------------------------------------
instance FromMisoString String where
  fromMisoStringEither = Right . unpack
----------------------------------------------------------------------------
instance FromMisoString LT.Text where
#ifdef VANILLA
  fromMisoStringEither = Right . LT.fromStrict
#else
  fromMisoStringEither = Right . LT.fromStrict . textFromJSString
#endif
----------------------------------------------------------------------------
instance FromMisoString B.ByteString where
  fromMisoStringEither = fmap T.encodeUtf8 . fromMisoStringEither
----------------------------------------------------------------------------
instance FromMisoString BL.ByteString where
  fromMisoStringEither = fmap LT.encodeUtf8 . fromMisoStringEither
----------------------------------------------------------------------------
instance FromMisoString B.Builder where
  fromMisoStringEither = fmap B.byteString . fromMisoStringEither
----------------------------------------------------------------------------
