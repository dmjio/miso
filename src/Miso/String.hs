-----------------------------------------------------------------------------
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleInstances    #-}
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
  , module Data.Monoid
#ifdef VANILLA
  , module Data.Text
#endif
  , ms
  ) where
----------------------------------------------------------------------------
#ifndef VANILLA
import           Data.Aeson
#endif
import           Data.Text hiding (show)
import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
----------------------------------------------------------------------------
import           Miso.DSL.JSString
----------------------------------------------------------------------------
-- | String type swappable based on compiler
type MisoString = JSString
----------------------------------------------------------------------------
#ifndef VANILLA
instance ToJSONKey MisoString
instance FromJSONKey MisoString
----------------------------------------------------------------------------
-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString
----------------------------------------------------------------------------
-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)
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
    Just x -> toMisoString x
----------------------------------------------------------------------------
instance ToMisoString Char where
  toMisoString = T.singleton
----------------------------------------------------------------------------
instance ToMisoString IOException where
  toMisoString = undefined
----------------------------------------------------------------------------
instance ToMisoString MisoString where
  toMisoString = id
----------------------------------------------------------------------------
instance ToMisoString SomeException where
  toMisoString = toMisoString . show
----------------------------------------------------------------------------
instance ToMisoString String where
  toMisoString = T.pack
----------------------------------------------------------------------------
instance ToMisoString LT.Text where
  toMisoString = LT.toStrict
----------------------------------------------------------------------------
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
----------------------------------------------------------------------------
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
----------------------------------------------------------------------------
instance ToMisoString B.Builder where
  toMisoString = toMisoString . B.toLazyByteString
----------------------------------------------------------------------------
instance ToMisoString Float where
  toMisoString = T.pack . show
----------------------------------------------------------------------------
instance ToMisoString Double where
  toMisoString = T.pack . show
----------------------------------------------------------------------------
instance ToMisoString Int where
  toMisoString = T.pack . show
----------------------------------------------------------------------------
instance ToMisoString Word where
  toMisoString = T.pack . show
----------------------------------------------------------------------------
instance FromMisoString MisoString where
  fromMisoStringEither = Right
----------------------------------------------------------------------------
instance FromMisoString String where
  fromMisoStringEither = Right . T.unpack
----------------------------------------------------------------------------
instance FromMisoString LT.Text where
  fromMisoStringEither = Right . LT.fromStrict
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
instance FromMisoString Float where
  fromMisoStringEither = fmap realToFrac . jsStringToDoubleEither
----------------------------------------------------------------------------
instance FromMisoString Double where
  fromMisoStringEither = jsStringToDoubleEither
----------------------------------------------------------------------------
instance FromMisoString Int where
  fromMisoStringEither = parseInt
----------------------------------------------------------------------------
instance FromMisoString Word where
  fromMisoStringEither = parseWord
----------------------------------------------------------------------------
jsStringToDoubleEither :: JSString -> Either String Double
jsStringToDoubleEither s = let d = read $ T.unpack s
                           in if isNaN d then Left "jsStringToDoubleEither: parse failed"
                                         else Right d
----------------------------------------------------------------------------
parseWord :: MisoString -> Either String Word
parseWord s = case T.uncons s of
                Nothing     -> Left "parseWord: parse error"
                Just (c, s') -> T.foldl' k (pDigit c) s'
  where
    pDigit c | isDigit c = Right . fromIntegral . digitToInt $ c
             | otherwise = Left "parseWord: parse error"
    k ea c = (\a x -> 10 * a + x) <$> ea <*> pDigit c
----------------------------------------------------------------------------
parseInt   :: MisoString -> Either String Int
parseInt s = case T.uncons s of
               Just ('-',s') -> ((-1)*) . fromIntegral <$> parseWord s'
               _             ->           fromIntegral <$> parseWord s
----------------------------------------------------------------------------
