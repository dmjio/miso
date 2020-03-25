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
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String (
    ToMisoString (..)
  , FromMisoString (..)
  , fromMisoString
  , MisoString
  , module Data.JSString
  , module Data.Monoid
  , ms
  ) where

#ifndef JSADDLE
import           Data.Aeson
import           Data.Char

#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.JSString
import           Data.JSString.Text
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Prelude hiding (foldr)
import           Miso.FFI

-- | String type swappable based on compiler
type MisoString = JSString

#ifndef JSADDLE


-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)
#endif

-- | Convenience class for creating `MisoString` from other string-like types
class ToMisoString str where
  toMisoString :: str -> MisoString

class FromMisoString t where
  -- -- | Reads a `MisoString` into an 'a', throws an error when
  fromMisoStringEither :: MisoString -> Either String t

-- | Reads a `MisoString` into an 'a', throws an error when decoding
-- fails. Use `fromMisoStringEither` for as a safe alternative.
fromMisoString :: FromMisoString a => MisoString -> a
fromMisoString s = case fromMisoStringEither s of
                     Left err -> error err
                     Right x  -> x

-- | Convenience function, shorthand for `toMisoString`
ms :: ToMisoString str => str -> MisoString
ms = toMisoString

instance ToMisoString MisoString where
  toMisoString = id
instance ToMisoString String where
  toMisoString = pack
instance ToMisoString T.Text where
  toMisoString = textToJSString
instance ToMisoString LT.Text where
  toMisoString = lazyTextToJSString
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
instance ToMisoString Float where
  toMisoString = realFloatToJSString
instance ToMisoString Double where
  toMisoString = realFloatToJSString
instance ToMisoString Int where
  toMisoString = integralToJSString
instance ToMisoString Word where
  toMisoString = integralToJSString

instance FromMisoString MisoString where
  fromMisoStringEither = Right
instance FromMisoString String where
  fromMisoStringEither = Right . unpack
instance FromMisoString T.Text where
  fromMisoStringEither = Right . textFromJSString
instance FromMisoString LT.Text where
  fromMisoStringEither = Right . lazyTextFromJSString
instance FromMisoString B.ByteString where
  fromMisoStringEither = fmap T.encodeUtf8 . fromMisoStringEither
instance FromMisoString BL.ByteString where
  fromMisoStringEither = fmap LT.encodeUtf8 . fromMisoStringEither
instance FromMisoString Float where
  fromMisoStringEither = fmap realToFrac . jsStringToDoubleEither
instance FromMisoString Double where
  fromMisoStringEither = jsStringToDoubleEither
instance FromMisoString Int where
  fromMisoStringEither = parseInt
instance FromMisoString Word where
  fromMisoStringEither = parseWord

jsStringToDoubleEither :: JSString -> Either String Double
jsStringToDoubleEither s = let d = jsStringToDouble s
                           in if isNaN d then Left "jsStringToDoubleEither: parse failed"
                                         else Right d


parseWord   :: MisoString -> Either String Word
parseWord s = case uncons s of
                Nothing     -> Left "parseWord: parse error"
                Just (c,s') -> foldl' k (pDigit c) s'
  where
    pDigit c | isDigit c = Right . fromIntegral . digitToInt $ c
             | otherwise = Left "parseWord: parse error"
    k ea c = (\a x -> 10*a + x) <$> ea <*> pDigit c

parseInt   :: MisoString -> Either String Int
parseInt s = case uncons s of
               Just ('-',s') -> ((-1)*) . fromIntegral <$> parseWord s'
               _             ->           fromIntegral <$> parseWord s
