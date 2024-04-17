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

import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Char
import           Data.JSString
import qualified Data.JSString as JS
import           Data.JSString.Text
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Prelude                 hiding (foldr)
import           Text.StringLike         (StringLike(..))

-- | String type swappable based on compiler
type MisoString = JS.JSString

#ifdef ghcjs_HOST_OS
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

-- | Class used to parse a 'MisoString'. Like a safe 'Read' for 'MisoString'
class FromMisoString t where
  -- -- | Reads a `MisoString`
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
  toMisoString = JS.pack
instance ToMisoString T.Text where
  toMisoString = textToJSString
instance ToMisoString LT.Text where
  toMisoString = lazyTextToJSString
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
instance ToMisoString Float where
  toMisoString = JS.pack . show
instance ToMisoString Double where
  toMisoString = JS.pack . show
instance ToMisoString Int where
  toMisoString = JS.pack . show
instance ToMisoString Word where
  toMisoString = JS.pack . show

instance FromMisoString MisoString where
  fromMisoStringEither = Right
instance FromMisoString String where
  fromMisoStringEither = Right . JS.unpack
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

jsStringToDoubleEither :: JS.JSString -> Either String Double
jsStringToDoubleEither s = let d = read $ JS.unpack s
                           in if isNaN d then Left "jsStringToDoubleEither: parse failed"
                                         else Right d


parseWord   :: MisoString -> Either String Word
parseWord s = case JS.uncons s of
                Nothing     -> Left "parseWord: parse error"
                Just (c,s') -> JS.foldl' k (pDigit c) s'
  where
    pDigit c | isDigit c = Right . fromIntegral . digitToInt $ c
             | otherwise = Left "parseWord: parse error"
    k ea c = (\a x -> 10*a + x) <$> ea <*> pDigit c

parseInt   :: MisoString -> Either String Int
parseInt s = case JS.uncons s of
               Just ('-',s') -> ((-1)*) . fromIntegral <$> parseWord s'
               _             ->           fromIntegral <$> parseWord s

instance StringLike MisoString where
  uncons = JS.uncons
  toString = JS.unpack
  fromChar = JS.singleton
  strConcat = JS.concat
  empty = JS.empty
  strNull = JS.null
  cons = JS.cons
  append = JS.append
  strMap = JS.map
