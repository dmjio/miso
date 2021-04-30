{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Decoder
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Event.Decoder
  ( -- * Decoder
    Decoder (..)
  , DecodeTarget (..)
  , at
  -- * Decoders
  , emptyDecoder
  , keycodeDecoder
  , keyInfoDecoder
  , checkedDecoder
  , valueDecoder
  )
  where

import Data.Aeson.Types
import Control.Applicative
import GHCJS.Marshal (ToJSVal, toJSVal)

import Miso.Event.Types
import Miso.String

-- | Data type representing path (consisting of field names) within event object, where a decoder should be applied.
data DecodeTarget
  = DecodeTarget [MisoString] -- ^ Specify single path within Event object, where a decoder should be applied.
  | DecodeTargets [[MisoString]] -- ^ Specify multiple paths withing Event object, where decoding should be attempted. The first path where decoding suceeds is the one taken.

-- | `ToJSVal` instance for `Decoder`
instance ToJSVal DecodeTarget where
  toJSVal (DecodeTarget xs) = toJSVal xs
  toJSVal (DecodeTargets xs) = toJSVal xs

-- | Decoder data type for parsing events
data Decoder a = Decoder {
  decoder :: Value -> Parser a -- ^ FromJSON-based Event decoder
, decodeAt :: DecodeTarget -- ^ Location in DOM of where to decode
}

-- | Smart constructor for building a `Decoder`.
at :: [MisoString] -> (Value -> Parser a) -> Decoder a
at decodeAt decoder = Decoder {decodeAt = DecodeTarget decodeAt, ..}

-- | Empty decoder for use with events like "click" that do not
-- return any meaningful values
emptyDecoder :: Decoder ()
emptyDecoder = mempty `at` go
  where
    go = withObject "emptyDecoder" $ \_ -> pure ()

-- | Retrieves either "keyCode", "which" or "charCode" field in `Decoder`
keycodeDecoder :: Decoder KeyCode
keycodeDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o ->
       KeyCode <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")

-- | Retrieves either "keyCode", "which" or "charCode" field in `Decoder`, along with shift, ctrl, meta and alt.
keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder =
      withObject "event" $ \o ->
        KeyInfo <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
                <*> o .: "shiftKey"
                <*> o .: "metaKey"
                <*> o .: "ctrlKey"
                <*> o .: "altKey"

-- | Retrieves "value" field in `Decoder`
valueDecoder :: Decoder MisoString
valueDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject "target" $ \o -> o .: "value"

-- | Retrieves "checked" field in Decoder
checkedDecoder :: Decoder Checked
checkedDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject "target" $ \o ->
       Checked <$> (o .: "checked")
