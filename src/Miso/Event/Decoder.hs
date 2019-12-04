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
  , checkedDecoder
  , valueDecoder
  )
  where

import Data.Aeson.Types
import Control.Applicative

import Miso.Event.Types
import Miso.String

-- | Data type representing path (consisting of field names) within event object, where a decoder should be applied.
data DecodeTarget
  = DecodeTarget [MisoString] -- ^ Specify single path within Event object, where a decoder should be applied.
  | DecodeTargets [[MisoString]] -- ^ Specify multiple paths withing Event object, where decoding should be attempted. The first path where decoding suceeds is the one taken.

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
