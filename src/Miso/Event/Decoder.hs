-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event.Decoder
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Event.Decoder
  ( -- ** Types
    Decoder (..)
  , DecodeTarget (..)
    -- ** Combinators
  , at
    -- ** Decoders
  , emptyDecoder
  , keycodeDecoder
  , keyInfoDecoder
  , checkedDecoder
  , valueDecoder
  , pointerDecoder
  ) where
-----------------------------------------------------------------------------
import Control.Applicative
-----------------------------------------------------------------------------
import Miso.DSL (ToJSVal(toJSVal))
import Miso.Event.Types
import Miso.JSON
import Miso.String
-----------------------------------------------------------------------------
-- | Path (or paths) within the browser event object where a decoder is applied.
data DecodeTarget
  = DecodeTarget [MisoString]
  -- ^ Single dot-separated path into the event object (e.g. @["target", "value"]@).
  | DecodeTargets [[MisoString]]
  -- ^ Multiple candidate paths; the first path where decoding succeeds is used.
-----------------------------------------------------------------------------
-- | `ToJSVal` instance for t'DecodeTarget'.
instance ToJSVal DecodeTarget where
  toJSVal = \case
    DecodeTarget xs -> toJSVal xs
    DecodeTargets xs -> toJSVal xs
-----------------------------------------------------------------------------
-- | t'Decoder' data type for parsing events
data Decoder a
  = Decoder
  { decoder :: Value -> Parser a
    -- ^ FromJSON-based Event decoder
  , decodeAt :: DecodeTarget
    -- ^ Location in DOM of where to decode
  }
-----------------------------------------------------------------------------
-- | Smart constructor for a t'Decoder' that targets a single path.
--
-- @
-- -- Decode the value of an input element:
-- myDecoder :: Decoder MisoString
-- myDecoder = ["target"] \`at\` (withObject "target" $ \o -> o .: "value")
-- @
--
at
  :: [MisoString]
  -- ^ Path into the event object (empty list means the root event object)
  -> (Value -> Parser a)
  -- ^ JSON parser for the value at that path
  -> Decoder a
at decodeAt decoder = Decoder {decodeAt = DecodeTarget decodeAt, ..}
-----------------------------------------------------------------------------
-- | Empty t'Decoder' for use with events like "click" that do not
-- return any meaningful values
emptyDecoder :: Decoder ()
emptyDecoder = mempty `at` go
  where
    go = withObject "emptyDecoder" $ \_ -> pure ()
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder'
keycodeDecoder :: Decoder KeyCode
keycodeDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget mempty
    decoder = withObject "event" $ \o ->
       KeyCode <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
-----------------------------------------------------------------------------
-- | Retrieves either "keyCode", "which" or "charCode" field in t'Decoder',
-- along with shift, ctrl, meta and alt.
keyInfoDecoder :: Decoder KeyInfo
keyInfoDecoder = Decoder {..}
  where
    decodeAt =
      DecodeTarget mempty
    decoder =
      withObject "event" $ \o ->
        KeyInfo
          <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")
          <*> o .: "shiftKey"
          <*> o .: "metaKey"
          <*> o .: "ctrlKey"
          <*> o .: "altKey"
-----------------------------------------------------------------------------
-- | Retrieves "value" field in t'Decoder'
valueDecoder :: Decoder MisoString
valueDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject "target" $ \o -> o .: "value"
-----------------------------------------------------------------------------
-- | Retrieves "checked" field in t'Decoder'
checkedDecoder :: Decoder Checked
checkedDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject "target" $ \o ->
      Checked <$> (o .: "checked")
-----------------------------------------------------------------------------
-- | Pointer t'Decoder' for use with events like "onpointerover"
pointerDecoder :: Decoder PointerEvent
pointerDecoder = Decoder {..}
  where
    pair o x y = liftA2 (,) (o .: x) (o .: y)
    decodeAt = DecodeTarget mempty
    decoder = withObject "pointerDecoder" $ \o ->
      PointerEvent
        <$> o .: "pointerType"
        <*> o .: "pointerId"
        <*> o .: "isPrimary"
        <*> pair o "clientX" "clientY"
        <*> pair o "screenX" "screenY"
        <*> pair o "offsetX" "offsetY"
        <*> pair o "pageX" "pageY"
        <*> pair o "tiltX" "tiltY"
        <*> o .: "pressure"
        <*> o .: "button"
-----------------------------------------------------------------------------
