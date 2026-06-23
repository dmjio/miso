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
--
-- = Overview
--
-- "Miso.Event.Decoder" provides 'Decoder', the type that tells miso how to
-- extract a Haskell value from a browser
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event DOM Event> object.
-- It pairs a target path ('DecodeTarget') into the event with a
-- JSON-style parser ('Miso.JSON.Value' @->@ 'Miso.JSON.Parser' @a@).
--
-- Decoders are consumed by 'Miso.Html.Event.on' from "Miso.Html.Event":
--
-- @
-- on :: 'Miso.String.MisoString'   -- event name (e.g. \"click\")
--    -> 'Decoder' a      -- how to extract @a@ from the event object
--    -> (a -> action)   -- turn the extracted value into an action
--    -> 'Miso.Types.Attribute' action
-- @
--
-- = DecodeTarget
--
-- A 'DecodeTarget' selects the sub-object of the event to decode:
--
-- * @'DecodeTarget' []@ — the event object itself (e.g. for keyboard events).
-- * @'DecodeTarget' [\"target\"]@ — @event.target@ (e.g. for input values).
-- * @'DecodeTargets' [[\"a\"], [\"b\"]]@ — tries @event.a@ first, then
--   @event.b@; the first successful decode wins.
--
-- = Built-in decoders
--
-- ['emptyDecoder'] @()@ — @click@, @submit@, stateless events
-- ['keycodeDecoder'] 'Miso.Event.Types.KeyCode' — @keydown@ \/ @keyup@ key code
-- ['keyInfoDecoder'] 'Miso.Event.Types.KeyInfo' — key code + modifier keys
-- ['valueDecoder'] 'Miso.String.MisoString' — @input@ \/ @change@ (@event.target.value@)
-- ['checkedDecoder'] 'Miso.Event.Types.Checked' — checkbox @change@ (@event.target.checked@)
-- ['pointerDecoder'] 'Miso.Event.Types.PointerEvent' — pointer\/mouse position and metadata
--
-- = Custom decoders
--
-- Build a custom decoder with 'at' or by constructing 'Decoder' directly:
--
-- @
-- -- Extract (offsetX, offsetY) from a click event
-- clickXY :: 'Decoder' (Int, Int)
-- clickXY = 'Decoder'
--   { 'decodeAt' = 'DecodeTarget' []
--   , 'decoder'  = 'Miso.JSON.withObject' \"click\" $ \\o ->
--       (,) \<$\> o 'Miso.JSON..:' \"offsetX\"
--           \<*\> o 'Miso.JSON..:' \"offsetY\"
--   }
-- @
--
-- = See also
--
-- * "Miso.Html.Event" — @on@ and the pre-wired event handlers (@onClick@, @onInput@, …)
-- * "Miso.Event.Types" — payload types ('Miso.Event.Types.KeyCode', 'Miso.Event.Types.PointerEvent', …)
-- * "Miso.JSON" — 'Miso.JSON.Value', 'Miso.JSON.Parser', 'Miso.JSON.withObject', @('Miso.JSON..:')@
-----------------------------------------------------------------------------
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
-- | Data type representing path (consisting of field names) within event object
-- where a decoder should be applied.
data DecodeTarget
  = DecodeTarget [MisoString]
  -- ^ Specify single path within Event object, where a decoder should be applied.
  | DecodeTargets [[MisoString]]
  -- ^ Specify multiple paths withing Event object, where decoding should be attempted. The first path where decoding suceeds is the one taken.
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
-- | Smart constructor for building a t'Decoder'.
at
  :: [MisoString]
  -- ^ Path into the event object (e.g. @[\"target\"]@ for @event.target@, @[]@ for the event itself)
  -> (Value -> Parser a)
  -- ^ JSON-style decoder applied at the given path
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
