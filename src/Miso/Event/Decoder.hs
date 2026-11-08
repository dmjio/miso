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
-- "Miso.Event.Decoder" provides t'Decoder', the type that tells miso how to
-- extract a Haskell value from a browser
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event DOM Event> object.
-- It pairs a target path ('DecodeTarget') into the event with a
-- JSON-style parser ('Miso.JSON.Value' @->@ 'Miso.JSON.Parser' @a@).
--
-- Decoders are consumed by 'Miso.Html.Event.on' from "Miso.Html.Event":
--
-- @
-- on :: 'Miso.String.MisoString'   -- event name (e.g. \"click\")
--    -> t'Decoder' a      -- how to extract @a@ from the event object
--    -> (a -> action)   -- turn the extracted value into an action
--    -> 'Miso.Types.Attribute' action
-- @
--
-- = DecodeTarget
--
-- A t'DecodeTarget' selects the sub-object of the event to decode. It is a
-- path of t'Segment's, each either a property read or a method call:
--
-- * @'DecodeTarget' []@ — the event object itself (e.g. for keyboard events).
-- * @'DecodeTarget' [\"target\"]@ — @event.target@ (e.g. for input values).
-- * @'DecodeTarget' [\"clipboardData\", 'Call' \"getData\" [\"text\/plain\"]]@ —
--   @event.clipboardData.getData(\"text\/plain\")@.
-- * @'DecodeTarget' [\"currentTarget\", 'Call' \"getBoundingClientRect\" []]@ —
--   the bounding rect of the element the handler is attached to.
-- * @'DecodeTargets' [[\"a\"], [\"b\"]]@ — decodes @event.a@ and @event.b@ and
--   hands the parser a JSON array of both results. This is what the
--   'Applicative' instance uses to combine decoders.
--
-- The path is walked while the browser is still dispatching the event, so
-- methods see live state: a @DataTransfer@'s data, the current selection,
-- the element's layout.
--
-- A path that reads @null@ or @undefined@, or names a method that does not
-- exist, decodes as @null@; use 'Miso.JSON..:?' or 'Control.Applicative.<|>'
-- in the parser to handle that.
--
-- __@currentTarget@__: miso delegates events from the mount point, so the
-- event's own @currentTarget@ is useless. A path starting with
-- @\"currentTarget\"@ is instead rooted at the element the handler is on,
-- which is what @currentTarget@ means for a directly-attached listener.
--
-- = Built-in decoders
--
-- ['emptyDecoder'] @()@ — @click@, @submit@, stateless events
-- ['keycodeDecoder'] 'Miso.Event.Types.KeyCode' — @keydown@ \/ @keyup@ key code
-- ['keyInfoDecoder'] 'Miso.Event.Types.KeyInfo' — key code + modifier keys
-- ['valueDecoder'] 'Miso.String.MisoString' — @input@ \/ @change@ (@event.target.value@)
-- ['checkedDecoder'] 'Miso.Event.Types.Checked' — checkbox @change@ (@event.target.checked@)
-- ['pointerDecoder'] 'Miso.Event.Types.PointerEvent' — pointer\/mouse position and metadata
-- ['boundingClientRectDecoder'] 'Miso.Event.Types.DOMRect' — @currentTarget.getBoundingClientRect()@
-- ['clipboardTextDecoder'] 'Miso.String.MisoString' — @paste@ (@event.clipboardData.getData(\"text\/plain\")@)
-- ['dataTransferDecoder'] 'Miso.String.MisoString' — @drop@ (@event.dataTransfer.getData(format)@)
-- ['modifierStateDecoder'] 'Bool' — @event.getModifierState(key)@
--
-- = Custom decoders
--
-- Build a custom decoder with 'at' or by constructing t'Decoder' directly:
--
-- @
-- -- Extract (offsetX, offsetY) from a click event
-- clickXY :: t'Decoder' (Int, Int)
-- clickXY = t'Decoder'
--   { 'decodeAt' = t'DecodeTarget' []
--   , 'decoder'  = 'Miso.JSON.withObject' \"click\" $ \\o ->
--       (,) \<$\> o 'Miso.JSON..:' \"offsetX\"
--           \<*\> o 'Miso.JSON..:' \"offsetY\"
--   }
-- @
--
-- Call methods with 'Call':
--
-- @
-- -- The text being pasted (only readable while the event is dispatching)
-- pasted :: t'Decoder' 'Miso.String.MisoString'
-- pasted = 'at' [\"clipboardData\", 'Call' \"getData\" [\"text\/plain\"]] 'parseJSON'
--
-- -- The pointer's path through the (shadow) DOM, as tag names
-- path :: t'Decoder' ['Miso.String.MisoString']
-- path = 'at' ['Call' \"composedPath\" []] $
--   'Miso.JSON.withArray' \"path\" ('traverse' ('Miso.JSON.withObject' \"node\" ('Miso.JSON..:' \"tagName\")))
--
-- -- Whether the click landed inside a @.card@ (closest() is null otherwise)
-- inCard :: t'Decoder' Bool
-- inCard = 'at' [\"target\", 'Call' \"closest\" [\".card\"]] $ \\case
--   'Miso.JSON.Null' -> pure False
--   _    -> pure True
--
-- -- Coalesced pointer positions for smooth drawing
-- strokes :: t'Decoder' [(Double, Double)]
-- strokes = 'at' ['Call' \"getCoalescedEvents\" []] $
--   'Miso.JSON.withArray' \"events\" $ 'traverse' $ 'Miso.JSON.withObject' \"event\" $ \\o ->
--     (,) \<$\> o 'Miso.JSON..:' \"offsetX\" \<*\> o 'Miso.JSON..:' \"offsetY\"
--
-- -- Current text selection, as a string
-- selection :: t'Decoder' 'Miso.String.MisoString'
-- selection = 'at' [\"target\", \"ownerDocument\", 'Call' \"getSelection\" [], 'Call' \"toString\" []] 'parseJSON'
-- @
--
-- Combine decoders with the 'Applicative' instance; each is decoded at its
-- own path:
--
-- @
-- -- Click position relative to the element the handler is on
-- clickInElement :: t'Decoder' (Double, Double)
-- clickInElement = relative \<$\> 'pointerDecoder' \<*\> 'boundingClientRectDecoder'
--   where
--     relative p r = (fst ('Miso.Event.Types.client' p) - 'Miso.Event.Types.rectLeft' r, snd ('Miso.Event.Types.client' p) - 'Miso.Event.Types.rectTop' r)
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
  , Segment (..)
    -- ** Combinators
  , at
    -- ** Decoders
  , emptyDecoder
  , keycodeDecoder
  , keyInfoDecoder
  , checkedDecoder
  , valueDecoder
  , pointerDecoder
  , boundingClientRectDecoder
  , clipboardTextDecoder
  , dataTransferDecoder
  , modifierStateDecoder
  ) where
-----------------------------------------------------------------------------
import Control.Applicative
import Data.String (IsString(..))
-----------------------------------------------------------------------------
import Miso.DSL (ToJSVal(toJSVal))
import Miso.Event.Types
import Miso.JSON
import Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | One step of a t'DecodeTarget' path.
--
-- String literals are 'Prop's, so @[\"target\", \"value\"]@ still works.
data Segment
  = Prop MisoString
  -- ^ Read a property: @obj[name]@
  | Call MisoString [Value]
  -- ^ Call a method with the given arguments: @obj[name](args...)@.
  -- A missing method decodes as @null@.
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance IsString Segment where
  fromString = Prop . fromString
-----------------------------------------------------------------------------
-- | A 'Prop' is sent as its name, a 'Call' as @{call, args}@.
instance ToJSVal Segment where
  toJSVal = \case
    Prop s -> toJSVal s
    Call f args -> toJSVal (object [ "call" .= f, "args" .= args ])
-----------------------------------------------------------------------------
-- | Data type representing path (consisting of property reads and method
-- calls) within event object where a decoder should be applied.
data DecodeTarget
  = DecodeTarget [Segment]
  -- ^ Specify single path within Event object, where a decoder should be applied.
  | DecodeTargets [[Segment]]
  -- ^ Specify multiple paths within Event object. Each is decoded, and the
  -- parser receives a JSON array holding one result per path, in order.
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
instance Functor Decoder where
  fmap f (Decoder d t) = Decoder (fmap f . d) t
-----------------------------------------------------------------------------
-- | Decoders combine by decoding each at its own path: the runtime returns
-- one value per path ('DecodeTargets'), and each parser is run on its own.
--
-- @
-- (,) \<$\> 'pointerDecoder' \<*\> 'boundingClientRectDecoder'
-- @
instance Applicative Decoder where
  pure x = Decoder (const (pure x)) (DecodeTargets [])
  Decoder df tf <*> Decoder dx tx = Decoder go (DecodeTargets (pf <> px))
    where
      pf = paths tf
      px = paths tx
      go
        | null pf && null px = \_ -> df (Array []) <*> dx (Array [])
        | otherwise = withArray "Decoder" $ \vs -> do
            let (vf, vx) = splitAt (length pf) vs
            df (rewrap tf vf) <*> dx (rewrap tx vx)
      paths (DecodeTarget p)   = [p]
      paths (DecodeTargets ps) = ps
      -- A parser built for a single DecodeTarget expects the bare value,
      -- not a one-element array.
      rewrap (DecodeTarget _) [v] = v
      rewrap _ vs                 = Array vs
-----------------------------------------------------------------------------
-- | Smart constructor for building a t'Decoder'.
at
  :: [Segment]
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
-- | @currentTarget.getBoundingClientRect()@: the layout box of the element
-- the handler is attached to, measured during dispatch.
boundingClientRectDecoder :: Decoder DOMRect
boundingClientRectDecoder =
  at ["currentTarget", Call "getBoundingClientRect" []] parseJSON
-----------------------------------------------------------------------------
-- | @event.clipboardData.getData(\"text\/plain\")@, for @paste@ events.
--
-- The clipboard is only readable while the event is dispatching, which is
-- why this has to be a t'Decoder' and not something done in @update@.
clipboardTextDecoder :: Decoder MisoString
clipboardTextDecoder =
  at ["clipboardData", Call "getData" ["text/plain"]] parseJSON
-----------------------------------------------------------------------------
-- | @event.dataTransfer.getData(format)@, for @drop@ events, e.g.
-- @dataTransferDecoder \"text\/uri-list\"@ or a custom type set in @dragstart@.
dataTransferDecoder :: MisoString -> Decoder MisoString
dataTransferDecoder format =
  at ["dataTransfer", Call "getData" [String format]] parseJSON
-----------------------------------------------------------------------------
-- | @event.getModifierState(key)@ for keyboard and mouse events, e.g.
-- @modifierStateDecoder \"CapsLock\"@.
modifierStateDecoder :: MisoString -> Decoder Bool
modifierStateDecoder key =
  at [Call "getModifierState" [String key]] parseJSON
