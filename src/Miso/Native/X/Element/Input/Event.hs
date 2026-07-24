-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Input.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Input.Event
  ( -- *** Events
    onBlur
  , onConfirm
  , onFocus
  , onInput
  , onSelection
    -- *** Types
  , InputEvent (..)
  , SelectionEvent (..)
    -- *** Decoders
  , inputValueDecoder
  , inputDecoder
  , selectionDecoder
    -- *** Event Map
  , inputEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
inputEvents :: Events
inputEvents
  = M.fromList
  [ ("blur", BUBBLE)
  , ("confirm", BUBBLE)
  , ("focus", BUBBLE)
  , ("input", BUBBLE)
  , ("selection", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @bindinput@ event.
data InputEvent
  = InputEvent
  { inputValue :: MisoString
    -- ^ The current input content
  , inputSelectionStart :: Int
    -- ^ Start position of the selection
  , inputSelectionEnd :: Int
    -- ^ End position of the selection
  , inputIsComposing :: Bool
    -- ^ Whether the input is mid-composition (IME)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Payload of the @bindselection@ event.
data SelectionEvent
  = SelectionEvent
  { selStart :: Int
    -- ^ Start position of the selection
  , selEnd :: Int
    -- ^ End position of the selection
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Decodes the @value@ field shared by @bindblur@, @bindconfirm@ and @bindfocus@.
inputValueDecoder :: Decoder MisoString
inputValueDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o -> o .: "value"
-----------------------------------------------------------------------------
inputDecoder :: Decoder InputEvent
inputDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      InputEvent
        <$> o .: "value"
        <*> o .: "selectionStart"
        <*> o .: "selectionEnd"
        <*> o .:? "isComposing" .!= False
-----------------------------------------------------------------------------
-- Note: the JS keys stay @selectionStart@/@selectionEnd@; the record fields are
-- 'selStart'/'selEnd' to avoid clashing with 'InputValue' when the hub module
-- re-exports Event and Method together.
selectionDecoder :: Decoder SelectionEvent
selectionDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      SelectionEvent
        <$> o .: "selectionStart"
        <*> o .: "selectionEnd"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindblur
--
-- Triggered when the input is blurred, outputting the current value.
--
onBlur :: (MisoString -> action) -> Attribute action
onBlur action = on "blur" inputValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindconfirm
--
-- Triggered when the confirm button is clicked, outputting the current value.
--
onConfirm :: (MisoString -> action) -> Attribute action
onConfirm action = on "confirm" inputValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindfocus
--
-- Triggered when the input is focused, outputting the current value.
--
onFocus :: (MisoString -> action) -> Attribute action
onFocus action = on "focus" inputValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindinput
--
-- Triggered when the input content changes.
--
onInput :: (InputEvent -> action) -> Attribute action
onInput action = on "input" inputDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindselection
--
-- Triggered when the input selection changes.
--
onSelection :: (SelectionEvent -> action) -> Attribute action
onSelection action = on "selection" selectionDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
