-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Textarea.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Textarea.Event
  ( -- *** Events
    onBlur
  , onBlurWith
  , onConfirm
  , onConfirmWith
  , onFocus
  , onFocusWith
  , onInput
  , onInputWith
  , onSelection
  , onSelectionWith
    -- *** Types
  , TextareaEvent (..)
  , SelectionEvent (..)
    -- *** Decoders
  , textareaValueDecoder
  , textareaDecoder
  , selectionDecoder
    -- *** Event Map
  , textareaEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (EventHandler, DOMRef)
-----------------------------------------------------------------------------
textareaEvents :: Events
textareaEvents
  = backgroundEvents
  [ ("blur", BUBBLE)
  , ("confirm", BUBBLE)
  , ("focus", BUBBLE)
  , ("input", BUBBLE)
  , ("selection", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @bindinput@ event.
data TextareaEvent
  = TextareaEvent
  { textareaValue :: MisoString
    -- ^ The current input content
  , textareaSelectionStart :: Int
    -- ^ Start position of the selection
  , textareaSelectionEnd :: Int
    -- ^ End position of the selection
  , textareaIsComposing :: Bool
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
textareaValueDecoder :: Decoder MisoString
textareaValueDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o -> o .: "value"
-----------------------------------------------------------------------------
textareaDecoder :: Decoder TextareaEvent
textareaDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      TextareaEvent
        <$> o .: "value"
        <*> o .: "selectionStart"
        <*> o .: "selectionEnd"
        <*> o .:? "isComposing" .!= False
-----------------------------------------------------------------------------
-- Note: the JS keys stay @selectionStart@/@selectionEnd@; the record fields are
-- 'selStart'/'selEnd' to avoid clashing with 'TextareaValue' when the hub module
-- re-exports Event and Method together.
selectionDecoder :: Decoder SelectionEvent
selectionDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      SelectionEvent
        <$> o .: "selectionStart"
        <*> o .: "selectionEnd"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindblur
--
-- Triggered when the textarea is blurred, outputting the current value.
--
onBlur :: (MisoString -> action) -> EventHandler action
onBlur action = on "blur" textareaValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindconfirm
--
-- Triggered when the confirm button is clicked (only when @confirm-type@ is
-- defined), outputting the current value.
--
onConfirm :: (MisoString -> action) -> EventHandler action
onConfirm action = on "confirm" textareaValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindfocus
--
-- Triggered when the textarea is focused, outputting the current value.
--
onFocus :: (MisoString -> action) -> EventHandler action
onFocus action = on "focus" textareaValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindinput
--
-- Triggered when the textarea content changes.
--
onInput :: (TextareaEvent -> action) -> EventHandler action
onInput action = on "input" textareaDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindselection
--
-- Triggered when the textarea selection changes.
--
onSelection :: (SelectionEvent -> action) -> EventHandler action
onSelection action = on "selection" selectionDecoder (\e _ -> action e)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onBlur', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onBlurWith :: (MisoString -> DOMRef -> action) -> EventHandler action
onBlurWith action = on "blur" textareaValueDecoder action
-----------------------------------------------------------------------------
-- | Like 'onConfirm', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onConfirmWith :: (MisoString -> DOMRef -> action) -> EventHandler action
onConfirmWith action = on "confirm" textareaValueDecoder action
-----------------------------------------------------------------------------
-- | Like 'onFocus', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onFocusWith :: (MisoString -> DOMRef -> action) -> EventHandler action
onFocusWith action = on "focus" textareaValueDecoder action
-----------------------------------------------------------------------------
-- | Like 'onInput', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onInputWith :: (TextareaEvent -> DOMRef -> action) -> EventHandler action
onInputWith action = on "input" textareaDecoder action
-----------------------------------------------------------------------------
-- | Like 'onSelection', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onSelectionWith :: (SelectionEvent -> DOMRef -> action) -> EventHandler action
onSelectionWith action = on "selection" selectionDecoder action
-----------------------------------------------------------------------------
