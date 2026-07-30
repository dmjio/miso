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
import           Miso.Types (EventHandler, DOMRef)
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
        <*> o .:? "selectionStart" .!= 0
        <*> o .:? "selectionEnd" .!= 0
        -- Lynx's native input sends @isComposing@ as a number (0/1), bridged
        -- from an ObjC @BOOL@ — not a JSON boolean — so decode it as an 'Int'
        -- and coerce. Absent (e.g. on the simulator's non-composing path) is
        -- 'False'. Decoding it as 'Bool' fails the whole decoder on device.
        <*> (maybe False (/= (0 :: Int)) <$> o .:? "isComposing")
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
onBlur :: (MisoString -> action) -> EventHandler action
onBlur action = on "blur" inputValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindconfirm
--
-- Triggered when the confirm button is clicked, outputting the current value.
--
onConfirm :: (MisoString -> action) -> EventHandler action
onConfirm action = on "confirm" inputValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindfocus
--
-- Triggered when the input is focused, outputting the current value.
--
onFocus :: (MisoString -> action) -> EventHandler action
onFocus action = on "focus" inputValueDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindinput
--
-- Triggered when the input content changes.
--
onInput :: (InputEvent -> action) -> EventHandler action
onInput action = on "input" inputDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindselection
--
-- Triggered when the input selection changes.
--
onSelection :: (SelectionEvent -> action) -> EventHandler action
onSelection action = on "selection" selectionDecoder (\e _ -> action e)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onBlur', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onBlurWith :: (MisoString -> DOMRef -> action) -> EventHandler action
onBlurWith action = on "blur" inputValueDecoder action
-----------------------------------------------------------------------------
-- | Like 'onConfirm', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onConfirmWith :: (MisoString -> DOMRef -> action) -> EventHandler action
onConfirmWith action = on "confirm" inputValueDecoder action
-----------------------------------------------------------------------------
-- | Like 'onFocus', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onFocusWith :: (MisoString -> DOMRef -> action) -> EventHandler action
onFocusWith action = on "focus" inputValueDecoder action
-----------------------------------------------------------------------------
-- | Like 'onInput', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onInputWith :: (InputEvent -> DOMRef -> action) -> EventHandler action
onInputWith action = on "input" inputDecoder action
-----------------------------------------------------------------------------
-- | Like 'onSelection', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onSelectionWith :: (SelectionEvent -> DOMRef -> action) -> EventHandler action
onSelectionWith action = on "selection" selectionDecoder action
-----------------------------------------------------------------------------
