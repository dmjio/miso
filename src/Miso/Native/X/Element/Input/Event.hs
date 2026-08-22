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
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Input.Event
  ( -- *** Events
    onBlur
  , onBlurWith
  , onBlurMain
  , onBlurMainWith
  , onConfirm
  , onConfirmWith
  , onConfirmMain
  , onConfirmMainWith
  , onFocus
  , onFocusWith
  , onFocusMain
  , onFocusMainWith
  , onInput
  , onInputWith
  , onInputMain
  , onInputMainWith
  , onSelection
  , onSelectionWith
  , onSelectionMain
  , onSelectionMainWith
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
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
-- | The 'Events' map for the Lynx @<input>@ element.
--
-- Combine with other element maps using @<>@ and pass the result to
-- 'Miso.Native.native', so the delegator listens for these events.
--
-- @since 1.13.0.0
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
-- | t'Decoder' producing a t'InputEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
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
        -- @False@. Decoding it as 'Bool' fails the whole decoder on device.
        <*> (maybe False (/= (0 :: Int)) <$> o .:? "isComposing")
-----------------------------------------------------------------------------
-- Note: the JS keys stay @selectionStart@/@selectionEnd@; the record fields are
-- 'selStart'/'selEnd' to avoid clashing with 'InputValue' when the hub module
-- re-exports Event and Method together.
-- | t'Decoder' producing a t'SelectionEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
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
onBlur :: (MisoString -> action) -> Attribute model action
onBlur action = on "blur" inputValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onBlur', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Blurred MisoString
--
-- view_ [ event (static (onBlurMain Blurred)) ] [ "some view" ]
-- @
--
onBlurMain :: (MisoString -> action) -> EventHandler model action
onBlurMain action = onMain "blur" inputValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onBlurMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Blurred MisoString Model DOMRef
--
-- view_ [ event (static (onBlurMainWith Blurred)) ] [ "some view" ]
-- @
--
onBlurMainWith :: (MisoString -> model -> DOMRef -> action) -> EventHandler model action
onBlurMainWith action = onMain "blur" inputValueDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindconfirm
--
-- Triggered when the confirm button is clicked, outputting the current value.
--
onConfirm :: (MisoString -> action) -> Attribute model action
onConfirm action = on "confirm" inputValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onConfirm', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Confirmed MisoString
--
-- view_ [ event (static (onConfirmMain Confirmed)) ] [ "some view" ]
-- @
--
onConfirmMain :: (MisoString -> action) -> EventHandler model action
onConfirmMain action = onMain "confirm" inputValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onConfirmMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Confirmed MisoString Model DOMRef
--
-- view_ [ event (static (onConfirmMainWith Confirmed)) ] [ "some view" ]
-- @
--
onConfirmMainWith :: (MisoString -> model -> DOMRef -> action) -> EventHandler model action
onConfirmMainWith action = onMain "confirm" inputValueDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindfocus
--
-- Triggered when the input is focused, outputting the current value.
--
onFocus :: (MisoString -> action) -> Attribute model action
onFocus action = on "focus" inputValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onFocus', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Focused MisoString
--
-- view_ [ event (static (onFocusMain Focused)) ] [ "some view" ]
-- @
--
onFocusMain :: (MisoString -> action) -> EventHandler model action
onFocusMain action = onMain "focus" inputValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onFocusMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Focused MisoString Model DOMRef
--
-- view_ [ event (static (onFocusMainWith Focused)) ] [ "some view" ]
-- @
--
onFocusMainWith :: (MisoString -> model -> DOMRef -> action) -> EventHandler model action
onFocusMainWith action = onMain "focus" inputValueDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindinput
--
-- Triggered when the input content changes.
--
onInput :: (InputEvent -> action) -> Attribute model action
onInput action = on "input" inputDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onInput', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Changed InputEvent
--
-- view_ [ event (static (onInputMain Changed)) ] [ "some view" ]
-- @
--
onInputMain :: (InputEvent -> action) -> EventHandler model action
onInputMain action = onMain "input" inputDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onInputMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Changed InputEvent Model DOMRef
--
-- view_ [ event (static (onInputMainWith Changed)) ] [ "some view" ]
-- @
--
onInputMainWith :: (InputEvent -> model -> DOMRef -> action) -> EventHandler model action
onInputMainWith action = onMain "input" inputDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#bindselection
--
-- Triggered when the input selection changes.
--
onSelection :: (SelectionEvent -> action) -> Attribute model action
onSelection action = on "selection" selectionDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onSelection', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Selected SelectionEvent
--
-- view_ [ event (static (onSelectionMain Selected)) ] [ "some view" ]
-- @
--
onSelectionMain :: (SelectionEvent -> action) -> EventHandler model action
onSelectionMain action = onMain "selection" selectionDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onSelectionMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Selected SelectionEvent Model DOMRef
--
-- view_ [ event (static (onSelectionMainWith Selected)) ] [ "some view" ]
-- @
--
onSelectionMainWith :: (SelectionEvent -> model -> DOMRef -> action) -> EventHandler model action
onSelectionMainWith action = onMain "selection" selectionDecoder action
-----------------------------------------------------------------------------
-- | Like 'onBlur', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onBlurWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onBlurWith action = on "blur" inputValueDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onConfirm', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onConfirmWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onConfirmWith action = on "confirm" inputValueDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onFocus', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onFocusWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onFocusWith action = on "focus" inputValueDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onInput', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onInputWith :: (InputEvent -> DOMRef -> action) -> Attribute model action
onInputWith action = on "input" inputDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onSelection', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onSelectionWith :: (SelectionEvent -> DOMRef -> action) -> Attribute model action
onSelectionWith action = on "selection" selectionDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
