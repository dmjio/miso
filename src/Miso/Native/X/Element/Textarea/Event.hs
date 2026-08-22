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
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Textarea.Event
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
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
textareaEvents :: Events
textareaEvents = M.fromList
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
        <*> o .:? "selectionStart" .!= 0
        <*> o .:? "selectionEnd" .!= 0
        -- See 'Miso.Native.X.Element.Input.Event': Lynx sends @isComposing@ as
        -- a number (0/1), not a JSON boolean, so decode as 'Int' and coerce.
        <*> (maybe False (/= (0 :: Int)) <$> o .:? "isComposing")
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
onBlur :: (MisoString -> action) -> Attribute model action
onBlur action = on "blur" textareaValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindblur
--
-- Triggered when the textarea is blurred, outputting the current value.
--
-- Called on main thread, provides read-only access to model.
-- Meant to be used with '-XStaticPointers'.
--
-- data Action = CurrentValue MisoString
--
-- @
-- view_ [ event (static (onBlurMain CurrentValue)) ] [ "some view" ]
-- @
--
onBlurMain :: (MisoString -> action) -> EventHandler model action
onBlurMain action = onMain "blur" textareaValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindblur
--
-- Triggered when the textarea is blurred, outputting the current value.
--
-- Called on main thread, provides read-only access to model.
-- Meant to be used with '-XStaticPointers'.
--
-- @
--
-- data Action = CurrentValue MisoString Model DOMRef
--
-- view_ [ event (static (onBlurMain CurrentValue)) ] [ "some view" ]
--
-- @
--
onBlurMainWith :: (MisoString -> model -> DOMRef -> action) -> EventHandler model action
onBlurMainWith action = onMain "blur" textareaValueDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindconfirm
--
-- Triggered when the confirm button is clicked (only when @confirm-type@ is
-- defined), outputting the current value.
--
onConfirm :: (MisoString -> action) -> Attribute model action
onConfirm action = on "confirm" textareaValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindconfirm
--
-- Triggered when the confirm button is clicked (only when @confirm-type@ is
-- defined), outputting the current value.
--
onConfirmMain :: (MisoString -> action) -> EventHandler model action
onConfirmMain action = onMain "confirm" textareaValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindconfirm
--
-- Triggered when the confirm button is clicked (only when @confirm-type@ is
-- defined), outputting the current value.
--
onConfirmMainWith :: (MisoString -> model -> DOMRef -> action) -> EventHandler model action
onConfirmMainWith action = onMain "confirm" textareaValueDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindfocus
--
-- Triggered when the textarea is focused, outputting the current value.
--
onFocus :: (MisoString -> action) -> Attribute model action
onFocus action = on "focus" textareaValueDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onFocus', but dispatched on the Lynx __main thread__ ('MTS').
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
onFocusMain action = onMain "focus" textareaValueDecoder (\e _ _ -> action e)
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
onFocusMainWith action = onMain "focus" textareaValueDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindinput
--
-- Triggered when the textarea content changes.
--
onInput :: (TextareaEvent -> action) -> Attribute model action
onInput action = on "input" textareaDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onInput', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Changed TextareaEvent
--
-- view_ [ event (static (onInputMain Changed)) ] [ "some view" ]
-- @
--
onInputMain :: (TextareaEvent -> action) -> EventHandler model action
onInputMain action = onMain "input" textareaDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onInputMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Changed TextareaEvent Model DOMRef
--
-- view_ [ event (static (onInputMainWith Changed)) ] [ "some view" ]
-- @
--
onInputMainWith :: (TextareaEvent -> model -> DOMRef -> action) -> EventHandler model action
onInputMainWith action = onMain "input" textareaDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bindselection
--
-- Triggered when the textarea selection changes.
--
onSelection :: (SelectionEvent -> action) -> Attribute model action
onSelection action = on "selection" selectionDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onSelection', but dispatched on the Lynx __main thread__ ('MTS').
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
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onBlurWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onBlurWith action = on "blur" textareaValueDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onConfirm', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onConfirmWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onConfirmWith action = on "confirm" textareaValueDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onFocus', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onFocusWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onFocusWith action = on "focus" textareaValueDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onInput', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onInputWith :: (TextareaEvent -> DOMRef -> action) -> Attribute model action
onInputWith action = on "input" textareaDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onSelection', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onSelectionWith :: (SelectionEvent -> DOMRef -> action) -> Attribute model action
onSelectionWith action = on "selection" selectionDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
