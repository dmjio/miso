-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Input.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Input.Property
  ( -- *** Property
    androidFullscreenMode_
  , confirmType_
  , disabled_
  , inputFilter_
  , iosAutoCorrect_
  , iosSpellCheck_
  , maxlength_
  , placeholder_
  , readonly_
  , showSoftInputOnFocus_
  , type_
    -- *** Types
  , InputType (..)
  , ConfirmType (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..))
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | The content type of an \<input\>, used by 'type_'.
data InputType
  = InputNumber
  | InputText
  | InputDigit
  | InputPassword
  | InputTel
  | InputEmail
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON InputType where
  toJSON InputNumber   = "number"
  toJSON InputText     = "text"
  toJSON InputDigit    = "digit"
  toJSON InputPassword = "password"
  toJSON InputTel      = "tel"
  toJSON InputEmail    = "email"
-----------------------------------------------------------------------------
-- | The confirm button type of an \<input\>, used by 'confirmType_'.
data ConfirmType
  = ConfirmSearch
  | ConfirmSend
  | ConfirmGo
  | ConfirmDone
  | ConfirmNext
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON ConfirmType where
  toJSON ConfirmSearch = "search"
  toJSON ConfirmSend   = "send"
  toJSON ConfirmGo     = "go"
  toJSON ConfirmDone   = "done"
  toJSON ConfirmNext   = "next"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#android-fullscreen-mode
--
-- Whether to enter the full-screen input mode when in landscape screen.
--
-- > androidFullscreenMode_ False
--
-- Default Value: 'True'
--
androidFullscreenMode_ :: Bool -> Attribute action
androidFullscreenMode_ = boolProp "android-fullscreen-mode"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#confirm-type
--
-- Specifies the confirm button type.
--
-- > confirmType_ ConfirmDone
--
-- Default Value: 'ConfirmSend'
--
confirmType_ :: ConfirmType -> Attribute action
confirmType_ = prop "confirm-type"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#disabled
--
-- Controls whether interaction is enabled.
--
-- > disabled_ True
--
-- Default Value: 'False'
--
disabled_ :: Bool -> Attribute action
disabled_ = boolProp "disabled"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#input-filter
--
-- Filters the input content in the form of a regular expression.
--
-- > inputFilter_ "[0-9]"
--
inputFilter_ :: MisoString -> Attribute action
inputFilter_ = textProp "input-filter"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#ios-auto-correct
--
-- Enables auto-correction on iOS.
--
-- > iosAutoCorrect_ False
--
-- Default Value: 'True'
--
iosAutoCorrect_ :: Bool -> Attribute action
iosAutoCorrect_ = boolProp "ios-auto-correct"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#ios-spell-check
--
-- Enables spell-checking on iOS.
--
-- > iosSpellCheck_ False
--
-- Default Value: 'True'
--
iosSpellCheck_ :: Bool -> Attribute action
iosSpellCheck_ = boolProp "ios-spell-check"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#maxlength
--
-- Maximum input length allowed.
--
-- > maxlength_ 32
--
-- Default Value: 140
--
maxlength_ :: Int -> Attribute action
maxlength_ = intProp "maxlength"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#placeholder
--
-- Placeholder text display.
--
-- > placeholder_ "Enter your name"
--
placeholder_ :: MisoString -> Attribute action
placeholder_ = textProp "placeholder"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#readonly
--
-- Makes the input read-only.
--
-- > readonly_ True
--
-- Default Value: 'False'
--
readonly_ :: Bool -> Attribute action
readonly_ = boolProp "readonly"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#show-soft-input-on-focus
--
-- Show soft input keyboard while focused.
--
-- > showSoftInputOnFocus_ False
--
-- Default Value: 'True'
--
showSoftInputOnFocus_ :: Bool -> Attribute action
showSoftInputOnFocus_ = boolProp "show-soft-input-on-focus"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/input.html#type
--
-- Input content type.
--
-- > type_ InputPassword
--
-- Default Value: 'InputText'
--
type_ :: InputType -> Attribute action
type_ = prop "type"
-----------------------------------------------------------------------------
