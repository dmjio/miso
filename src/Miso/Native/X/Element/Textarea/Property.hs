-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Textarea.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Textarea.Property
  ( -- *** Property
    androidFullscreenMode_
  , bounces_
  , confirmType_
  , disabled_
  , enableScrollBar_
  , inputFilter_
  , iosAutoCorrect_
  , iosSpellCheck_
  , lineSpacing_
  , maxlength_
  , maxlines_
  , placeholder_
  , readonly_
  , showSoftInputOnFocus_
  , type_
    -- *** Types
  , TextareaType (..)
  , ConfirmType (..)
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..))
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | The content type of a \<textarea\>, used by 'type_'.
data TextareaType
  = TextareaNumber
  | TextareaText
  | TextareaDigit
  | TextareaTel
  | TextareaEmail
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON TextareaType where
  toJSON TextareaNumber = "number"
  toJSON TextareaText   = "text"
  toJSON TextareaDigit  = "digit"
  toJSON TextareaTel    = "tel"
  toJSON TextareaEmail  = "email"
-----------------------------------------------------------------------------
-- | The confirm button type of a \<textarea\>, used by 'confirmType_'.
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
-- | https://lynxjs.org/api/elements/built-in/textarea.html#android-fullscreen-mode
--
-- Whether to enter the full-screen input mode when in landscape screen.
--
-- Default Value: 'True'
--
androidFullscreenMode_ :: Bool -> Attribute action
androidFullscreenMode_ = boolProp "android-fullscreen-mode"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#bounces
--
-- *iOS* only. Bounce effect when scrolling past the boundary.
--
-- Default Value: 'True'
--
bounces_ :: Bool -> Attribute action
bounces_ = boolProp "bounces"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#confirm-type
--
-- Specifies the confirm button type.
--
-- Default Value: 'ConfirmDone'
--
confirmType_ :: ConfirmType -> Attribute action
confirmType_ = prop "confirm-type"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#disabled
--
-- Controls whether interaction is enabled.
--
-- Default Value: 'False'
--
disabled_ :: Bool -> Attribute action
disabled_ = boolProp "disabled"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#enable-scroll-bar
--
-- Whether to show the scroll bar.
--
-- Default Value: 'False'
--
enableScrollBar_ :: Bool -> Attribute action
enableScrollBar_ = boolProp "enable-scroll-bar"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#input-filter
--
-- Filters the input content in the form of a regular expression.
--
inputFilter_ :: MisoString -> Attribute action
inputFilter_ = textProp "input-filter"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#ios-auto-correct
--
-- Enables auto-correction on iOS.
--
-- Default Value: 'True'
--
iosAutoCorrect_ :: Bool -> Attribute action
iosAutoCorrect_ = boolProp "ios-auto-correct"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#ios-spell-check
--
-- Enables spell-checking on iOS.
--
-- Default Value: 'True'
--
iosSpellCheck_ :: Bool -> Attribute action
iosSpellCheck_ = boolProp "ios-spell-check"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#line-spacing
--
-- Line spacing.
--
lineSpacing_ :: Double -> Attribute action
lineSpacing_ = doubleProp "line-spacing"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#maxlength
--
-- Maximum input length allowed.
--
-- Default Value: 140
--
maxlength_ :: Int -> Attribute action
maxlength_ = intProp "maxlength"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#maxlines
--
-- Maximum number of input lines.
--
maxlines_ :: Int -> Attribute action
maxlines_ = intProp "maxlines"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#placeholder
--
-- Placeholder text display.
--
placeholder_ :: MisoString -> Attribute action
placeholder_ = textProp "placeholder"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#readonly
--
-- Makes the input read-only.
--
-- Default Value: 'False'
--
readonly_ :: Bool -> Attribute action
readonly_ = boolProp "readonly"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#show-soft-input-on-focus
--
-- Show soft input keyboard while focused.
--
-- Default Value: 'True'
--
showSoftInputOnFocus_ :: Bool -> Attribute action
showSoftInputOnFocus_ = boolProp "show-soft-input-on-focus"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/textarea.html#type
--
-- Input content type.
--
-- Default Value: 'TextareaText'
--
type_ :: TextareaType -> Attribute action
type_ = prop "type"
-----------------------------------------------------------------------------
