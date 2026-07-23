-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Text.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Text.Property
  ( -- *** Property
    textMaxLine_
  , includeFontPadding_
  , tailColorConvert_
  , textSingleLineVerticalAlign_
  , textSelection_
  , customContextMenu_
  , customTextSelection_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#text-maxline
--
-- Limits the maximum number of lines displayed for the text content,
-- overflow:hidden should be set simultaneously.
--
-- > textMaxLine_ 0
--
-- Default Value: -1
--
textMaxLine_ :: Int -> Attribute action
textMaxLine_ = intProp "text-max-line"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#include-font-padding
--
-- *Android* only
--
-- Add additional padding for Android text on top and bottom. Enabling this
-- may cause inconsistencies between platforms.
--
-- > includeFontPadding_ True
-- 
-- Default Value: 'False'
--
includeFontPadding_ :: Bool -> Attribute action
includeFontPadding_ = boolProp "include-font-padding"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#tail-color-convert
--
-- By default, if the text is truncated, the inserted ... will be displayed with
-- the color specified by the closest inline-text's style. If this attribute
-- is enabled, the color of ... will be specified by the outermost text tag's style.
--
-- > tailColorConvert_ True
-- 
-- Default Value: 'False'
--
tailColorConvert_ :: Bool -> Attribute action
tailColorConvert_ = boolProp "tail-color-convert"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#text-single-line-vertical-align
--
-- Used to set vertical alignment for single-line plain text. It can be changed
-- by setting "top" | "center" | "bottom". It is recommended to use this only
-- when the default font does not meet the center alignment requirements, as it
-- increases text measurement time.
--
-- > textSingleLineVerticalAlign_ "normal"
--
-- Default Value: "normal"
--
textSingleLineVerticalAlign_ :: MisoString -> Attribute action
textSingleLineVerticalAlign_ = textProp "text-single-line-vertical-align"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#text-selection
--
-- Sets whether to enable text selection.
-- When enabled, flatten = False should be set simultaneously.
--
-- > textSelection_ True
--
-- Default Value: 'False'
--
textSelection_ :: Bool -> Attribute action
textSelection_ = boolProp "text-selection"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#custom-context-menu
--
-- Used to set whether to turn on the custom pop-up context menu after selection
-- and copying. It takes effect after enabling text-selection.
--
-- > customContextMenu_ True
--
-- Default Value: 'False'
--
customContextMenu_ :: Bool -> Attribute action
customContextMenu_ = boolProp "custom-context-menu"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#custom-text-selection
--
-- Used to set whether to enable the custom text selection function.
-- When it is enabled, the element will no longer handle the gesture logic
-- related to selection and copying. Developers need to control it through
-- APIs such as `setTextSelection`. It takes effect after enabling text-selection.
--
-- > customTextSelection_ True
--
-- Default Value: 'False'
--
customTextSelection_ :: Bool -> Attribute action
customTextSelection_ = boolProp "custom-text-selection"
-----------------------------------------------------------------------------
