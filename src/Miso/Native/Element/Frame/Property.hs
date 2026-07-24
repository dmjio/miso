-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Frame.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Frame.Property
  ( -- *** Property
    src_
  , autoHeight_
  , autoWidth_
  , data_
  , enableMultiAsyncThread_
  , globalProps_
  , presetHeight_
  , presetWidth_
  ) where
----------------------------------------------------------------------------
import           Miso.JSON (Value)
import           Miso.Property
import           Miso.Types
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#src
--
-- Sets the loading path for the \<frame\> resource. Supports @http@ / @https@.
--
-- > src_ "http://url-goes-here.com"
--
src_ :: MisoString -> Attribute action
src_ = textProp "src"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#auto-height
--
-- When enabled, the \<frame\> adjusts its height to match the embedded page.
--
-- > autoHeight_ True
--
-- Default Value: 'False'
--
autoHeight_ :: Bool -> Attribute action
autoHeight_ = boolProp "auto-height"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#auto-width
--
-- When enabled, the \<frame\> adjusts its width to match the embedded page.
--
-- > autoWidth_ True
--
-- Default Value: 'False'
--
autoWidth_ :: Bool -> Attribute action
autoWidth_ = boolProp "auto-width"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#data
--
-- Initial data (a JSON object) passed to the embedded \<frame\> page.
--
-- > data_ (object [ "key" .= "value" ])
--
data_ :: Value -> Attribute action
data_ = prop "data"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#enable-multi-async-thread
--
-- Whether the embedded page runs on its own async thread.
--
-- > enableMultiAsyncThread_ True
--
-- Default Value: 'False'
--
enableMultiAsyncThread_ :: Bool -> Attribute action
enableMultiAsyncThread_ = boolProp "enable-multi-async-thread"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#global-props
--
-- Global properties (a JSON object) injected into the embedded \<frame\> page.
--
-- > globalProps_ (object [ "theme" .= "dark" ])
--
globalProps_ :: Value -> Attribute action
globalProps_ = prop "global-props"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#preset-height
--
-- Preset height of the \<frame\> before the embedded page loads, e.g. @\"100px\"@
-- or @\"100rpx\"@.
--
-- > presetHeight_ "100px"
--
presetHeight_ :: MisoString -> Attribute action
presetHeight_ = textProp "preset-height"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#preset-width
--
-- Preset width of the \<frame\> before the embedded page loads, e.g. @\"100px\"@
-- or @\"100rpx\"@.
--
-- > presetWidth_ "100px"
--
presetWidth_ :: MisoString -> Attribute action
presetWidth_ = textProp "preset-width"
-----------------------------------------------------------------------------
