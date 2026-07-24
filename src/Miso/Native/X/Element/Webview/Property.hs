-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Webview.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Webview.Property
  ( -- *** Property
    bounces_
  , cookies_
  , enableDebug_
  , html_
  , initjs_
  , params_
  , scrollBarEnable_
  , src_
  , useOsr_
  , webviewType_
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (Value)
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bounces
--
-- *iOS* only. Enables the bounce effect.
--
-- Default Value: 'False'
--
bounces_ :: Bool -> Attribute action
bounces_ = boolProp "bounces"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#cookies
--
-- *Desktop, Lynx 3.5+*. Preset cookies.
--
cookies_ :: Value -> Attribute action
cookies_ = prop "cookies"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#enable-debug
--
-- Enables WebView debugging on Android so it can be debugged in Chrome DevTools.
--
-- Default Value: 'False'
--
enableDebug_ :: Bool -> Attribute action
enableDebug_ = boolProp "enable-debug"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#html
--
-- *Lynx 3.6+*. A string of HTML content to load. Automatically refreshes when
-- the HTML changes. Lower priority than @src_@.
--
html_ :: MisoString -> Attribute action
html_ = textProp "html"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#initjs
--
-- *Desktop, Lynx 3.5+*. Executes JavaScript when the document is ready.
--
initjs_ :: MisoString -> Attribute action
initjs_ = textProp "initjs"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#params
--
-- Params for the external webview implementation.
--
params_ :: Value -> Attribute action
params_ = prop "params"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#scroll-bar-enable
--
-- *iOS* only. Enables the scrollbar.
--
-- Default Value: 'False'
--
scrollBarEnable_ :: Bool -> Attribute action
scrollBarEnable_ = boolProp "scroll-bar-enable"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#src
--
-- The location of a resource on a remote server.
--
-- > src_ "https://url.com"
--
src_ :: MisoString -> Attribute action
src_ = textProp "src"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#use-osr
--
-- *Desktop, Lynx 3.5+*. Whether to enable offscreen rendering mode.
--
-- Default Value: 'False'
--
useOsr_ :: Bool -> Attribute action
useOsr_ = boolProp "use-osr"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#webview-type
--
-- Specifies the type of webview; it can be an implementation of a webview
-- injected from @LynxService@.
--
-- Default Value: @\"default\"@
--
webviewType_ :: MisoString -> Attribute action
webviewType_ = textProp "webview-type"
-----------------------------------------------------------------------------
