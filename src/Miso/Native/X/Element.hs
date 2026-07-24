-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Smart constructors for the Lynx
-- [XElement](https://lynxjs.org/api/elements/built-in.html) family — the
-- extended, opt-in built-in elements (@\<input\>@, @\<textarea\>@,
-- @\<overlay\>@, @\<svg\>@, @\<refresh\>@, @\<viewpager\>@,
-- @\<scroll-coordinator\>@, @\<blur-view\>@, @\<webview\>@ and
-- @\<title-bar-view\>@).
--
-- Attributes, events and methods for each element live in the corresponding
-- @Miso.Native.X.Element.\<Name\>@ modules.
--
----------------------------------------------------------------------------
module Miso.Native.X.Element
  ( -- ** Input
    input_
    -- ** Textarea
  , textarea_
    -- ** Overlay
  , overlay_
    -- ** Svg
  , svg_
    -- ** Refresh
  , refresh_
  , refreshHeader_
    -- ** Viewpager
  , viewpager_
  , viewpagerItem_
    -- ** Scroll Coordinator
  , scrollCoordinator_
    -- ** Blur View
  , blurView_
    -- ** Webview
  , webview_
    -- ** Title Bar View
  , titleBarView_
  ) where
-----------------------------------------------------------------------------
import           Miso.Native.Element (lynx_)
import           Miso.Types (View, Attribute)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/input.html>
--
-- Single-line text input element. Does not support children.
--
input_ :: [Attribute action] -> View context action
input_ attrs = lynx_ "input" attrs []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/textarea.html>
--
-- Multi-line text input element. Does not support children.
--
textarea_ :: [Attribute action] -> View context action
textarea_ attrs = lynx_ "textarea" attrs []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/overlay.html>
--
-- Renders its children on an independent layer above the page.
--
overlay_ :: [Attribute action] -> [View context action] -> View context action
overlay_ = lynx_ "overlay"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/svg.html>
--
-- Displays SVG content, supplied either inline via @content_@ or by URL via
-- @src_@.
--
svg_ :: [Attribute action] -> [View context action] -> View context action
svg_ = lynx_ "svg"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/refresh.html>
--
-- Pull-to-refresh container. Wraps a @refreshHeader_@ and a vertically
-- scrollable child.
--
refresh_ :: [Attribute action] -> [View context action] -> View context action
refresh_ = lynx_ "refresh"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/refresh.html>
--
-- Customizable header revealed during the pull gesture of a @refresh_@.
--
refreshHeader_ :: [Attribute action] -> [View context action] -> View context action
refreshHeader_ = lynx_ "refresh-header"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/viewpager.html>
--
-- Horizontally paged container. Each page is a @viewpagerItem_@.
--
viewpager_ :: [Attribute action] -> [View context action] -> View context action
viewpager_ = lynx_ "viewpager"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/viewpager.html>
--
-- A single page within a @viewpager_@.
--
viewpagerItem_ :: [Attribute action] -> [View context action] -> View context action
viewpagerItem_ = lynx_ "viewpager-item"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-coordinator.html>
--
-- Coordinates nested scrolling, typically used with sticky headers and
-- tabbed layouts.
--
scrollCoordinator_ :: [Attribute action] -> [View context action] -> View context action
scrollCoordinator_ = lynx_ "scroll-coordinator"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/blur-view.html>
--
-- Applies a Gaussian blur / material effect to the content behind it.
--
blurView_ :: [Attribute action] -> [View context action] -> View context action
blurView_ = lynx_ "blur-view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/webview.html>
--
-- Embeds a web page. Does not support children.
--
webview_ :: [Attribute action] -> View context action
webview_ attrs = lynx_ "webview" attrs []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/title-bar-view.html>
--
-- Defines a custom draggable window region (Clay Windows / macOS).
--
titleBarView_ :: [Attribute action] -> [View context action] -> View context action
titleBarView_ = lynx_ "title-bar-view"
-----------------------------------------------------------------------------
