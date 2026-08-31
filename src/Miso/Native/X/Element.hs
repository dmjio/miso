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
-- @\<scroll-coordinator\>@, @\<blur-view\>@, @\<webview\>@,
-- @\<title-bar-view\>@ and @\<video\>@).
--
-- Attributes, events and methods for each element live in the corresponding
-- @Miso.Native.X.Element.\<Name\>@ modules.
--
-- @since 1.13.0.0
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
  , scrollCoordinatorHeader_
  , scrollCoordinatorSlot_
  , scrollCoordinatorToolbar_
    -- ** Blur View
  , blurView_
    -- ** Webview
  , webview_
    -- ** Title Bar View
  , titleBarView_
    -- ** Video
  , video_
  ) where
-----------------------------------------------------------------------------
import           Miso.Native.Element (lynx_, lynxDirect_)
import           Miso.String (MisoString)
import           Miso.Types (View, Attribute)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/input.html>
--
-- Single-line text input element. Does not support children.
--
input_ :: [Attribute model action] -> View context model action
input_ attrs = lynxDirect_ inputDirectEvents "input" attrs []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/textarea.html>
--
-- Multi-line text input element. Does not support children.
--
textarea_ :: [Attribute model action] -> View context model action
textarea_ attrs = lynxDirect_ inputDirectEvents "textarea" attrs []
-----------------------------------------------------------------------------
-- | Events that @input@ and @textarea@ dispatch directly on the element (Lynx
-- component events that do not bubble to the delegated mount listener).
inputDirectEvents :: [MisoString]
inputDirectEvents = [ "blur", "confirm", "focus", "input", "selection" ]
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/overlay.html>
--
-- Renders its children on an independent layer above the page.
--
overlay_ :: [Attribute model action] -> [View context model action] -> View context model action
overlay_ = lynxDirect_
  [ "dismissoverlay", "error", "overlaytouch", "requestclose", "showoverlay" ]
  "overlay"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/svg.html>
--
-- Displays SVG content, supplied either inline via @content_@ or by URL via
-- @src_@.
--
svg_ :: [Attribute model action] -> [View context model action] -> View context model action
svg_ = lynxDirect_ [ "load" ] "svg"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/refresh.html>
--
-- Pull-to-refresh container. Wraps a @refreshHeader_@ and a vertically
-- scrollable child.
--
refresh_ :: [Attribute model action] -> [View context model action] -> View context model action
refresh_ = lynxDirect_
  [ "headeroffset", "refreshstatechange", "startrefresh" ]
  "refresh"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/refresh.html>
--
-- Customizable header revealed during the pull gesture of a @refresh_@.
--
refreshHeader_ :: [Attribute model action] -> [View context model action] -> View context model action
refreshHeader_ = lynx_ "refresh-header"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/viewpager.html>
--
-- Horizontally paged container. Each page is a @viewpagerItem_@.
--
viewpager_ :: [Attribute model action] -> [View context model action] -> View context model action
viewpager_ = lynxDirect_ [ "change", "offsetchange", "willchange" ] "viewpager"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/viewpager.html>
--
-- A single page within a @viewpager_@.
--
viewpagerItem_ :: [Attribute model action] -> [View context model action] -> View context model action
viewpagerItem_ = lynx_ "viewpager-item"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-coordinator.html>
--
-- Coordinates nested scrolling, typically used with sticky headers and
-- tabbed layouts.
--
scrollCoordinator_ :: [Attribute model action] -> [View context model action] -> View context model action
scrollCoordinator_ = lynxDirect_ [ "offset" ] "scroll-coordinator"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-coordinator.html>
--
-- The collapsing header of a @scrollCoordinator_@. Folds away as the slot
-- content scrolls. Required (together with @scrollCoordinatorSlot_@).
--
scrollCoordinatorHeader_ :: [Attribute model action] -> [View context model action] -> View context model action
scrollCoordinatorHeader_ = lynx_ "scroll-coordinator-header"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-coordinator.html>
--
-- The main content region of a @scrollCoordinator_@, whose scroll drives the
-- header fold. Holds the scrollable child (e.g. a @scrollView_@). Required
-- (together with @scrollCoordinatorHeader_@).
--
scrollCoordinatorSlot_ :: [Attribute model action] -> [View context model action] -> View context model action
scrollCoordinatorSlot_ = lynx_ "scroll-coordinator-slot"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-coordinator.html>
--
-- An optional sticky bar of a @scrollCoordinator_@ that stays pinned while the
-- header folds (e.g. tabs above the content).
--
scrollCoordinatorToolbar_ :: [Attribute model action] -> [View context model action] -> View context model action
scrollCoordinatorToolbar_ = lynx_ "scroll-coordinator-toolbar"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/blur-view.html>
--
-- Applies a Gaussian blur / material effect to the content behind it.
--
blurView_ :: [Attribute model action] -> [View context model action] -> View context model action
blurView_ = lynx_ "blur-view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/webview.html>
--
-- Embeds a web page. Does not support children.
--
webview_ :: [Attribute model action] -> View context model action
webview_ attrs = lynxDirect_
  [ "error", "load", "locationchange", "message", "openwindow" ]
  "webview" attrs []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/title-bar-view.html>
--
-- Defines a custom draggable window region (Clay Windows / macOS).
--
titleBarView_ :: [Attribute model action] -> [View context model action] -> View context model action
titleBarView_ = lynx_ "title-bar-view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/next/api/elements/built-in/video.html>
--
-- Experimental video playback element. Does not support children.
--
video_ :: [Attribute model action] -> View context model action
video_ attrs = lynxDirect_
  [ "firstframe", "playing", "paused", "stopped", "timeupdate", "ended", "looped", "error", "buffering" ]
  "video" attrs []
-----------------------------------------------------------------------------
