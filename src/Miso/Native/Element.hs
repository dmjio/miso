-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element
  ( -- ** Smart constructor for lynx elements
    lynx_
    -- ** Page
  , page_
    -- ** View
  , view_
    -- ** Scroll View
  , scrollView_
    -- ** Image
  , image_
    -- ** List
  , list_
  , listItem_
    -- * Text
  , text_
    -- * Frame
  , frame_
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (toJSON)
import           Miso.Native.Element.List (ListOptions(..))
import           Miso.Property (textProp, prop)
import           Miso.String (MisoString)
import           Miso.Types (View, Attribute, node, Namespace(HTML))
-----------------------------------------------------------------------------
-- | Smart constructor for constructing a built-in lynx element.
--
lynx_ :: MisoString -> [Attribute action] -> [View context action] -> View context action
lynx_ = node HTML
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/page.html>
--
-- <page> element is the root node, only one <page> element is allowed per page.
-- You can omit the explicit <page> wrapper, as the frontend framework will
-- generate the root node by default.
--
-- You shouldn't use this, we already generate the 'page' for you when
-- the initial 'renderPage' callback is invoked by PrimJS, and there can
-- only be one 'page' present at at time. We include it here for completeness,
-- and because 'page' functionality might change in the future.
--
page_ :: [Attribute action] -> [View context action] -> View context action
page_ = lynx_ "page"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-view.html>
--
-- Basic element, used to contain other elements. <view> is the foundation
-- for all other elements; its attributes, events, and methods can be
-- used in other elements.
--
scrollView_ :: [Attribute action] -> [View context action] -> View context action
scrollView_ = lynx_ "scroll-view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/view.html>
--
-- Basic element, used to contain other elements. <view> is the foundation
-- for all other elements; its attributes, events, and methods can be
-- used in other elements.
--
view_ :: [Attribute action] -> [View context action] -> View context action
view_ = lynx_ "view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html>
--
-- Used to display different types of images, including web images,
-- static resources, and locally stored images.
--
-- <https://lynxjs.org/api/elements/built-in/image.html>
--
-- 'image_' does not support children.
--
-- <https://lynxjs.org/api/elements/built-in/image.html#required-src>
--
-- *Required*
--
-- 'image_' takes a required *src* parameter (as 'MisoString') by default.
--
-- The supported image formats are: *png*, *jpg*, *jpeg*, *bmp*, *gif*, and *webp*.
--
-- > image_ "https://url.com/image.png" []
--
image_ :: MisoString -> [Attribute action] -> View context action
image_ url attrs = lynx_ "image" (textProp "src" url : attrs) []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/list.html>
--
listItem_ :: [Attribute action] -> [View context action] -> View context action
listItem_ = lynx_ "list-item"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/list.html>
--
list_ :: ListOptions -> [Attribute action] -> [View context action] -> View context action
list_ ListOptions {..} attrs = lynx_ "list" (defaults <> attrs)
  where
    defaults =
      [ prop "list-type" (toJSON listType_)
      , prop "span-count" (toJSON spanCount_)
      , prop "scroll-orientation" (toJSON scrollOrientation_)
      ]
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/text.html>
--
-- <text> is a built-in component in Lynx used to display text content.
-- It supports specifying text style, binding click event callbacks, and can
-- nest <text>, <image>, and <view> components to achieve relatively complex
-- text and image content presentation.
--
text_ :: [Attribute action] -> [View context action] -> View context action
text_ = lynx_ "text"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/frame.html>
--
-- A page element similar to HTML's \<iframe\>, which can embed a Lynx page
-- into the current page.
--
frame_ :: [Attribute action] -> View context action
frame_ attrs = lynx_ "frame" attrs []
-----------------------------------------------------------------------------
