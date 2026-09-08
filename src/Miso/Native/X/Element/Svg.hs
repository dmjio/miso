-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Svg
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- [/<svg/>](https://lynxjs.org/api/elements/built-in/svg.html)
--
-- Displays SVG content supplied inline or by URL.
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Svg
  ( -- *** Element
    svgWith_
  , module Miso.Native.X.Element.Svg.Event
  , module Miso.Native.X.Element.Svg.Property
  ) where
-----------------------------------------------------------------------------
import Miso.Native.X.Element (svg_)
import Miso.Native.X.Element.Svg.Event
import Miso.Native.X.Element.Svg.Property
import Miso.Types (Attribute, View, withModel, withProps)
-----------------------------------------------------------------------------
-- | An @\<svg\>@ whose inline content is rendered against the enclosing
-- component's @props@ and @model@, so the content may itself use
-- 'Miso.Types.vprops' \/ 'Miso.Types.vmodel'. The values are obtained
-- ambiently with 'Miso.Types.withProps' \/ 'Miso.Types.withModel' and passed
-- to 'contentWith_':
--
-- @
-- svgWith_ [ 'Miso.CSS.style_' [ 'Miso.CSS.width' "100px" ] ] $
--   Svg.svg_ [ textProp "xmlns" "http://www.w3.org/2000/svg" ]
--     [ 'Miso.Types.vmodel' $ \\Model { color } -> Svg.circle_ [ Svg.fill_ color ] [] ]
-- @
--
-- Compare 'content_', which takes static content with @props@ and @model@
-- fixed to @()@.
--
-- @since 1.14.0.0
svgWith_
  :: [Attribute model action]
  -> View context props model action
  -- ^ Inline SVG content, in the component's own @props@ \/ @model@ types
  -> View context props model action
svgWith_ attrs content =
  withProps $ \props -> withModel $ \model ->
    svg_ (contentWith_ props model content : attrs) []
-----------------------------------------------------------------------------
