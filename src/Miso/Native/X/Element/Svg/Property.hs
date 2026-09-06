-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Svg.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Svg.Property
  ( -- *** Property
    content_
  , contentWith_
  , contentRaw_
  , src_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
import           Miso.Types (Attribute, View)
import           Miso.Property
import           Miso.Html.Render (toHtmlWith)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/svg.html#content
--
-- Inline SVG XML content.
--
-- > content_ "<svg>...</svg>"
--
contentRaw_ :: MisoString -> Attribute model action
contentRaw_ = textProp "content"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/svg.html#content
--
-- Inline SVG XML content using 'Miso.miso' 'Miso.Typess.View' Syntax.
--
-- > content_ (svg_ [] [])
--
-- N.B. Must use "Miso.Svg" and 'Miso.Svg.Element.svg_' combinator.
--
content_ :: View context () model action -> Attribute model action
content_ = contentWith_ ()
-----------------------------------------------------------------------------
-- | Like 'content_', but for a 'Miso.Types.View' whose @props@ type is not
-- @()@ — e.g. one containing a 'Miso.Types.vprops' node. The @props@ value
-- is what those nodes resolve against; the caller is always inside
-- 'Miso.Types.view', where it is in scope.
--
-- > contentWith_ props (svg_ [] [ vprops $ \Props { color } -> circle_ [ fill_ color ] [] ])
--
-- @since 1.14.0.0
contentWith_ :: props -> View context props model action -> Attribute model action
contentWith_ props = textProp "content" . ms . toHtmlWith props
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/svg.html#src
--
-- SVG resource URL.
--
-- > src_ "https://url.com/image.svg"
--
src_ :: MisoString -> Attribute model action
src_ = textProp "src"
-----------------------------------------------------------------------------
