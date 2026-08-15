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
----------------------------------------------------------------------------
module Miso.Native.X.Element.Svg.Property
  ( -- *** Property
    content_
  , contentRaw_
  , src_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
import           Miso.Types (Attribute, View)
import           Miso.Property
import           Miso.Html.Render (toHtml)
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
-- Inline SVG XML content using 'miso' 'Miso.Typess.View' Syntax.
--
-- > content_ (svg_ [] [])
--
-- N.B. Must use "Miso.Svg" and 'Miso.Svg.Element.svg_' combinator.
--
content_ :: View context model action -> Attribute model action
content_ = textProp "content" . ms . toHtml
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
