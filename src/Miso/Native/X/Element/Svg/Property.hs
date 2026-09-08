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
-- Inline SVG XML content using 'Miso.miso' 'Miso.Types.View' Syntax.
--
-- > content_ (svg_ [] [])
--
-- N.B. Must use "Miso.Svg" and 'Miso.Svg.Element.svg_' combinator.
--
-- The content is serialised to a string when the attribute is built, so there
-- is no running component to take @context@, @props@ or @model@ from: the
-- caller supplies all three, and 'Miso.Types.vcontext' \/ 'Miso.Types.vprops'
-- \/ 'Miso.Types.vmodel' inside the content resolve against those values. To
-- draw from the enclosing component, obtain them with 'Miso.Types.withContext'
-- \/ 'Miso.Types.withProps' \/ 'Miso.Types.withModel' above the element and
-- pass them down:
--
-- > withContext $ \context -> withProps $ \props -> withModel $ \model ->
-- >   svg_
-- >     [ content_ context props model $ Svg.svg_
-- >         [ textProp "xmlns" "http://www.w3.org/2000/svg" ]
-- >         [ Svg.circle_ [ Svg.fill_ color ] [] ]
-- >     ]
-- >     []
--
-- or use 'Miso.Native.X.Element.Svg.svgWith_', which does that lifting and
-- lets the content itself use 'Miso.Types.vcontext' \/ 'Miso.Types.vprops' \/
-- 'Miso.Types.vmodel'.
--
--
-- @since 1.14.0.0
content_ :: context -> props -> model -> View context props model action -> Attribute model action
content_ context props model = textProp "content" . ms . toHtmlWith context props model
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
