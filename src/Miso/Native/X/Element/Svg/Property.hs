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
-- Inline SVG XML content using 'Miso.miso' 'Miso.Types.View' Syntax.
--
-- > content_ (svg_ [] [])
--
-- N.B. Must use "Miso.Svg" and 'Miso.Svg.Element.svg_' combinator.
--
-- The content is serialised to a string when the attribute is built, so the
-- 'Miso.Types.View' has no enclosing component to take @props@ or @model@
-- from: both are fixed to @()@, and a 'Miso.Types.vprops' \/
-- 'Miso.Types.vmodel' inside the content is a type error. Event handlers
-- are dropped by the serialiser. Note this pins the /content's/ type
-- parameters only — the enclosing component's @model@ is unconstrained. To
-- draw from the component's @props@ or @model@, either lift
-- 'Miso.Types.withProps' \/ 'Miso.Types.withModel' above the element so the
-- values are closed over:
--
-- > withModel $ \Model { color } ->
-- >   svg_
-- >     [ content_ $ Svg.svg_ [ textProp "xmlns" "http://www.w3.org/2000/svg" ]
-- >         [ Svg.circle_ [ Svg.fill_ color ] [] ]
-- >     ]
-- >     []
--
-- or use 'Miso.Native.X.Element.Svg.svgWith_', which does that lifting and
-- lets the content itself use 'Miso.Types.vprops' \/ 'Miso.Types.vmodel'.
--
content_ :: View context () () action -> Attribute model action
content_ = contentWith_ () ()
-----------------------------------------------------------------------------
-- | 'content_' for a 'Miso.Types.View' that reads the enclosing component's
-- @props@ \/ @model@ (via 'Miso.Types.vprops' \/ 'Miso.Types.vmodel'):
-- the values are supplied explicitly and the content is rendered with
-- 'Miso.Html.Render.toHtmlWith'. 'Miso.Native.X.Element.Svg.svgWith_'
-- obtains them ambiently for you.
--
-- @since 1.14.0.0
contentWith_ :: props -> model -> View context props model action -> Attribute model action
contentWith_ props model = textProp "content" . ms . toHtmlWith props model
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
