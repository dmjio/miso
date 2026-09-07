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
-- Inline SVG XML content using 'Miso.miso' 'Miso.Types.View' Syntax.
--
-- > content_ (svg_ [] [])
--
-- N.B. Must use "Miso.Svg" and 'Miso.Svg.Element.svg_' combinator.
--
-- The content is serialised to a string when the attribute is built, so the
-- 'Miso.Types.View' has no running component to take @context@ or @props@
-- from and both are fixed to @()@. To draw from the component's @context@
-- or @props@, lift 'Miso.Types.withContext' \/ 'Miso.Types.withProps' above
-- the element instead of using the accessors inside the content:
--
-- > withProps $ \Props { color } ->
-- >   svg_
-- >     [ content_ $ Svg.svg_ [ textProp "xmlns" "http://www.w3.org/2000/svg" ]
-- >         [ Svg.circle_ [ Svg.fill_ color ] [] ]
-- >     ]
-- >     []
--
content_ :: View () () model action -> Attribute model action
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
