-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Svg" is the re-export hub for miso's SVG DSL. It re-exports
-- "Miso.Svg.Element" (all SVG element constructors) and "Miso.Svg.Event"
-- (SVG-specific event handlers).
--
-- SVG property\/attribute combinators live in "Miso.Svg.Property" and
-- must be imported separately when needed.
--
-- = Quick start
--
-- Embed an SVG circle inside a miso view:
--
-- @
-- import "Miso"
-- import "Miso.Svg"
-- import qualified "Miso.Svg.Property" as SP
--
-- badge :: 'Miso.Types.View' model action
-- badge =
--   'svg_' [ SP.'Miso.Svg.Property.width_' \"100\", SP.'Miso.Svg.Property.height_' \"100\" ]
--     [ 'circle_'
--         [ SP.'Miso.Svg.Property.cx_' \"50\", SP.'Miso.Svg.Property.cy_' \"50\"
--         , SP.'Miso.Svg.Property.r_' \"40\"
--         , SP.'Miso.Svg.Property.stroke_' \"green\"
--         , SP.'Miso.Svg.Property.strokeWidth_' \"4\"
--         , SP.'Miso.Svg.Property.fill_' \"yellow\"
--         ]
--     ]
-- @
--
-- = Modules
--
-- * "Miso.Svg.Element" — SVG element constructors (@svg_@, @circle_@, @path_@, @g_@, …)
-- * "Miso.Svg.Event"   — SVG event handlers (@onBegin@, @onEnd@, @onClick@, …)
-- * "Miso.Svg.Property" — SVG attribute combinators (@cx_@, @r_@, @fill_@, @stroke_@, …)
--   /(not re-exported here — import separately)/
--
-- = See also
--
-- * "Miso.Html.Element" — HTML element constructors
-- * "Miso.Mathml.Element" — MathML element constructors
-- * <https://developer.mozilla.org/en-US/docs/Web/SVG MDN SVG reference>
----------------------------------------------------------------------------
module Miso.Svg
   ( -- ** Element
     module Miso.Svg.Element
     -- ** Event
   , module Miso.Svg.Event
   ) where
-----------------------------------------------------------------------------
import Miso.Svg.Element
import Miso.Svg.Event
-----------------------------------------------------------------------------
