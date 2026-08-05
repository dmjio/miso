-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Mathml.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Mathml.Property" provides 'Miso.Types.Attribute' smart constructors
-- for
-- <https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Attribute MathML attributes>.
-- They are used alongside the element constructors from "Miso.Mathml.Element".
-- The module is re-exported by "Miso.Mathml".
--
-- = Quick start
--
-- @
-- import "Miso.Mathml.Element"
-- import "Miso.Mathml.Property"
--
-- styledFrac :: 'Miso.Types.View' model action
-- styledFrac =
--   'Miso.Mathml.Element.math_' [ 'display_' \"block\" ]
--     [ 'Miso.Mathml.Element.mfrac_' [ 'linethickness_' \"2px\" ]
--         [ 'Miso.Mathml.Element.mn_' [ 'mathvariant_' \"bold\" ] [ 'Miso.text' \"1\" ]
--         , 'Miso.Mathml.Element.mn_' [] [ 'Miso.text' \"3\" ]
--         ]
--     ]
-- @
--
-- = Attribute groups
--
-- * __Global MathML attributes__: 'dir_', 'displaystyle_', 'scriptlevel_',
--   'id_', 'href_', 'mathbackground_', 'mathcolor_', 'mathsize_', 'mathvariant_'
-- * __Layout__: 'display_', 'height_', 'width_', 'depth_', 'voffset_',
--   'lspace_', 'rspace_', 'linethickness_', 'minsize_', 'maxsize_'
-- * __Table__: 'align_', 'rowalign_', 'rowlines_', 'rowspacing_', 'rowspan_',
--   'columnalign_', 'columnlines_', 'columnspacing_', 'columnspan_'
-- * __Operator flags__ (boolean): 'accent_', 'accentunder_', 'fence_',
--   'separator_', 'stretchy_', 'symmetric_', 'movablelimits_'
-- * __Frame__: 'frame_', 'framespacing_'
-- * __Grouping__: 'open_', 'close_', 'notation_'
--
-- For full semantics of each attribute consult the
-- <https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Attribute MDN MathML attribute reference>.
--
-- = See also
--
-- * "Miso.Mathml.Element" — MathML element constructors
-- * "Miso.Mathml" — re-export hub for the full MathML DSL
-- * "Miso.Property" — low-level 'Miso.Property.textProp', 'Miso.Property.boolProp', 'Miso.Property.intProp'
-----------------------------------------------------------------------------
module Miso.Mathml.Property
  ( -- * Global attributes
    dir_
  , displaystyle_
  , scriptlevel_
  -- * Regular attributes
  , accent_
  , accentunder_
  , align_
  , columnalign_
  , columnlines_
  , columnspacing_
  , columnspan_
  , depth_
  , display_
  , fence_
  , frame_
  , framespacing_
  , height_
  , href_
  , id_
  , linethickness_
  , lspace_
  , mathbackground_
  , mathcolor_
  , mathsize_
  , mathvariant_
  , maxsize_
  , minsize_
  , movablelimits_
  , notation_
  , rowalign_
  , rowlines_
  , rowspacing_
  , rowspan_
  , rspace_
  , separator_
  , stretchy_
  , symmetric_
  , voffset_
  , width_
  , close_
  , open_
  ) where
-----------------------------------------------------------------------------
import           Miso.Types
import           Miso.Property
-----------------------------------------------------------------------------
-- | [dir](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Global_attributes/dir)
--
-- @since 1.9.0.0
dir_ :: MisoString -> Attribute model action
dir_ = textProp "dir"
-----------------------------------------------------------------------------
-- | [displaystyle](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Global_attributes/displaystyle)
--
-- @since 1.9.0.0
displaystyle_ :: MisoString -> Attribute model action
displaystyle_ = textProp "displaystyle"
------------------------------------------------------------------------------
-- | [scriptlevel](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Global_attributes/scriptlevel)
--
-- @since 1.9.0.0
scriptlevel_ :: Int -> Attribute model action
scriptlevel_ = intProp "scriptlevel"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
accent_ ::  Bool -> Attribute model action
accent_ = boolProp "accent"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
accentunder_ ::  Bool -> Attribute model action
accentunder_ = boolProp "accentunder"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
align_ ::  Bool -> Attribute model action
align_ = boolProp "align"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnalign_ :: MisoString -> Attribute model action
columnalign_ = textProp "columnalign"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnlines_ :: MisoString -> Attribute model action
columnlines_ = textProp "columnlines"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnspacing_ :: MisoString -> Attribute model action
columnspacing_ = textProp "columnspacing"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnspan_ :: Int -> Attribute model action
columnspan_ = intProp "columnspan"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
depth_ :: MisoString -> Attribute model action
depth_ = textProp "depth"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
display_ :: MisoString -> Attribute model action
display_ = textProp "display"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
fence_ :: Bool -> Attribute model action
fence_ = boolProp "fence"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
frame_ :: MisoString -> Attribute model action
frame_ = textProp "frame"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
framespacing_ :: MisoString -> Attribute model action
framespacing_ = textProp "framespacing"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
height_ :: MisoString -> Attribute model action
height_ = textProp "height"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
href_ :: MisoString -> Attribute model action
href_ = textProp "href"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
id_ :: MisoString -> Attribute model action
id_ = textProp "id"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
linethickness_ :: MisoString -> Attribute model action
linethickness_ = textProp "linethickness"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
lspace_ :: MisoString -> Attribute model action
lspace_ = textProp "lspace"
-- | @since 1.9.0.0
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathbackground_ :: MisoString -> Attribute model action
mathbackground_ = textProp "mathbackground"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathcolor_ :: MisoString -> Attribute model action
mathcolor_ = textProp "mathcolor"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathsize_ :: MisoString -> Attribute model action
mathsize_ = textProp "mathsize"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathvariant_ :: MisoString -> Attribute model action
mathvariant_ = textProp "mathvariant"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
maxsize_ :: MisoString -> Attribute model action
maxsize_ = textProp "maxsize"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
minsize_ :: MisoString -> Attribute model action
minsize_ = textProp "minsize"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
movablelimits_ :: Bool -> Attribute model action
movablelimits_ = boolProp "movablelimits"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
notation_ :: MisoString -> Attribute model action
notation_ = textProp "notation"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowalign_ :: MisoString -> Attribute model action
rowalign_ = textProp "rowalign"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowlines_ :: MisoString -> Attribute model action
rowlines_ = textProp "rowlines"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowspacing_ :: MisoString -> Attribute model action
rowspacing_ = textProp "rowspacing"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowspan_ :: Int -> Attribute model action
rowspan_ = intProp "rowspan"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rspace_ :: MisoString -> Attribute model action
rspace_ = textProp "rspace"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
separator_ :: Bool -> Attribute model action
separator_ = boolProp "separator"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
stretchy_ :: Bool -> Attribute model action
stretchy_ = boolProp "stretchy"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
symmetric_ :: Bool -> Attribute model action
symmetric_ = boolProp "symmetric"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
voffset_ :: MisoString -> Attribute model action
voffset_ = textProp "voffset"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
width_ :: MisoString -> Attribute model action
width_ = textProp "width"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
close_ :: MisoString -> Attribute model action
close_ = textProp "close"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
open_ :: MisoString -> Attribute model action
open_ = textProp "open"
-----------------------------------------------------------------------------
