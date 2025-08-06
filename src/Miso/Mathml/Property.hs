-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Mathml.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- MathML attributes.
--
-- We recommend referring to [MDN Attributes reference](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Attribute)
-- for details on how each attribute can be used.
--
----------------------------------------------------------------------------
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
  ) where
-----------------------------------------------------------------------------
import           Miso.Types
import           Miso.Property
-----------------------------------------------------------------------------
-- | [<\dir\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Global_attributes/dir)
--
-- @since 1.9.0.0
dir_ :: MisoString -> Attribute action
dir_ = textProp "dir"
-----------------------------------------------------------------------------
-- | [<\displaystyle\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Global_attributes/displaystyle)
--
-- @since 1.9.0.0
displaystyle_ :: MisoString -> Attribute action
displaystyle_ = textProp "displaystyle"
------------------------------------------------------------------------------
-- | [<\scriptlevel\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Global_attributes/scriptlevel)
--
-- @since 1.9.0.0
scriptlevel_ :: Int -> Attribute action
scriptlevel_ = intProp "scriptlevel"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
accent_ ::  Bool -> Attribute action
accent_ = boolProp "accent"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
accentunder_ ::  Bool -> Attribute action
accentunder_ = boolProp "accentunder"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
align_ ::  Bool -> Attribute action
align_ = boolProp "align"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnalign_ :: MisoString -> Attribute action
columnalign_ = textProp "columnalign"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnlines_ :: MisoString -> Attribute action
columnlines_ = textProp "columnlines"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnspacing_ :: MisoString -> Attribute action
columnspacing_ = textProp "columnspacing"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
columnspan_ :: Int -> Attribute action
columnspan_ = intProp "columnspan"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
depth_ :: MisoString -> Attribute action
depth_ = textProp "depth"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
display_ :: MisoString -> Attribute action
display_ = textProp "display"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
fence_ :: Bool -> Attribute action
fence_ = boolProp "fence"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
frame_ :: MisoString -> Attribute action
frame_ = textProp "frame"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
framespacing_ :: MisoString -> Attribute action
framespacing_ = textProp "framespacing"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
height_ :: MisoString -> Attribute action
height_ = textProp "height"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
href_ :: MisoString -> Attribute action
href_ = textProp "href"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
id_ :: MisoString -> Attribute action
id_ = textProp "id"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
linethickness_ :: MisoString -> Attribute action
linethickness_ = textProp "linethickness"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
lspace_ :: MisoString -> Attribute action
lspace_ = textProp "lspace"
-- | @since 1.9.0.0
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathbackground_ :: MisoString -> Attribute action
mathbackground_ = textProp "mathbackground"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathcolor_ :: MisoString -> Attribute action
mathcolor_ = textProp "mathcolor"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathsize_ :: MisoString -> Attribute action
mathsize_ = textProp "mathsize"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
mathvariant_ :: MisoString -> Attribute action
mathvariant_ = textProp "mathvariant"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
maxsize_ :: MisoString -> Attribute action
maxsize_ = textProp "maxsize"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
minsize_ :: MisoString -> Attribute action
minsize_ = textProp "minsize"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
movablelimits_ :: Bool -> Attribute action
movablelimits_ = boolProp "movablelimits"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
notation_ :: MisoString -> Attribute action
notation_ = textProp "notation"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowalign_ :: MisoString -> Attribute action
rowalign_ = textProp "rowalign"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowlines_ :: MisoString -> Attribute action
rowlines_ = textProp "rowlines"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowspacing_ :: MisoString -> Attribute action
rowspacing_ = textProp "rowspacing"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rowspan_ :: Int -> Attribute action
rowspan_ = intProp "rowspan"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
rspace_ :: MisoString -> Attribute action
rspace_ = textProp "rspace"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
separator_ :: Bool -> Attribute action
separator_ = boolProp "separator"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
stretchy_ :: Bool -> Attribute action
stretchy_ = boolProp "stretchy"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
symmetric_ :: Bool -> Attribute action
symmetric_ = boolProp "symmetric"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
voffset_ :: MisoString -> Attribute action
voffset_ = textProp "voffset"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
width_ :: MisoString -> Attribute action
width_ = textProp "width"
-----------------------------------------------------------------------------
