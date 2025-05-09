-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Mathml.Element
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Mathml.Element
  ( -- ** Combinator
    nodeMathml
   -- ** Elements
  , math_
  , annotationXml_
  , annotation_
  , merror_
  , mfrac_
  , mi_
  , mmultiscripts_
  , mn_
  , mo_
  , mover_
  , mpadded_
  , mphantom_
  , mprescripts_
  , mroot_
  , mrow_
  , ms_
  , mspace_
  , msqrt_
  , mstyle_
  , msub_
  , msubsup_
  , msup_
  , mtable_
  , mtd_
  , mtext_
  , mtr_
  , munder_
  , munderover_
  , semantics_
  ) where
-----------------------------------------------------------------------------
import           Miso.Html.Types
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Low-level used to construct @Node@ in @View@.
-- Most View helpers in this module are defined in terms of it.
nodeMathml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeMathml nodeName = node MATHML nodeName Nothing
-----------------------------------------------------------------------------
-- | [\<annotation-xml\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/annotation-xml)
--
-- @since 1.9.0.0
annotationXml_ :: [Attribute action] -> [View action] -> View action
annotationXml_ = nodeMathml "annotation-xml"
-----------------------------------------------------------------------------
-- | [\<annotation\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/annotation)
--
-- @since 1.9.0.0
annotation_ :: [Attribute action] -> [View action] -> View action
annotation_ = nodeMathml "annotation"
-----------------------------------------------------------------------------
-- | [\<math\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/math)
--
-- @since 1.9.0.0
math_ :: [Attribute action] -> [View action] -> View action
math_ = nodeMathml "math"
-----------------------------------------------------------------------------
-- | [\<merror\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/merror)
--
-- @since 1.9.0.0
merror_ :: [Attribute action] -> [View action] -> View action
merror_ = nodeMathml "merror"
-----------------------------------------------------------------------------
-- | [\<mfrac\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mfrac)
--
-- @since 1.9.0.0
mfrac_ :: [Attribute action] -> [View action] -> View action
mfrac_ = nodeMathml "mfrac"
-----------------------------------------------------------------------------
-- | [\<mi\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mi)
--
-- @since 1.9.0.0
mi_ :: [Attribute action] -> [View action] -> View action
mi_ = nodeMathml "mi"
-----------------------------------------------------------------------------
-- | [\<mmultiscripts\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mmultiscripts)
--
-- @since 1.9.0.0
mmultiscripts_ :: [Attribute action] -> [View action] -> View action
mmultiscripts_ = nodeMathml "mmultiscripts"
-----------------------------------------------------------------------------
-- | [\<mn\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mn)
--
-- @since 1.9.0.0
mn_ :: [Attribute action] -> [View action] -> View action
mn_ = nodeMathml "mn"
-----------------------------------------------------------------------------
-- | [\<mo\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mo)
--
-- @since 1.9.0.0
mo_ :: [Attribute action] -> [View action] -> View action
mo_ = nodeMathml "mo"
-----------------------------------------------------------------------------
-- | [\<mover\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mover)
--
-- @since 1.9.0.0
mover_ :: [Attribute action] -> [View action] -> View action
mover_ = nodeMathml "mover"
-----------------------------------------------------------------------------
-- | [\<mpadded\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mpadded)
--
-- @since 1.9.0.0
mpadded_ :: [Attribute action] -> [View action] -> View action
mpadded_ = nodeMathml "mpadded"
-----------------------------------------------------------------------------
-- | [\<mphantom\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mphantom)
--
-- @since 1.9.0.0
mphantom_ :: [Attribute action] -> [View action] -> View action
mphantom_ = nodeMathml "mphantom"
-----------------------------------------------------------------------------
-- | [\<mprescripts\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mprescripts)
--
-- @since 1.9.0.0
mprescripts_ :: [Attribute action] -> [View action] -> View action
mprescripts_ = nodeMathml "mprescripts"
-----------------------------------------------------------------------------
-- | [\<mroot\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mroot)
--
-- @since 1.9.0.0
mroot_ :: [Attribute action] -> [View action] -> View action
mroot_ = nodeMathml "mroot"
-----------------------------------------------------------------------------
-- | [\<mrow\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mrow)
--
-- @since 1.9.0.0
mrow_ :: [Attribute action] -> [View action] -> View action
mrow_ = nodeMathml "mrow"
-----------------------------------------------------------------------------
-- | [\<ms\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/ms)
--
-- @since 1.9.0.0
ms_ :: [Attribute action] -> [View action] -> View action
ms_ = nodeMathml "ms"
-----------------------------------------------------------------------------
-- | [\<mspace\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mspace)
--
-- @since 1.9.0.0
mspace_ :: [Attribute action] -> [View action] -> View action
mspace_ = nodeMathml "mspace"
-----------------------------------------------------------------------------
-- | [\<msqrt\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/msqrt)
--
-- @since 1.9.0.0
msqrt_ :: [Attribute action] -> [View action] -> View action
msqrt_ = nodeMathml "msqrt"
-----------------------------------------------------------------------------
-- | [\<mstyle\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mstyle)
--
-- @since 1.9.0.0
mstyle_ :: [Attribute action] -> [View action] -> View action
mstyle_ = nodeMathml "mstyle"
-----------------------------------------------------------------------------
-- | [\<msub\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/msub)
--
-- @since 1.9.0.0
msub_ :: [Attribute action] -> [View action] -> View action
msub_ = nodeMathml "msub"
-----------------------------------------------------------------------------
-- | [\<msubsup\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/msubsup)
--
-- @since 1.9.0.0
msubsup_ :: [Attribute action] -> [View action] -> View action
msubsup_ = nodeMathml "msubsup"
-----------------------------------------------------------------------------
-- | [\<msup\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/msup)
--
-- @since 1.9.0.0
msup_ :: [Attribute action] -> [View action] -> View action
msup_ = nodeMathml "msup"
-----------------------------------------------------------------------------
-- | [\<mtable\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mtable)
--
-- @since 1.9.0.0
mtable_ :: [Attribute action] -> [View action] -> View action
mtable_ = nodeMathml "mtable"
-----------------------------------------------------------------------------
-- | [\<mtd\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mtd)
--
-- @since 1.9.0.0
mtd_ :: [Attribute action] -> [View action] -> View action
mtd_ = nodeMathml "mtd"
-----------------------------------------------------------------------------
-- | [\<mtext\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mtext)
--
-- @since 1.9.0.0
mtext_ :: [Attribute action] -> [View action] -> View action
mtext_ = nodeMathml "mtext"
-----------------------------------------------------------------------------
-- | [\<mtr\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/mtr)
--
-- @since 1.9.0.0
mtr_ :: [Attribute action] -> [View action] -> View action
mtr_ = nodeMathml "mtr"
-----------------------------------------------------------------------------
-- | [\<munder\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/munder)
--
-- @since 1.9.0.0
munder_ :: [Attribute action] -> [View action] -> View action
munder_ = nodeMathml "munder"
-----------------------------------------------------------------------------
-- | [\<munderover\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/munderover)
--
-- @since 1.9.0.0
munderover_ :: [Attribute action] -> [View action] -> View action
munderover_ = nodeMathml "munderover"
-----------------------------------------------------------------------------
-- | [\<semantics\>](https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/semantics)
--
-- @since 1.9.0.0
semantics_ :: [Attribute action] -> [View action] -> View action
semantics_ = nodeMathml "semantics"
-----------------------------------------------------------------------------
