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
  , math_
  ) where
-----------------------------------------------------------------------------
import           Miso.Html.Types
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Used to construct @Node@ in @View@
nodeMathml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeMathml nodeName = node MATHML nodeName Nothing
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/MathML/Reference/Element/math
math_ :: [Attribute action] -> [View action] -> View action
math_ = nodeMathml "math"
-----------------------------------------------------------------------------
