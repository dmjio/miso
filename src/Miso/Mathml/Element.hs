{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  ) where

import           Miso.Html.Types
import           Miso.String (MisoString)
import qualified Prelude            as P

-- | Used to construct `VNode`'s in `View`
nodeMathml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeMathml = P.flip (node MATHML) P.Nothing
