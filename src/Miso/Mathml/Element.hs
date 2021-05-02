{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Mathml.Element
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Mathml.Element
  ( -- * Construct an Element
      nodeMathml
  ) where

import           Miso.Html.Types
import           Miso.String (MisoString)
import qualified Prelude            as P

-- | Used to construct `VNode`'s in `View`
nodeMathml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeMathml = P.flip (node MATHML) P.Nothing
