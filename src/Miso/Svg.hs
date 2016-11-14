-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- import Miso.Svg
-- import qualified Data.Text as T
--
-- intView :: Int -> View IntAction
-- intView n = svg_ [ height_ "100", width "100" ] [
--    circle_ [ cx "50", cy "50", r "40", stroke "green", stroke-width "4", fill "yellow" ] []
--  ]
-- @
--
-- More information on how to use `miso` and `miso-html` is available on the miso wiki:
--
-- <http://github.com/haskell-miso/miso/wiki>
--
----------------------------------------------------------------------------
module Miso.Svg
   ( module Miso.Svg.Combinator
   , module Miso.Svg.Attributes
   , module Miso.Svg.Events
   ) where

import Miso.Svg.Attributes
import Miso.Svg.Combinator hiding ( filter, path, title, mask
                                  , glyphRef, clipPath, colorProfile
                                  , cursor, style )
import Miso.Svg.Events
