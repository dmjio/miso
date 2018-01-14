-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- import Miso
-- import Miso.Svg
--
-- intView :: Int -> View IntAction
-- intView n = svg_ [ height_ "100", width "100" ] [
--    circle_ [ cx_ "50", cy_ "50", r_ "40", stroke_ "green", strokeWidth_ "4", fill_ "yellow" ] []
--  ]
-- @
--
-- More information on how to use `miso` is available on GitHub
--
-- <http://github.com/dmjio/miso>
--
----------------------------------------------------------------------------
module Miso.Svg
   ( module Miso.Svg.Element
   , module Miso.Svg.Attribute
   , module Miso.Svg.Event
   ) where

import Miso.Svg.Attribute hiding ( filter_, path_, title_, mask_
                               , glyphRef_, clipPath_, colorProfile_
                               , cursor_, style_ )
import Miso.Svg.Element
import Miso.Svg.Event
