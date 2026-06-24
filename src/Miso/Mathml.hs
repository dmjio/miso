-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Mathml
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- [MathML](https://developer.mozilla.org/en-US/docs/Web/MathML) element
-- combinators for rendering mathematical expressions in Miso views.
-- Re-exports everything from "Miso.Mathml.Element".
--
-- __Example__ — render /x²/:
--
-- @
-- xSquared :: View model action
-- xSquared =
--   math_ []
--     [ msup_ []
--         [ mi_ [] [ text \"x\" ]
--         , mn_ [] [ text \"2\" ]
--         ]
--     ]
-- @
--
-- For a more complete example see
-- [miso-mathml](https://github.com/haskell-miso/miso-mathml).
--
----------------------------------------------------------------------------
module Miso.Mathml
   ( -- * Elements
     module Miso.Mathml.Element
   ) where
-----------------------------------------------------------------------------
import Miso.Mathml.Element
-----------------------------------------------------------------------------
