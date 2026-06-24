-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Prelude
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Prelude" is a batteries-included custom prelude for miso
-- applications. It re-exports:
--
-- * The entirety of "Miso" — all view, update, and subscription
--   combinators are available without a qualified import.
-- * The standard 'Prelude' — familiar Haskell functions remain in scope,
--   with @'(!!)'@ hidden to avoid the clash with miso's index operator.
-- * @'Control.Category.(.)'@ — replaces 'Prelude.(.)' so it works for any
--   'Control.Category.Category', not just @(->)@.
--
-- = Usage
--
-- __Option 1 — explicit import__ (simplest):
--
-- @
-- import "Miso.Prelude"
-- @
--
-- __Option 2 — Cabal mixin__ (replaces @Prelude@ project-wide, zero
-- per-file boilerplate):
--
-- @
-- executable app
--   main-is:         Main.hs
--   build-depends:   base, miso
--   mixins:
--     miso,
--     miso (Miso.Prelude as Prelude),
--     base hiding (Prelude)
--   default-language: Haskell2010
-- @
--
-- = See also
--
-- * "Miso" — the main miso re-export hub
-- * "Miso.Effect" — 'Miso.Effect.Effect', 'Miso.Effect.Sub', 'Miso.Effect.io_'
-- * "Miso.Html" — HTML element and event combinators
-----------------------------------------------------------------------------
module Miso.Prelude
  ( module Miso
  , module Prelude
  , (.)
  ) where
----------------------------------------------------------------------------
import Control.Category ((.))
import Prelude hiding ((.), (!!))
import Miso
----------------------------------------------------------------------------
