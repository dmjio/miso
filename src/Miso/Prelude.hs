-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Prelude
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A custom Prelude for [miso](https://github.com/dmjio/miso).
--
-- Can consume via @NoImplicitPrelude@ or using Cabal mixins.
--
-- @
-- import Miso.Prelude
-- @
--
-- Almost identical to the existing @Prelude@, except we re-export the
-- @Miso@ module, along with @Control.Category@.
--
-- @
-- executable app
--   import:
--     options
--   main-is:
--     Main.hs
--   build-depends:
--     base, miso
--   mixins:
--     miso,
--     miso (Miso.Prelude as Prelude),
--     base hiding (Prelude)
--   default-language:
--     Haskell2010
-- @
--
----------------------------------------------------------------------------
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
