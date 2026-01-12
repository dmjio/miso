-----------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskellQuotes     #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.String.QQ
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String.QQ
  ( misoString
  ) where
----------------------------------------------------------------------------
import Language.Haskell.TH.Quote
----------------------------------------------------------------------------
import Miso.String (toMisoString)
----------------------------------------------------------------------------
-- | QuasiQuoter for specifying multiline 'Miso.String.MisoString'
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
--
-- test :: MisoString
-- test = [misoString| foo
--   bar
--     baz
-- |]
--
-- @
--
misoString :: QuasiQuoter
misoString = QuasiQuoter
  { quoteExp  = \string -> [| toMisoString string |]
  , quotePat  = \_ -> fail "quotePat: not implemented"
  , quoteType = \_ -> fail "quoteType: not implemented"
  , quoteDec  = \_ -> fail "quoteDec: not implemented"
  }
----------------------------------------------------------------------------
