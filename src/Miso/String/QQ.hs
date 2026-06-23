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
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.String.QQ" provides the 'misoString' quasi-quoter, which lets you
-- write multiline 'Miso.String.MisoString' literals with preserved
-- whitespace and indentation directly in Haskell source.
--
-- Enable the extension and import the quoter:
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import "Miso.String.QQ" ('misoString')
-- @
--
-- = Quick start
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import "Miso.String.QQ"
-- import "Miso.String" ('Miso.String.MisoString')
--
-- -- Multiline literal — newlines and indentation are preserved as-is
-- myCSS :: 'Miso.String.MisoString'
-- myCSS = ['misoString'|
--   body {
--     margin: 0;
--     font-family: sans-serif;
--   }
-- |]
--
-- -- Useful for injecting inline \<style\> or \<script\> content:
-- view :: Model -> 'Miso.Types.View' Model Action
-- view _ =
--   'Miso.Html.Element.div_' []
--     [ 'Miso.Html.Element.style_' [] [ 'Miso.text' myCSS ] ]
-- @
--
-- = How it works
--
-- The quasi-quoter is expression-only (@quoteExp@). At compile time it
-- splices @toMisoString \<the-literal-string\>@, so the result is
-- identical to writing @'Miso.String.ms' \"…\"@ but without needing to
-- escape newlines or quotes inside the brackets.
--
-- @quotePat@, @quoteType@, and @quoteDec@ are not implemented and will
-- produce a compile error if attempted.
--
-- = See also
--
-- * "Miso.FFI.QQ" — @[js| … |]@ quasi-quoter for inline JavaScript
-- * "Miso.String" — 'Miso.String.MisoString', 'Miso.String.ms', 'Miso.String.toMisoString'
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
