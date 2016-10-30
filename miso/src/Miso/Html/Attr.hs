{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Attr
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes>
--
----------------------------------------------------------------------------
module Miso.Html.Attr where

import           Miso.Html.Types ( Attribute ( A )  )
import           Miso.Html.Internal ( MisoString )

-- | Used to construct HTML Attributes
attr :: MisoString -> MisoString -> Attribute action
attr = A
