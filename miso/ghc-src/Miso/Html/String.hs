{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.String
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.String ( MisoString, misoPack, misoIntercalate, MisoVal ) where

import Data.Text

type MisoVal = ()

-- | String type swappable based on compiler
type MisoString = Text

-- | Pack specialized for miso
misoPack :: String -> Text
misoPack = pack

-- | intercalate specialized for `MisoString`
misoIntercalate :: MisoString -> [MisoString] -> MisoString
misoIntercalate = intercalate
