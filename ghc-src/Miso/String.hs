{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.String
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String
  ( ToMisoString (..)
  , MisoString
  , module Data.Text
  ) where

import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Text
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | String type swappable based on compiler
type MisoString = Text

-- | Convenience class for creating `MisoString` from other string-like types
class ToMisoString str where toMisoString :: str -> MisoString
instance ToMisoString MisoString where toMisoString = id
instance ToMisoString String where toMisoString = T.pack
instance ToMisoString LT.Text where toMisoString = LT.toStrict
instance ToMisoString B.ByteString where toMisoString = toMisoString . T.decodeUtf8
instance ToMisoString BL.ByteString where toMisoString = toMisoString . LT.decodeUtf8
instance ToMisoString Value where toMisoString = toMisoString . encode
