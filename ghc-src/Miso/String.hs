{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.String
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String
  ( ToMisoString (..)
  , MisoString
  , module Data.Monoid
  , module Data.Text
  , ms
  ) where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Monoid
import           Data.Text
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | String type swappable based on compiler
type MisoString = Text

-- | Convenience class for creating `MisoString` from other string-like types
class ToMisoString str where
  toMisoString :: str -> MisoString
  fromMisoString :: MisoString -> str

-- | Convenience function, shorthand for `toMisoString`
ms :: ToMisoString str => str -> MisoString
ms = toMisoString

instance ToMisoString MisoString where
  toMisoString = id
  fromMisoString = id
instance ToMisoString String where
  toMisoString = T.pack
  fromMisoString = T.unpack
instance ToMisoString LT.Text where
  toMisoString = LT.toStrict
  fromMisoString = LT.fromStrict
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
  fromMisoString = T.encodeUtf8 . fromMisoString
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
  fromMisoString = LT.encodeUtf8 . fromMisoString
instance ToMisoString Float where
  toMisoString = T.pack . show
  fromMisoString = read . T.unpack
instance ToMisoString Double where
  toMisoString = T.pack . show
  fromMisoString = read . T.unpack
instance ToMisoString Int where
  toMisoString = T.pack . show
  -- Replicate frontend behavior
  fromMisoString = round . read @Double . T.unpack
instance ToMisoString Word where
  toMisoString = T.pack . show
  -- Replicate frontend behavior
  fromMisoString = round . read @Double . T.unpack
