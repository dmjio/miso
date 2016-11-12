{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.String
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.String (
    MisoString
  , MisoVal
  , module Data.JSString
  , ToMisoString (..)
  ) where

import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.JSString
import           Data.JSString.Text
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

import           GHCJS.Types

type MisoVal = JSVal

-- | String type swappable based on compiler
type MisoString = JSString

-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)

class ToMisoString str where
  toMisoString :: str -> MisoString

instance ToMisoString MisoString where toMisoString = id
instance ToMisoString String where toMisoString = pack
instance ToMisoString T.Text where toMisoString = textToJSString
instance ToMisoString LT.Text where toMisoString = lazyTextToJSString
instance ToMisoString B.ByteString where toMisoString = toMisoString . T.decodeUtf8
instance ToMisoString BL.ByteString where toMisoString = toMisoString . LT.decodeUtf8
instance ToMisoString Value where
  toMisoString (String x) = toMisoString x
  toMisoString x = toMisoString . encode $ x

