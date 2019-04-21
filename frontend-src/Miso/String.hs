{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.String
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String (
    ToMisoString (..)
  , MisoString
  , module Data.JSString
  , module Data.Monoid
  , ms
  ) where

#ifndef JSADDLE
import           Data.Aeson
#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.JSString
import           Data.JSString.Text
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import           Miso.FFI

-- | String type swappable based on compiler
type MisoString = JSString

#ifndef JSADDLE
-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)
#endif

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
  toMisoString = pack
  fromMisoString = unpack
instance ToMisoString T.Text where
  toMisoString = textToJSString
  fromMisoString = textFromJSString
instance ToMisoString LT.Text where
  toMisoString = lazyTextToJSString
  fromMisoString = lazyTextFromJSString
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
  fromMisoString = T.encodeUtf8 . fromMisoString
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
  fromMisoString = LT.encodeUtf8 . fromMisoString
instance ToMisoString Float where
  toMisoString = realFloatToJSString
  fromMisoString = realToFrac . jsStringToDouble
instance ToMisoString Double where
  toMisoString = realFloatToJSString
  fromMisoString = jsStringToDouble
instance ToMisoString Int where
  toMisoString = integralToJSString
  fromMisoString = round . jsStringToDouble
instance ToMisoString Word where
  toMisoString = integralToJSString
  fromMisoString = round . jsStringToDouble
