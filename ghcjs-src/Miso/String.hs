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

import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.JSString
import           Data.JSString.Int
import           Data.JSString.RealFloat
import           Data.JSString.Text
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           GHCJS.Marshal.Pure
import           GHCJS.Types

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
  toMisoString = realFloat
  fromMisoString = pFromJSVal . toJSNumber
instance ToMisoString Double where
  toMisoString = realFloat
  fromMisoString = pFromJSVal . toJSNumber
instance ToMisoString Int where
  toMisoString = decimal
  fromMisoString = pFromJSVal . toJSNumber
instance ToMisoString Word where
  toMisoString = decimal
  fromMisoString = pFromJSVal . toJSNumber

foreign import javascript unsafe "$r = Number($1);"
  toJSNumber :: JSString -> JSVal
