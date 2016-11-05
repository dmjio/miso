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
  , misoPack
  , misoIntercalate
  , MisoVal
  ) where

import Data.Aeson
import Data.JSString
import Data.JSString.Text
import GHCJS.Marshal
import GHCJS.Types
import System.IO.Unsafe

import Miso.FFI

type MisoVal = JSVal

-- | String type swappable based on compiler
type MisoString = JSString

-- | pack specialized for `MisoString`
misoPack :: String -> MisoString
misoPack = pack

-- | intercalate specialized for `MisoString`
misoIntercalate :: MisoString -> [MisoString] -> MisoString
misoIntercalate = intercalate

-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON x = pure $ unsafePerformIO $ stringify' =<< toJSVal x
