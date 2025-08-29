----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.JSON
  ( -- ** Types
    Value (..)
  , Object (..)
  , Result (..)
    -- ** Classes
  , ToJSON (..)
  , FromJSON (..)
  , ToJSONKey
  , FromJSONKey
    -- ** Functions
  , parseEither
  , fromJSON
  , withText
  ) where
-----------------------------------------------------------------------------
import Language.Javascript.JSaddle hiding (Result, Object)
-----------------------------------------------------------------------------
import Miso.String
-----------------------------------------------------------------------------
newtype Value = Value JSVal
-----------------------------------------------------------------------------
newtype Object = Object JSVal
-----------------------------------------------------------------------------
type Parser value = JSM value
-----------------------------------------------------------------------------
class FromJSON value where
  parseJSON :: Value -> Parser value
-----------------------------------------------------------------------------
class ToJSON value where
  toJSON :: value -> Value
-----------------------------------------------------------------------------
data Result a
  = Success a
  | Error MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
runParser :: JSM a -> Either MisoString a
runParser = undefined
-----------------------------------------------------------------------------
parseEither
  :: (a -> Parser value)
  -> a
  -> Either MisoString value
parseEither f x = runParser (f x)
-----------------------------------------------------------------------------
fromJSON
  :: FromJSON a
  => Value
  -> Result a
fromJSON = undefined
-----------------------------------------------------------------------------
withText
  :: MisoString
  -> (MisoString -> Parser a)
  -> Value
  -> Parser a
withText = undefined
-----------------------------------------------------------------------------
instance ToJSON MisoString where
  toJSON = undefined
-----------------------------------------------------------------------------
instance ToJSON Bool where
  toJSON = undefined
-----------------------------------------------------------------------------
instance ToJSON Object where
  toJSON = undefined
-----------------------------------------------------------------------------
instance ToJSON Int where
  toJSON = undefined
-----------------------------------------------------------------------------
instance ToJSON Integer where
  toJSON = undefined
-----------------------------------------------------------------------------
instance ToJSON Double where
  toJSON = undefined
-----------------------------------------------------------------------------
instance ToJSON String where
  toJSON = undefined
-----------------------------------------------------------------------------
class FromJSONKey key
class ToJSONKey key
-----------------------------------------------------------------------------
instance ToJSVal Value where
  toJSVal (Value v) = pure v
-----------------------------------------------------------------------------
instance FromJSVal Value where
  fromJSVal x = pure (Just (Value x))
-----------------------------------------------------------------------------

