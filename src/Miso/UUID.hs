-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.UUID
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.UUID" provides a t'UUID' type, as defined in
-- <http://tools.ietf.org/html/rfc4122 RFC 4122>, backed by a
-- 'Miso.String.MisoString'.
--
-- New v4 UUIDs are generated with the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Crypto/randomUUID crypto.randomUUID()>.
-- Parsing (via 'Miso.String.fromMisoStringEither', 'Read' or JSON)
-- accepts the canonical @8-4-4-4-12@ hexadecimal form and normalizes it
-- to lowercase.
--
-- = Quick start
--
-- @
-- import "Miso.UUID" (t'UUID')
-- import qualified "Miso.UUID" as UUID
--
-- -- Generate a fresh identifier
-- freshId :: IO t'UUID'
-- freshId = UUID.'newV4'
--
-- -- Check for the nil UUID
-- isUnset :: t'UUID' -> Bool
-- isUnset = UUID.'null'
-- @
--
-- = Instances
--
-- t'UUID' can be converted to and from 'Miso.String.MisoString', JSON,
-- 'Miso.DSL.JSVal', and used as a route capture with "Miso.Router".
----------------------------------------------------------------------------
module Miso.UUID
  ( -- ** Types
    UUID
    -- ** Functions
  , nil
  , null
  , newV4
  ) where
-----------------------------------------------------------------------------
import           Control.Monad ((<=<))
import           Data.Bifunctor (first)
import           Data.Bool (bool)
import           Data.Char (isHexDigit, toLower)
-----------------------------------------------------------------------------
import           Miso.JSON (FromJSON (..), ToJSON (..))
import qualified Miso.JSON as JSON
import           Miso.Prelude hiding (null)
import           Miso.Router (Router (..), capture, toPath)
import           Miso.String (FromMisoString (..), ToMisoString (..))
import           Miso.Util.Parser (ParseError)
import qualified Miso.Util.Parser as Parser
-----------------------------------------------------------------------------
-- | A universally unique identifier, as defined in <http://tools.ietf.org/html/rfc4122 RFC 4122>.
newtype UUID = UUID MisoString
  deriving newtype (Eq, Ord, Show)
-----------------------------------------------------------------------------
instance Read UUID where
  readsPrec d r = do
    (s, t) <- readsPrec @String d r
    Right u <- [fromMisoStringEither . toMisoString $ s]
    pure (u, t)
-----------------------------------------------------------------------------
parse :: MisoString -> Either (ParseError UUID Char) UUID
parse =
  Parser.parse
    ( UUID
        . toMisoString
        . fmap toLower
        <$> traverse (Parser.satisfy . bool isHexDigit (== '-') . (== '-')) nilPattern
        <* Parser.endOfInput
    )
    . fromMisoString
-----------------------------------------------------------------------------
nilPattern :: String
nilPattern = "00000000-0000-0000-0000-000000000000"
-----------------------------------------------------------------------------
-- | The nil UUID, as defined in <http://tools.ietf.org/html/rfc4122 RFC 4122>.
--  It is a UUID of all zeros.
nil :: UUID
nil = UUID $ toMisoString nilPattern
-----------------------------------------------------------------------------
-- | Returns True if the passed-in UUID is the 'nil' UUID.
null :: UUID -> Bool
null = (== nil)
-----------------------------------------------------------------------------
instance ToMisoString UUID where
  toMisoString (UUID s) = s
-----------------------------------------------------------------------------
instance FromMisoString UUID where
  fromMisoStringEither = first show . parse
-----------------------------------------------------------------------------
instance ToJSON UUID where
  toJSON = toJSON . toMisoString
-----------------------------------------------------------------------------
instance FromJSON UUID where
  parseJSON = either fail pure . fromMisoStringEither <=< parseJSON
-----------------------------------------------------------------------------
instance ToJSVal UUID where
  toJSVal = toJSVal . toMisoString
-----------------------------------------------------------------------------
instance FromJSVal UUID where
  fromJSVal = fmap (JSON.parseMaybe parseJSON =<<) . fromJSVal
-----------------------------------------------------------------------------
instance Router UUID where
  fromRoute = pure . toPath . toMisoString
  routeParser = capture
-----------------------------------------------------------------------------
-- | Generate a v4 'UUID' using a cryptographically secure random number generator.
newV4 :: IO UUID
newV4 = UUID <$> (fromJSValUnchecked =<< (jsg "crypto" # "randomUUID") ())
-----------------------------------------------------------------------------
