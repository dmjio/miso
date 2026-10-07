{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Miso.UUID (UUID, nil, null, newV4) where

import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Char (isHexDigit, toLower)
import Miso.JSON (FromJSON (..), ToJSON (..))
import Miso.JSON qualified as JSON
import Miso.Prelude hiding (null)
import Miso.Router (Router (..), capture, toPath)
import Miso.String (FromMisoString (..), ToMisoString (..))
import Miso.Util.Parser (ParseError)
import Miso.Util.Parser qualified as Parser

-- | A universally unique identifier, as defined in <http://tools.ietf.org/html/rfc4122 RFC 4122>.
newtype UUID = UUID MisoString
    deriving newtype (Eq, Ord, Show)

instance Read UUID where
    readsPrec d r = do
        (s, t) <- readsPrec @String d r
        Right u <- [fromMisoStringEither . toMisoString $ s]
        pure (u, t)

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

nilPattern :: String
nilPattern = "00000000-0000-0000-0000-000000000000"

-- | The nil UUID, as defined in <http://tools.ietf.org/html/rfc4122 RFC 4122>.
--  It is a UUID of all zeros.
nil :: UUID
nil = UUID $ toMisoString nilPattern

-- | Returns True if the passed-in UUID is the 'nil' UUID.
null :: UUID -> Bool
null = (== nil)

instance ToMisoString UUID where
    toMisoString (UUID s) = s

instance FromMisoString UUID where
    fromMisoStringEither = first show . parse

instance ToJSON UUID where
    toJSON = toJSON . toMisoString

instance FromJSON UUID where
    parseJSON = either fail pure . fromMisoStringEither <=< parseJSON

instance ToJSVal UUID where
    toJSVal = toJSVal . toMisoString

instance FromJSVal UUID where
    fromJSVal = fmap (JSON.parseMaybe parseJSON =<<) . fromJSVal

instance Router UUID where
    fromRoute = pure . toPath . toMisoString
    routeParser = capture

-- | Generate a v4 'UUID' using a cryptographically secure random number generator.
newV4 :: IO UUID
newV4 = UUID <$> (fromJSValUnchecked =<< (jsg "crypto" # "randomUUID") ())
