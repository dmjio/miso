-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- 'Miso.String.MisoString'. Its API mirrors @Data.UUID@ from the
-- <https://hackage.haskell.org/package/uuid uuid> package.
--
-- New v4 UUIDs are generated with the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Crypto/randomUUID crypto.randomUUID()>.
-- Parsing accepts the canonical @8-4-4-4-12@ hexadecimal form, in either
-- case, and normalizes it to lowercase.
--
-- = Quick start
--
-- @
-- import "Miso.UUID" (t'UUID')
-- import qualified "Miso.UUID" as UUID
--
-- -- Generate a fresh identifier
-- freshId :: IO t'UUID'
-- freshId = UUID.'nextRandom'
--
-- -- Parse one
-- parsed :: Maybe t'UUID'
-- parsed = UUID.'fromString' \"550e8400-e29b-41d4-a716-446655440000\"
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
-- Its 'Show' and 'Read' instances use the unquoted @8-4-4-4-12@ form,
-- like @Data.UUID@.
----------------------------------------------------------------------------
module Miso.UUID
  ( -- ** Types
    UUID
    -- ** String conversion
  , toString
  , fromString
  , toText
  , fromText
  , toASCIIBytes
  , fromASCIIBytes
  , toLazyASCIIBytes
  , fromLazyASCIIBytes
    -- ** Binary conversion
  , toByteString
  , fromByteString
  , toWords
  , fromWords
  , toWords64
  , fromWords64
    -- ** Nil
  , null
  , nil
    -- ** Generation
  , nextRandom
  ) where
-----------------------------------------------------------------------------
import           Control.Monad ((<=<))
import           Data.Bifunctor (first)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char (digitToInt, intToDigit, isHexDigit, isSpace, toLower)
import           Data.List (intercalate)
import qualified Data.List as List
import qualified Data.Text as T
import           Data.Word (Word32, Word64)
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
  deriving (Eq, Ord)
-----------------------------------------------------------------------------
instance Show UUID where
  showsPrec _ = showString . toString
-----------------------------------------------------------------------------
instance Read UUID where
  readsPrec _ str =
    case fromString (take 36 s) of
      Nothing -> []
      Just u -> [(u, drop 36 s)]
    where
      s = dropWhile isSpace str
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
-- | Convert a t'UUID' to its @8-4-4-4-12@ string form, in lowercase.
toString :: UUID -> String
toString = fromMisoString . toMisoString
-----------------------------------------------------------------------------
-- | Parse a t'UUID' from its @8-4-4-4-12@ string form.
fromString :: String -> Maybe UUID
fromString = either (const Nothing) Just . parse . toMisoString
-----------------------------------------------------------------------------
-- | Like 'toString', but to t'T.Text'.
toText :: UUID -> T.Text
toText = fromMisoString . toMisoString
-----------------------------------------------------------------------------
-- | Like 'fromString', but from t'T.Text'.
fromText :: T.Text -> Maybe UUID
fromText = fromString . T.unpack
-----------------------------------------------------------------------------
-- | Like 'toString', but to an ASCII-encoded strict 'B8.ByteString'.
toASCIIBytes :: UUID -> B8.ByteString
toASCIIBytes = B8.pack . toString
-----------------------------------------------------------------------------
-- | Like 'fromString', but from an ASCII-encoded strict 'B8.ByteString'.
fromASCIIBytes :: B8.ByteString -> Maybe UUID
fromASCIIBytes = fromString . B8.unpack
-----------------------------------------------------------------------------
-- | Like 'toString', but to an ASCII-encoded lazy 'BL.ByteString'.
toLazyASCIIBytes :: UUID -> BL.ByteString
toLazyASCIIBytes = BL8.pack . toString
-----------------------------------------------------------------------------
-- | Like 'fromString', but from an ASCII-encoded lazy 'BL.ByteString'.
fromLazyASCIIBytes :: BL.ByteString -> Maybe UUID
fromLazyASCIIBytes = fromString . BL8.unpack
-----------------------------------------------------------------------------
-- | Encode a t'UUID' as 16 bytes, in network byte order.
toByteString :: UUID -> BL.ByteString
toByteString u =
  BL.pack [ fromIntegral (w `shiftR` n) | w <- [hi, lo], n <- [56, 48 .. 0] ]
    where
      (hi, lo) = toWords64 u
-----------------------------------------------------------------------------
-- | Decode a t'UUID' from 16 bytes, in network byte order. Returns
-- 'Nothing' if the input is not exactly 16 bytes long.
fromByteString :: BL.ByteString -> Maybe UUID
fromByteString bs
  | length bytes == 16 = Just (fromWords64 (word hi) (word lo))
  | otherwise = Nothing
    where
      bytes = BL.unpack bs
      (hi, lo) = splitAt 8 bytes
      word = List.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0
-----------------------------------------------------------------------------
-- | Convert a t'UUID' to four 32-bit words, most significant first.
toWords :: UUID -> (Word32, Word32, Word32, Word32)
toWords u =
  ( fromIntegral (hi `shiftR` 32)
  , fromIntegral hi
  , fromIntegral (lo `shiftR` 32)
  , fromIntegral lo
  ) where
      (hi, lo) = toWords64 u
-----------------------------------------------------------------------------
-- | Build a t'UUID' from four 32-bit words, most significant first.
fromWords :: Word32 -> Word32 -> Word32 -> Word32 -> UUID
fromWords a b c d = fromWords64 (join a b) (join c d)
  where
    join x y = fromIntegral x `shiftL` 32 .|. fromIntegral y
-----------------------------------------------------------------------------
-- | Convert a t'UUID' to two 64-bit words, most significant first.
toWords64 :: UUID -> (Word64, Word64)
toWords64 u = (word hi, word lo)
  where
    (hi, lo) = splitAt 16 (filter (/= '-') (toString u))
    word = List.foldl' (\acc c -> acc `shiftL` 4 .|. fromIntegral (digitToInt c)) 0
-----------------------------------------------------------------------------
-- | Build a t'UUID' from two 64-bit words, most significant first.
fromWords64 :: Word64 -> Word64 -> UUID
fromWords64 hi lo = UUID $ toMisoString $ intercalate "-" [a, b, c, d, e]
  where
    hex w = [ intToDigit (fromIntegral (w `shiftR` n .&. 0xf)) | n <- [60, 56 .. 0] ]
    (a, r1) = splitAt 8 (hex hi <> hex lo)
    (b, r2) = splitAt 4 r1
    (c, r3) = splitAt 4 r2
    (d, e) = splitAt 4 r3
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
nextRandom :: IO UUID
nextRandom = UUID <$> (fromJSValUnchecked =<< (jsg "crypto" # "randomUUID") ())
-----------------------------------------------------------------------------
