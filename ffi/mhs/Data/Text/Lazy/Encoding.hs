-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lazy.Encoding
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Lazy text UTF-8 encoding for MicroHs, via the strict conversions.
-----------------------------------------------------------------------------
module Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8) where
-----------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
-----------------------------------------------------------------------------
encodeUtf8 :: LT.Text -> BL.ByteString
encodeUtf8 = BL.fromStrict . T.encodeUtf8 . LT.toStrict
-----------------------------------------------------------------------------
decodeUtf8 :: BL.ByteString -> LT.Text
decodeUtf8 = LT.fromStrict . T.decodeUtf8 . BL.toStrict
-----------------------------------------------------------------------------
