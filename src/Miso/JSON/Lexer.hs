----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON.Lexer
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.JSON.Lexer" is the first stage of miso's pure Haskell JSON pipeline,
-- which is used for server-side rendering (SSR). It tokenises a
-- 'Miso.String.MisoString' into a stream of 'Token' values consumed by
-- "Miso.JSON.Parser".
--
-- This module is __internal__. Application code should use "Miso.JSON" or
-- "Miso.JSON.Parser" ('Miso.JSON.Parser.decodePure') instead.
--
-- This module was ported from <https://github.com/dmjio/json-test> by
-- <https://github.com/ners @ners>.
--
-- = Token types
--
-- @
-- data 'Token'
--   = 'TokenPunctuator' Char    -- one of @[ ] { } , :@
--   | 'TokenNumber'     Double  -- JSON number (integer or floating-point)
--   | 'TokenBool'       Bool    -- @true@ or @false@
--   | 'TokenString'     'Miso.String.MisoString' -- quoted string with escape sequences
--   | 'TokenNull'               -- @null@
-- @
--
-- String tokens handle all
-- <https://www.rfc-editor.org/rfc/rfc8259#section-7 RFC 8259 escape sequences>
-- including @\\uXXXX@ and UTF-16 surrogate pairs (@\\uD800\\uDC00@).
--
-- = See also
--
-- * "Miso.JSON.Parser" — consumes 'Token' streams produced here
-- * "Miso.JSON.Types" — 'Miso.JSON.Types.Value' produced by the parser
-- * "Miso.Util.Lexer" — the underlying 'Miso.Util.Lexer.Lexer' combinator library
----------------------------------------------------------------------------
module Miso.JSON.Lexer (Token (..), tokens) where
----------------------------------------------------------------------------
import           Control.Applicative (Alternative (some, many), optional)
import           Control.Monad (replicateM)
import           Data.Char (isHexDigit, chr, isSpace)
import           Data.Foldable (Foldable (fold))
import           Data.Functor (void)
import           Data.Ix (Ix (inRange))
import           Data.Maybe (catMaybes)
import           Numeric (readHex)
import           Prelude hiding (null)
----------------------------------------------------------------------------
import           Miso.String (fromMisoString, ToMisoString (toMisoString), MisoString)
import           Miso.Util (oneOf)
import           Miso.Util.Lexer hiding (string', token)
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import Control.Applicative (liftA2)
#endif
----------------------------------------------------------------------------
data Token
  = TokenPunctuator Char
  | TokenNumber Double
  | TokenBool Bool
  | TokenString MisoString
  | TokenNull
  deriving (Eq, Show)
----------------------------------------------------------------------------
number :: Lexer Double
number = fromMisoString . fold . catMaybes <$> sequence
  [ optional $ string "-"
  , Just <$> int
  , optional $ liftA2 (<>) (string ".") int
  , optional $ liftA2 (<>) (oneOf $ string <$> ["e", "e+", "e-", "E", "E+", "E-"]) int
  ] where
      digit = satisfy $ inRange ('0', '9')
      int = toMisoString <$> some digit
----------------------------------------------------------------------------
bool :: Lexer Bool
bool = oneOf
  [ False <$ string "false"
  , True <$ string "true"
  ]
----------------------------------------------------------------------------
string' :: Lexer MisoString
string' = char '"' *> (toMisoString <$> many character) <* char '"'
  where
    character = oneOf
      [ satisfy $ \c -> c /= '"' && c /= '\\'
      , escapedCharacter
      ]
    hexDigit = satisfy isHexDigit
    escaped = (char '\\' *>)
    escapedCharacter = escaped $ oneOf
      [ char '"'
      , char '\\'
      , char '/'
      , '\b' <$ char 'b'
      , '\f' <$ char 'f'
      , '\n' <$ char 'n'
      , '\r' <$ char 'r'
      , '\t' <$ char 't'
      , unicodeHexQuad >>= \high -> do
          if inRange highSurrogateRange high
            then do
              low <- escaped unicodeHexQuad
              if inRange lowSurrogateRange low
                then
                  pure . chr . sum $
                    [ (high - fst highSurrogateRange) * 0x400
                    , low - fst lowSurrogateRange
                    , 0x10000
                    ]
                else oops
            else
              pure $ chr high
      ]
    highSurrogateRange = (0xD800, 0xDBFF)
    lowSurrogateRange = (0xDC00, 0xDFFF)
    unicodeHexQuad = char 'u' *> do
        [(num, "")] <- readHex <$> replicateM 4 hexDigit
        pure num
----------------------------------------------------------------------------
null :: Lexer ()
null = void (string "null")
----------------------------------------------------------------------------
punctuator :: Lexer Char
punctuator = oneOf (char <$> "[]{},:")
----------------------------------------------------------------------------
whitespace :: Lexer ()
whitespace = void (satisfy isSpace)
----------------------------------------------------------------------------
token :: Lexer Token
token = oneOf
  [ TokenPunctuator <$> punctuator
  , TokenNumber <$> number
  , TokenBool <$> bool
  , TokenString <$> string'
  , TokenNull <$ null
  ]
----------------------------------------------------------------------------
tokens :: Lexer [Token]
tokens = some (many whitespace *> token)
----------------------------------------------------------------------------
