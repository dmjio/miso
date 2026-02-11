{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Miso.JSON.Lexer where

import Control.Applicative (optional, Alternative (some, many))
import Data.Char (isHexDigit, chr, isSpace)
import Data.Foldable (Foldable (fold))
import Data.Functor (void)
import Data.Ix (Ix (inRange))
import Data.Maybe (catMaybes, listToMaybe)
import Miso.String (fromMisoString, ToMisoString (toMisoString), MisoString)
import Miso.Util (oneOf)
import Miso.Util.Lexer hiding (string', token)
import Numeric (readHex)
import Prelude hiding (null)

#if __GLASGOW_HASKELL__ <= 881
import Control.Applicative (liftA2)
#endif

data Token
    = TokenPunctuator Char
    | TokenNumber Double
    | TokenBool Bool
    | TokenString MisoString
    | TokenNull
    deriving (Eq, Show)

number :: Lexer Double
number = fromMisoString . fold . catMaybes <$> sequence
    [ optional $ string "-"
    , Just <$> int
    , optional $ liftA2 (<>) (string ".") int
    , optional $ liftA2 (<>) (oneOf $ string <$> ["e", "e+", "e-", "E", "E+", "E-"]) int
    ]
    where
        digit = satisfy $ inRange ('0', '9')
        int = toMisoString <$> some digit

bool :: Lexer Bool
bool =
    oneOf
        [ False <$ string "false"
        , True <$ string "true"
        ]

string' :: Lexer MisoString
string' = char '"' *> (toMisoString <$> some character) <* char '"'
    where
        character =
            oneOf
                [ satisfy $ \c -> c /= '"' && c /= '\\'
                , escapedCharacter
                ]
        hexDigit = satisfy isHexDigit
        escapedCharacter = char '\\' *>
            oneOf
                [ char '"'
                , char '\\'
                , char '/'
                , '\b' <$ char 'b'
                , '\f' <$ char 'f'
                , '\n' <$ char 'n'
                , '\r' <$ char 'r'
                , '\t' <$ char 't'
                , do
                    a <- hexDigit
                    b <- hexDigit
                    c <- hexDigit
                    d <- hexDigit
                    maybe oops (pure . chr . fst)
                        . listToMaybe
                        . readHex
                        $ [a, b, c, d]
                ]

null :: Lexer ()
null = void $ string "null"

punctuator :: Lexer Char
punctuator = oneOf $ char <$> "[]{},:"

whitespace :: Lexer ()
whitespace = void $ satisfy isSpace

token :: Lexer Token
token =
    oneOf
        [ TokenPunctuator <$> punctuator
        , TokenNumber <$> number
        , TokenBool <$> bool
        , TokenString <$> string'
        , TokenNull <$ null
        ]

tokens :: Lexer [Token]
tokens = some $ many whitespace *> token
