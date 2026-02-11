{-# LANGUAGE LambdaCase #-}

module Miso.JSON.Parser where

import Data.Functor (void)
import Data.Map.Strict (Map)
import Miso.JSON.Types
import Miso.JSON.Lexer (Token (..))
import Miso.String (MisoString)
import Miso.Util (sepBy, oneOf)
import Miso.Util.Parser
import qualified Data.Map.Strict as Map
import Prelude hiding (null)

number :: Parser Token Double
number = do
    TokenNumber d <- anyToken
    pure d

bool :: Parser Token Bool
bool = do
    TokenBool b <- anyToken
    pure b

string' :: Parser Token MisoString
string' = do
    TokenString s <- anyToken
    pure s

array :: Parser Token [Value]
array = do
    void . token_ $ TokenPunctuator '['
    values <- sepBy (token_ $ TokenPunctuator ',') value
    void . token_ $ TokenPunctuator ']'
    pure values

object :: Parser Token (Map MisoString Value)
object = do
    void . token_ $ TokenPunctuator '{'
    fields <- sepBy (token_ $ TokenPunctuator ',') $ do
        key <- string'
        void . token_ $ TokenPunctuator ':'
        val <- value
        pure (key, val)
    void . token_ $ TokenPunctuator '}'
    pure $ Map.fromList fields

null :: Parser Token ()
null = void $ token_ TokenNull

value :: Parser Token Value
value =
    oneOf
        [ Number <$> number
        , Bool <$> bool
        , String <$> string'
        , Array <$> array
        , Object <$> object
        , Null <$ null
        ]
