----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON.Parser
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A Parser for the JSON specification. Meant to be used on the server w/ SSR.
--
-- This was ported from <https://github.com/dmjio/json-test> by [@ners](https://github.com/ners)
-- 
----------------------------------------------------------------------------
module Miso.JSON.Parser (decodePure) where
----------------------------------------------------------------------------
import           Data.Bifunctor (Bifunctor(first))
import           Data.Functor (void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Prelude hiding (null)
----------------------------------------------------------------------------
import           Miso.JSON.Types
import           Miso.JSON.Lexer (Token (..), tokens)
import           Miso.String (MisoString)
import           Miso.Util (sepBy, oneOf)
import           Miso.Util.Parser
import           Miso.Util.Lexer (runLexer, mkStream)
----------------------------------------------------------------------------
number :: Parser Token Double
number = do
  TokenNumber d <- anyToken
  pure d
----------------------------------------------------------------------------
bool :: Parser Token Bool
bool = do
  TokenBool b <- anyToken
  pure b
----------------------------------------------------------------------------
string' :: Parser Token MisoString
string' = do
  TokenString s <- anyToken
  pure s
----------------------------------------------------------------------------
array :: Parser Token [Value]
array = do
  void . token_ $ TokenPunctuator '['
  values <- sepBy (token_ $ TokenPunctuator ',') value
  void . token_ $ TokenPunctuator ']'
  pure values
----------------------------------------------------------------------------
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
----------------------------------------------------------------------------
null :: Parser Token ()
null = void $ token_ TokenNull
----------------------------------------------------------------------------
value :: Parser Token Value
value = oneOf
  [ Number <$> number
  , Bool <$> bool
  , String <$> string'
  , Array <$> array
  , Object <$> object
  , Null <$ null
  ]
----------------------------------------------------------------------------
decodePure :: MisoString -> Either String Value
decodePure = first show
  . either (Left . LexicalError) (parse value . fst)
  . runLexer tokens
  . mkStream
----------------------------------------------------------------------------
