-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE CPP                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util.Parser
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Util.Parser
  ( -- ** Types
    Parser
  , ParserT (..)
  , ParseError (..)
    -- ** Combinators
  , parse
  , anyToken
  , satisfy
  , peek
  , token_
  , errorOut
  , allTokens
  , modifyTokens
  , askParser
  , endOfInput
  ) where
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import           Control.Monad.Fail (MonadFail (..))
#endif
import           Control.Applicative
import           Control.Monad
import           Data.Maybe (isNothing)
----------------------------------------------------------------------------
import           Miso.Util.Lexer (LexerError)
----------------------------------------------------------------------------
-- | A type for expressing failure during parsing.
data ParseError a token
  = UnexpectedParse [token]
  | LexicalError LexerError
  | Ambiguous [(a, [token])]
  | NoParses token
  | EmptyStream
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Executes a parser against a series of tokens.
parse :: Parser token a -> [token] -> Either (ParseError a token) a
parse _ [] = Left EmptyStream
parse parser tokens =
  case runParserT parser () tokens of
    []        -> Left (NoParses (last tokens))
    [(x, [])] -> Right x
    [(_, xs)] -> Left (UnexpectedParse xs)
    xs        -> Left (Ambiguous xs)
----------------------------------------------------------------------------
-- | Convenience synonym when defining parser combinators
type Parser token a = ParserT () [token] [] a
----------------------------------------------------------------------------
-- | Core type for parsing
newtype ParserT r token m a
  = Parser
  { runParserT :: r -> token -> m (a, token)
  }
----------------------------------------------------------------------------
instance Functor (ParserT r token []) where
  fmap f (Parser run) = Parser $ \r input ->
    case run r input of
      tokens -> [ (f x, toks) | (x, toks) <- tokens ]
----------------------------------------------------------------------------
instance Applicative (ParserT r token []) where
  pure x = Parser $ \_ s -> pure (x,s)
  Parser f <*> Parser g = Parser $ \r input -> do
    (k, s) <- f r input
    (x, t) <- g r s
    pure (k x, t)
----------------------------------------------------------------------------
instance Alternative (ParserT r token []) where
  empty = Parser $ \_ _ -> []
  Parser f <|> Parser g =
    Parser $ \r tokens ->
      case f r tokens of
        [] -> g r tokens
        x  -> x
----------------------------------------------------------------------------
instance Monad (ParserT r token []) where
  return = pure
  Parser f >>= k = Parser $ \r tokens -> do
    (x, tokens') <- f r tokens
    runParserT (k x) r tokens'
----------------------------------------------------------------------------
instance MonadFail (ParserT r token []) where
  fail _ = empty
----------------------------------------------------------------------------
instance MonadPlus (ParserT r token [])
----------------------------------------------------------------------------
-- | Match any token.
anyToken :: ParserT r [a] [] a
anyToken = Parser $ \_ input ->
  case input of
    t : ts -> [(t, ts)]
    _ -> []
----------------------------------------------------------------------------
-- | Succeeds for any token for which the predicate @f@ returns 'True'.
-- Returns the parsed token.
satisfy :: (a -> Bool) -> ParserT r [a] [] a
satisfy f = do
  t <- anyToken
  guard (f t)
  pure t
----------------------------------------------------------------------------
-- | Succeeds if the next token in the stream matches the given one.
-- Returns the parsed token.
token_ :: Eq token => token -> Parser token token
token_ t = satisfy (==t)
----------------------------------------------------------------------------
-- | Returns all input from a parser
allTokens :: ParserT r a [] a
allTokens = Parser $ \_ input -> [(input, input)]
----------------------------------------------------------------------------
-- | Modifies tokens
modifyTokens :: (t -> t) -> ParserT r t [] ()
modifyTokens f = Parser $ \_ input -> [((), f input)]
----------------------------------------------------------------------------
-- | Retrieves read-only state from a Parser
askParser :: ParserT r token [] r
askParser = Parser $ \r input -> [(r, input)]
----------------------------------------------------------------------------
-- | Views the next token without consuming input
peek :: Parser a a
peek = Parser $ \_ tokens ->
  case tokens of
    [] -> []
    (x:xs) -> [(x, x:xs)]
----------------------------------------------------------------------------
-- | Parser combinator that always fails
errorOut :: errorToken -> ParserT r errorToken [] ()
errorOut x = Parser $ \_ _ -> [((),x)]
----------------------------------------------------------------------------
-- | Parser combinator that only succeeds if there are no more tokens.
endOfInput :: Parser a ()
endOfInput = guard . isNothing =<< optional anyToken
----------------------------------------------------------------------------
