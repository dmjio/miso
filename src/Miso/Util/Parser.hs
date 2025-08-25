-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
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
  , satisfy
  , peek
  , token_
  , errorOut
  , allTokens
  , modifyTokens
  , askParser
  ) where
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import           Control.Monad.Fail (MonadFail (..))
#endif
import           Control.Applicative
----------------------------------------------------------------------------
import           Miso.Util.Lexer (LexerError)
----------------------------------------------------------------------------
data ParseError a token
  = UnexpectedParse [token]
  | LexicalError LexerError
  | Ambiguous [(a, [token])]
  | NoParses token
  | EmptyStream
  deriving (Show, Eq)
----------------------------------------------------------------------------
parse :: Parser token a -> [token] -> Either (ParseError a token) a
parse _ [] = Left EmptyStream
parse parser tokens =
  case runParserT parser () tokens of
    []        -> Left (NoParses (last tokens))
    [(x, [])] -> Right x
    [(_, xs)] -> Left (UnexpectedParse xs)
    xs        -> Left (Ambiguous xs)
----------------------------------------------------------------------------
type Parser token a = ParserT () [token] [] a
----------------------------------------------------------------------------
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
  fail _ = Parser $ \_ _ -> []
----------------------------------------------------------------------------
satisfy :: (a -> Bool) -> ParserT r [a] [] a
satisfy f = Parser $ \_ input ->
  case input of
    t : ts | f t -> [(t, ts)]
    _ -> []
----------------------------------------------------------------------------
allTokens :: ParserT r a [] a
allTokens = Parser $ \_ input -> [(input, input)]
----------------------------------------------------------------------------
modifyTokens :: (t -> t) -> ParserT r t [] ()
modifyTokens f = Parser $ \_ input -> [((), f input)]
----------------------------------------------------------------------------
token_ :: Eq token => token -> Parser token token
token_ t = satisfy (==t)
----------------------------------------------------------------------------
askParser :: ParserT r token [] r
askParser = Parser $ \r input -> [(r, input)]
----------------------------------------------------------------------------
peek :: Parser a a
peek = Parser $ \_ tokens ->
  case tokens of
    [] -> []
    (x:xs) -> [(x, x:xs)]
----------------------------------------------------------------------------
errorOut :: errorToken -> ParserT r errorToken [] ()
errorOut x = Parser $ \_ _ -> [((),x)]
----------------------------------------------------------------------------
