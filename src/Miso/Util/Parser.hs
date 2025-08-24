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
  case runParserT parser tokens of
    []        -> Left (NoParses (last tokens))
    [(x, [])] -> Right x
    [(_, xs)] -> Left (UnexpectedParse xs)
    xs        -> Left (Ambiguous xs)
----------------------------------------------------------------------------
type Parser token a = ParserT [token] [] a
----------------------------------------------------------------------------
newtype ParserT token m a
  = Parser
  { runParserT :: token -> m (a, token)
  }
----------------------------------------------------------------------------
instance Functor (ParserT token []) where
  fmap f (Parser run) = Parser $ \input ->
    case run input of
      tokens -> [ (f x, toks) | (x, toks) <- tokens ]
----------------------------------------------------------------------------
instance Applicative (ParserT token []) where
  pure x = Parser $ \s -> pure (x,s)
  Parser f <*> Parser g = Parser $ \input -> do
    (k, s) <- f input
    (x, t) <- g s
    pure (k x, t)
----------------------------------------------------------------------------
instance Alternative (ParserT token []) where
  empty = Parser $ \_ -> []
  Parser f <|> Parser g =
    Parser $ \tokens ->
      case f tokens of
        [] -> g tokens
        r  -> r
----------------------------------------------------------------------------
instance Monad (ParserT token []) where
  return = pure
  Parser f >>= k = Parser $ \tokens -> do
    (x, tokens') <- f tokens
    runParserT (k x) tokens'
----------------------------------------------------------------------------
instance MonadFail (ParserT token []) where
  fail _ = Parser $ \_ -> []
----------------------------------------------------------------------------
satisfy :: (token -> Bool) -> Parser token token
satisfy f = Parser $ \input ->
  case input of
    t : ts | f t -> [(t, ts)]
    _ -> []
----------------------------------------------------------------------------
allTokens :: Parser token [token]
allTokens = Parser $ \input -> [(input, input)]
----------------------------------------------------------------------------
token_ :: Eq token => token -> Parser token token
token_ t = satisfy (==t)
----------------------------------------------------------------------------
peek :: Parser a a
peek = Parser $ \tokens ->
  case tokens of
    [] -> []
    (x:xs) -> [(x, x:xs)]
----------------------------------------------------------------------------
errorOut :: errorToken -> ParserT errorToken [] ()
errorOut x = Parser $ \_ -> [((),x)]
----------------------------------------------------------------------------
