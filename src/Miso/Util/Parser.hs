-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
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
    Parser (..)
  , ParseError (..)
    -- ** Combinators
  , parse
  , satisfy
  , peek
  , token_
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
  | Ambiguous [([token], a)]
  | NoParses token
  | EmptyStream
  deriving (Show, Eq)
----------------------------------------------------------------------------
parse :: Parser token a -> [token] -> Either (ParseError a token) a
parse _ [] = Left EmptyStream
parse parser tokens =
  case runParser parser tokens of
    []        -> Left (NoParses (last tokens))
    [([], x)] -> Right x
    [(xs, _)] -> Left (UnexpectedParse xs)
    xs        -> Left (Ambiguous xs)
----------------------------------------------------------------------------
newtype Parser token a
  = Parser
  { runParser :: [token] -> [([token], a)]
  }
----------------------------------------------------------------------------
instance Functor (Parser token) where
  fmap f (Parser run) = Parser $ \input ->
    fmap f <$> run input
----------------------------------------------------------------------------
instance Applicative (Parser token) where
  pure x = Parser $ \s -> pure (s, x)
  Parser f <*> Parser g = Parser $ \input -> do
    (s, k) <- f input
    (t, x) <- g s
    pure (t, k x)
----------------------------------------------------------------------------
instance Alternative (Parser token) where
  empty = Parser $ \_ -> []
  Parser f <|> Parser g =
    Parser $ \tokens ->
      case f tokens of
        [] -> g tokens
        r  -> r
----------------------------------------------------------------------------
instance Monad (Parser token) where
  return = pure
  Parser f >>= k = Parser $ \tokens -> do
    (tokens', x) <- f tokens
    runParser (k x) tokens'
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
instance MonadFail (Parser token) where
  fail _ = Parser $ \_ -> []
#endif
----------------------------------------------------------------------------
satisfy :: (token -> Bool) -> Parser token token
satisfy f = Parser $ \input ->
  case input of
    t : ts | f t -> [(ts, t)]
    _ -> []
----------------------------------------------------------------------------
token_ :: Eq token => token -> Parser token token
token_ t = satisfy (==t)
----------------------------------------------------------------------------
peek :: Parser a a
peek = Parser $ \tokens ->
  case tokens of
    [] -> []
    (x:xs) -> [(x:xs,x)]
----------------------------------------------------------------------------
