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
  , sepBy
  , sepBy1
  , between
  , optionalDefault
  , exists
  , peek
  , token_
  , oneOf
  , enclosed
  ) where
----------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 881
import           Control.Monad.Fail (MonadFail (..))
#endif
import           Control.Applicative
import           Data.Maybe
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
sepBy1 :: Alternative m => m sep -> m a -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)
----------------------------------------------------------------------------
sepBy :: Alternative m => m sep -> m a -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []
----------------------------------------------------------------------------
between :: Applicative f => f a -> f b -> f c -> f (b, c)
between c l r = liftA2 (,) l (c *> r)
----------------------------------------------------------------------------
enclosed :: Applicative f => f a -> f b -> f c -> f c
enclosed l r x = l *> x <* r
----------------------------------------------------------------------------
oneOf :: Alternative f => [f a] -> f a
oneOf = foldr (<|>) empty
----------------------------------------------------------------------------
optionalDefault :: Alternative f => b -> f b -> f b
optionalDefault def p = fromMaybe def <$> optional p
----------------------------------------------------------------------------
exists :: Alternative f => f a -> f Bool
exists p = isJust <$> optional p
----------------------------------------------------------------------------
peek :: Parser a a
peek = Parser $ \tokens ->
  case tokens of
    [] -> []
    (x:xs) -> [(x:xs,x)]
----------------------------------------------------------------------------
