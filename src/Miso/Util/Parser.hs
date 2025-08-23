-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances #-}
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
    Parser
  , ParserT
  , ParseError (..)
    -- ** Combinators
  , parse
  , satisfy
  , peek
  , token_
  ) where
----------------------------------------------------------------------------
import Control.Monad.State
----------------------------------------------------------------------------
import Miso.Util.Lexer (LexerError)
----------------------------------------------------------------------------
data ParseError a token
  = UnexpectedParse [token]
  | LexicalError LexerError
  | Ambiguous [(a,[token])]
  | NoParses token
  | EmptyStream
  deriving (Show, Eq)
----------------------------------------------------------------------------
parse :: Parser token a -> [token] -> Either (ParseError a token) a
parse _ [] = Left EmptyStream
parse parser tokens =
  case runStateT parser tokens of
    []        -> Left (NoParses (last tokens))
    [(x, [])] -> Right x
    [(_, xs)] -> Left (UnexpectedParse xs)
    xs        -> Left (Ambiguous xs)
----------------------------------------------------------------------------
type ParserT token m a = StateT token m a
----------------------------------------------------------------------------
type Parser token a = ParserT [token] [] a
----------------------------------------------------------------------------
satisfy :: (token -> Bool) -> Parser token token
satisfy f = StateT $ \input ->
  case input of
    t : ts | f t -> [(t, ts)]
    _ -> []
----------------------------------------------------------------------------
token_ :: Eq token => token -> Parser token token
token_ t = satisfy (==t)
----------------------------------------------------------------------------
peek :: Parser a a
peek = StateT $ \tokens ->
  case tokens of
    [] -> []
    (x:xs) -> [(x, x:xs)]
----------------------------------------------------------------------------
