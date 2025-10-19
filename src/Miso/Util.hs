-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Util
  ( withFoldable
  , conditionalViews
  , oneOf
  , enclosed
  , optionalDefault
  , exists
  , sepBy1
  , sepBy
  , between
  , (=:)
  , compose
  ) where
-----------------------------------------------------------------------------
import           Control.Category
import           Data.Maybe (isJust, fromMaybe)
import           Control.Applicative (Alternative, many, empty, (<|>), optional)
import           Data.Foldable (toList)
import           Prelude hiding ((.))
-----------------------------------------------------------------------------
-- | Generic @map@ function, useful for creating @View@s from the elements of
-- some @Foldable@. Particularly handy for @Maybe@, as shown in the example
-- below.
--
-- @
-- view model =
--     div_ [] $
--      withFoldable (model ^. mSomeMaybeVal) $ \\someVal ->
--         p_ [] [ text $ "Hey, look at this value: " <> ms (show someVal) ]
-- @
withFoldable :: Foldable t => t a -> (a -> b) -> [b]
withFoldable ta f = map f (toList ta)
-----------------------------------------------------------------------------
-- | Hides the @View@s if the condition is False. Shows them when the condition
-- is True.
conditionalViews :: Bool -> [view] -> [view]
conditionalViews condition views =
    if condition
    then views
    else []
-----------------------------------------------------------------------------
-- | Select the first Alternative, analogous to @asum@.
oneOf :: Alternative f => [f a] -> f a
oneOf = foldr (<|>) empty
----------------------------------------------------------------------------
-- | Convenience function for constructing parser / lexer combinators.
--
-- @
-- test :: Parser a -> Parser a
-- test = enclosed (char '(') (char ')')
-- @
enclosed :: Applicative f => f a -> f b -> f c -> f c
enclosed l r x = l *> x <* r
----------------------------------------------------------------------------
-- | Allow the specification of default values during parsing / lexing
-- in the case of parser / lexer failure.
--
-- @
-- test :: Parser MisoString
-- test = optionalDefault "foo" (string "bar")
-- @
optionalDefault :: Alternative f => b -> f b -> f b
optionalDefault def p = fromMaybe def <$> optional p
----------------------------------------------------------------------------
-- | Combinator for testing parsing / lexing failure on any input.
--
-- @
-- test :: Parser Bool
-- test = exists (string "foo")
-- @
exists :: Alternative f => f a -> f Bool
exists p = isJust <$> optional p
----------------------------------------------------------------------------
-- | Interleaves one parser combinator with another, must have at least one
-- successful parse.
--
-- @
-- test :: Parser [Int]
-- test = sepBy1 (char ',') number
-- @
sepBy1 :: Alternative m => m sep -> m a -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)
----------------------------------------------------------------------------
-- | Interleaves one parser combinator with another, may not have any successful
-- parses.
--
-- @
-- test :: Parser [Int]
-- test = sepBy (char ',') number
-- @
sepBy :: Alternative m => m sep -> m a -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []
----------------------------------------------------------------------------
-- | Successfully parses the arguments between another combinator
--
-- @
-- test :: Parser (Int, Int)
-- test = between (char '*') number number
-- -- 5*5
-- @
between :: Applicative f => f a -> f b -> f c -> f (b, c)
between c l r = (,) <$> l <*> (c *> r)
----------------------------------------------------------------------------
-- | Smart constructor for tuple
--
(=:) :: k -> v -> (k, v)
k =: v = (k,v)
----------------------------------------------------------------------------
-- | Function composition generalized to 'Category'
--
-- @
-- test :: Int -> Int
-- test = (+1) `compose` (+1)
-- @
compose :: Category cat => cat b c -> cat a b -> cat a c
compose = (.)
----------------------------------------------------------------------------
