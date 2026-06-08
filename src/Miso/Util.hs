-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Utility functions for views, parsing, and general purpose combinators.
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
withFoldable
  :: Foldable t
  => t a
  -- ^ Container of values to map over (e.g. @Maybe a@, @[a]@)
  -> (a -> b)
  -- ^ Function applied to each element
  -> [b]
withFoldable ta f = map f (toList ta)
-----------------------------------------------------------------------------
-- | Conditionally includes views.
-- Hides the 'Miso.Types.View's if the condition is False. Shows them when the condition
-- is True.
conditionalViews
  :: Bool
  -- ^ When 'True', the views are included; when 'False', an empty list is returned
  -> [view]
  -- ^ Views to conditionally include
  -> [view]
conditionalViews condition views =
    if condition
    then views
    else []
-----------------------------------------------------------------------------
-- | Selects the first 'Alternative', analogous to 'Data.Foldable.asum'.
oneOf :: Alternative f => [f a] -> f a
oneOf = foldr (<|>) empty
----------------------------------------------------------------------------
-- | Convenience function for constructing parser / lexer combinators.
--
-- @
-- test :: Parser a -> Parser a
-- test = enclosed (char '(') (char ')')
-- @
enclosed
  :: Applicative f
  => f a
  -- ^ Left delimiter (e.g. @char '('@)
  -> f b
  -- ^ Right delimiter (e.g. @char ')'@)
  -> f c
  -- ^ Inner parser/applicative whose result is returned
  -> f c
enclosed l r x = l *> x <* r
----------------------------------------------------------------------------
-- | Allow the specification of default values during parsing / lexing
-- in the case of parser / lexer failure.
--
-- @
-- test :: Parser MisoString
-- test = optionalDefault "foo" (string "bar")
-- @
optionalDefault
  :: Alternative f
  => b
  -- ^ Default value returned when @p@ fails
  -> f b
  -- ^ Parser to attempt
  -> f b
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
sepBy1
  :: Alternative m
  => m sep
  -- ^ Separator parser (result discarded)
  -> m a
  -- ^ Element parser
  -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)
----------------------------------------------------------------------------
-- | Interleaves one parser combinator with another, may not have any successful
-- parses.
--
-- @
-- test :: Parser [Int]
-- test = sepBy (char ',') number
-- @
sepBy
  :: Alternative m
  => m sep
  -- ^ Separator parser (result discarded)
  -> m a
  -- ^ Element parser
  -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []
----------------------------------------------------------------------------
-- | Successfully parses the arguments between another combinator
--
-- @
-- test :: Parser (Int, Int)
-- test = between (char '*') number number
-- -- 5*5
-- @
between
  :: Applicative f
  => f a
  -- ^ Separator between the two element parsers
  -> f b
  -- ^ Left element parser
  -> f c
  -- ^ Right element parser
  -> f (b, c)
between c l r = (,) <$> l <*> (c *> r)
----------------------------------------------------------------------------
-- | Tuple constructor, useful for constructing CSS property key-value pairs.
--
-- @
-- style_ [ "display" =: "flex", "color" =: "red" ]
-- @
(=:)
  :: k
  -- ^ Key
  -> v
  -- ^ Value
  -> (k, v)
k =: v = (k,v)
----------------------------------------------------------------------------
-- | Function composition generalized to 'Category'
--
-- @
-- test :: Int -> Int
-- test = (+1) \`compose\` (+1)
-- @
compose
  :: Category cat
  => cat a b
  -- ^ First morphism to apply
  -> cat b c
  -- ^ Second morphism to apply after the first
  -> cat a c
compose = flip (.)
----------------------------------------------------------------------------
