-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Util" provides general-purpose combinators shared across miso's
-- internal modules and available to application code. It is re-exported
-- by "Miso".
--
-- = View helpers
--
-- * 'withFoldable' — @map@ over any 'Foldable' to produce a list of
--   views; particularly handy for @Maybe@:
--
-- @
-- 'withFoldable' (model ^. mAlert) $ \\msg ->
--   'Miso.Html.Element.div_' [ 'Miso.Html.Property.class_' \"alert\" ] [ 'Miso.text' msg ]
-- @
--
-- * 'conditionalViews' — include a list of views only when a condition
--   is 'True'; returns @[]@ otherwise:
--
-- @
-- 'conditionalViews' isLoggedIn
--   [ 'Miso.Html.Element.button_' [ 'Miso.Html.Event.onClick' Logout ] [ 'Miso.text' \"Log out\" ] ]
-- @
--
-- = Parser \/ lexer combinators
--
-- These 'Control.Applicative.Alternative'-polymorphic combinators work
-- with both 'Miso.Util.Lexer.Lexer' and 'Miso.Util.Parser.Parser':
--
-- * 'oneOf' — try alternatives in order, succeeding on the first match
--   (analogous to 'Data.Foldable.asum')
-- * 'sepBy' / 'sepBy1' — parse a list interleaved with a separator
-- * 'enclosed' — parse something between two delimiters (@l *> x \<* r@)
-- * 'between' — parse two things separated by a third, returning a pair
-- * 'optionalDefault' — parse with a fallback default on failure
-- * 'exists' — test whether a combinator succeeds, returning 'Bool'
--
-- = Miscellaneous
--
-- * '(=:)' — infix tuple constructor for key-value pairs:
--   @\"key\" '=:' value@
-- * 'compose' — forward function composition generalised to any
--   'Control.Category.Category': @f \`compose\` g = g . f@
--
-- = See also
--
-- * "Miso.Util.Lexer" — the 'Miso.Util.Lexer.Lexer' combinator library
-- * "Miso.Util.Parser" — the 'Miso.Util.Parser.Parser' combinator library
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
  -- ^ Container to map over (e.g. @Maybe@, @[]@)
  -> (a -> b)
  -- ^ Function to apply to each element
  -> [b]
withFoldable ta f = map f (toList ta)
-----------------------------------------------------------------------------
-- | Conditionally includes views.
-- Hides the 'Miso.Types.View's if the condition is False. Shows them when the condition
-- is True.
conditionalViews
  :: Bool
  -- ^ When 'True' the views are included; when 'False' an empty list is returned
  -> [view]
  -- ^ Views to include conditionally
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
  -- ^ Opening delimiter (e.g. @char '('@)
  -> f b
  -- ^ Closing delimiter (e.g. @char ')'@)
  -> f c
  -- ^ Inner parser\/lexer whose result is returned
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
  -- ^ Default value to use when the parser\/lexer fails
  -> f b
  -- ^ Parser\/lexer to attempt
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
  -- ^ Separator parser\/lexer (result discarded)
  -> m a
  -- ^ Element parser\/lexer
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
  -- ^ Separator parser\/lexer (result discarded)
  -> m a
  -- ^ Element parser\/lexer
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
  -- ^ Separator between the two elements (result discarded)
  -> f b
  -- ^ Left element parser\/lexer
  -> f c
  -- ^ Right element parser\/lexer
  -> f (b, c)
between c l r = (,) <$> l <*> (c *> r)
----------------------------------------------------------------------------
-- | Tuple constructor, useful for constructing key-value pairs.
--
(=:) :: k -> v -> (k, v)
k =: v = (k,v)
----------------------------------------------------------------------------
-- | Function composition generalized to 'Category'
--
-- @
-- test :: Int -> Int
-- test = (+1) \`compose\` (+1)
-- @
compose :: Category cat => cat a b -> cat b c -> cat a c
compose = flip (.)
----------------------------------------------------------------------------
