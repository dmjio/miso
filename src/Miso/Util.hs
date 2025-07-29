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
  ) where
-----------------------------------------------------------------------------
import           Data.Maybe (isJust, fromMaybe)
import           Control.Applicative (Alternative, many, empty, (<|>), optional)
import           Data.Foldable (toList)
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
oneOf :: Alternative f => [f a] -> f a
oneOf = foldr (<|>) empty
----------------------------------------------------------------------------
enclosed :: Applicative f => f a -> f b -> f c -> f c
enclosed l r x = l *> x <* r
----------------------------------------------------------------------------
optionalDefault :: Alternative f => b -> f b -> f b
optionalDefault def p = fromMaybe def <$> optional p
----------------------------------------------------------------------------
exists :: Alternative f => f a -> f Bool
exists p = isJust <$> optional p
----------------------------------------------------------------------------
sepBy1 :: Alternative m => m sep -> m a -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)
----------------------------------------------------------------------------
sepBy :: Alternative m => m sep -> m a -> m [a]
sepBy sep p = sepBy1 sep p <|> pure []
----------------------------------------------------------------------------
between :: Applicative f => f a -> f b -> f c -> f (b, c)
between c l r = (,) <$> l <*> (c *> r)
----------------------------------------------------------------------------
-- | Smart constructor for tuple
--
(=:) :: k -> v -> (k, v)
k =: v = (k,v)
----------------------------------------------------------------------------
