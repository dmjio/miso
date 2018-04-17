-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Util
  ( withFoldable
  , conditionalViews
  , (=:)
  ) where

import Data.Foldable
import qualified Data.Map as M
import Miso.Html (View)

-- | Generic @map@ function, useful for creating @View@s from the elements of
-- some @Foldable@. Particularly handy for @Maybe@, as shown in the example
-- below.
--
-- @
-- view model =
--     div_ [] $
--      withFoldable (model ^. mSomeMaybeVal) $ \someVal ->
--         p_ [] [ text $ "Hey, look at this value: " <> ms (show someVal) ]
-- @
withFoldable :: Foldable t => t a -> (a -> b) -> [b]
withFoldable ta f = map f (toList ta)

-- | Hides the @View@s the condition is False. Shows them when the condition
-- is True.
conditionalViews :: Bool -> [View action] -> [View action]
conditionalViews condition views =
    if condition
    then views
    else []

-- | Smart constructor for Attributes. This function is helpful when constructing numerous Attributes
-- Example shown below.
-- 
-- @ 
-- div_ [ style_  $ ("background" =: "red" <> "width" =: "250px" <> "height" =: "250px") ] []
-- @
(=:) :: k -> a -> M.Map k a 
a =: b = M.singleton a b