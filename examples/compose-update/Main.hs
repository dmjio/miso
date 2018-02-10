{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- This example demonstrates how you can split your update function
-- into separate update functions for parts of your model and then
-- combine them into a single update function operating on the whole
-- model which combines their effects.

import Control.Monad
import Data.Monoid

import Miso
import Miso.Lens
import Miso.String

-- In this slightly contrived example, our model consists of two
-- counters. When one of those counters is incremented, the other is
-- decremented and the other way around.
type Model = (Int, Int)

data Action
  = Increment
  | Decrement
  | NoOp
  deriving (Show, Eq)

-- We are going to use 'Lens'es in this example. Since @miso@ does not
-- depend on a lens library we are going to define a couple of
-- utilities ourselves. We recommend that in your own applications,
-- you depend on a lens library such as @lens@ or @microlens@ to get
-- these definitions.

-- | You can find this under the same name in @lens@ and
-- @microlens@. @lens@ also provides the infix operator '%%~' as a
-- synonym for 'traverseOf'.
--
-- In this example we are only going to use this when applied to
-- 'Lens' m a' and using 'Effect Action' for the @f@ type variable. In
-- that case the specialized type signature is:
--
-- @traverseOf :: Functor f => Lens' m a -> (a -> Effect Action a) -> s -> Effect action s
traverseOf :: Functor f => Lens s t a b -> (a -> f b) -> s -> f t
traverseOf = id

-- | A lens into the first element of a tuple. Both @lens@ and
-- @microlens@ provide this under the same name.
_1 :: Lens (a,c) (b,c) a b
_1 f (a,c) = (,c) <$> f a

-- | A lens into the second element of a tuple. Both @lens@ and
-- @microlens@ provide this under the same name.
_2 :: Lens (c,a) (c,b) a b
_2 f (c,a) = (c,) <$> f a

-- | Update function for the first counter in our 'Model'.
updateFirstCounter :: Action -> Int -> Effect Action Int
updateFirstCounter Increment m = noEff (m + 1)
updateFirstCounter Decrement m = noEff (m - 1)
updateFirstCounter NoOp m = noEff m

-- | Update function for the second counter in our 'Model'. As we’ve
-- mentioned before, this counter is decremented when the first
-- counter is incremented and the other way around.
updateSecondCounter :: Action -> Int -> Effect Action Int
updateSecondCounter Increment m = noEff (m - 1)
updateSecondCounter Decrement m = noEff (m + 1)
updateSecondCounter NoOp m = noEff m

-- | This is the combined update function for both counters.
updateModel :: Action -> Model -> Effect Action Model
updateModel act =
  let -- We use 'traverseOf' to lift an update function for one
      -- counter to an update function that operates on both
      -- counters. The lifted function leaves the other counter
      -- untouched.
      liftedUpdateFirst :: Model -> Effect Action Model
      liftedUpdateFirst = traverseOf _1 (updateFirstCounter act)
      liftedUpdateSecond :: Model -> Effect Action Model
      liftedUpdateSecond = traverseOf _2 (updateSecondCounter act)
  in -- Since 'Effect Action' is an instance of 'Monad', we can just
     -- use '<=<' to compose these lifted update functions.  It might
     -- be helpful to look at the type signature of '<=<' specialized
     -- for 'Effect Action':
     --
     -- @(<=<) :: (b -> Effect Action c) -> (a -> Effect Action b) -> a -> Effect Action c
     liftedUpdateFirst <=< liftedUpdateSecond

main :: IO ()
main = startApp App { initialAction = NoOp, ..}
  where
    model  = (0, 0)
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

viewModel :: Model -> View Action
viewModel (x, y) =
  div_
    []
    [ button_ [onClick Increment] [text "+"]
    , text (ms x <> " | " <> ms y)
    , button_ [onClick Decrement] [text "-"]
    ]
