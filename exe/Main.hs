{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.List

import Miso
import Miso.String

type Model = [Int]

main :: IO ()
main = startApp App { initialAction = NoOp
                    , ..
                    }
  where
    model  = [1,2,3]
    update = updateModel
    view   = viewModel
    events = defaultEvents
    mountPoint = Nothing
    subs   = []

foreign import javascript unsafe "$r = h$rand() % 10;"
  getRand :: IO Int

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetNums xs) _ = noEff xs
updateModel (Say x) m = m <# do
  print x >> pure NoOp
updateModel NewNums m = m <# do
  nums <- nub <$> replicateM 3 getRand
  print nums >> pure (SetNums nums)

data Action
  = NoOp
  | Say Int
  | SetNums Model
  | NewNums
  deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel xs = div_ [] [
    button_ [ onClick NewNums ] [ "get new nums" ]
  , ul_ [] [
     liKeyed_ (toKey x) [ onClick (Say x) ] [ text $ ms (show x) ]
     | x <- xs
     ]
  ]
