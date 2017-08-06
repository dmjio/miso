{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.List

import Miso
import Miso.String hiding (replicate)

type Model = [Int]

main :: IO ()
main = startApp App { initialAction = NoOp
                    , ..
                    }
  where
    model  = [1..10]
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ ] -- setNums SetNums ]

setNums :: ([Int] -> action) -> Sub action model
setNums f _ = \sink -> do
  void . forkIO . forever $ do
    threadDelay 100000
    nums <- sort . nub <$> replicateM 20 getRand
    sink (f nums)

foreign import javascript unsafe "$r = h$rand() % 10;"
  getRand :: IO Int

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetNums xs) _ = noEff xs
updateModel (Say x) m = m <# do
  print x >> pure NoOp
updateModel NewNums m = m <# do
  nums <- replicateM 40 getRand
  print nums >> pure (SetNums nums)

data Action
  = NoOp
  | Say Int
  | SetNums Model
  | NewNums
  deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel xs = div_ [] [
    button_ [ onClick NewNums ] [ "random nums" ]
  , button_ [ onClick $ SetNums [10,9..1 ] ] [ "set 10 10's and 1 9" ]
  , ul_ [] [
     liKeyed_ (toKey x) [ onClick (Say x) ] [ text $ ms (show x) ]
     | x <- xs
     ]
  ]
