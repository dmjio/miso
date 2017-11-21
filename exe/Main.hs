{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso
import Miso.String

type Model = Int

main :: IO ()
main = startApp App { initialAction = SayHelloWorld, ..}
  where
    model  = 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    mountPoint = Nothing
    subs   = []

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World!" >> pure NoOp

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

viewModel :: Int -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text $ ms (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]

