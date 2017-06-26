{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Miso

type Model = Int

main :: IO ()
main = startApp App {..}
  where
    model  = 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []

updateModel :: Action -> Model -> Effect Model Action
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m + 1)

data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)

viewModel :: Int -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
