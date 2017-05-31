{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
module Main where

import Miso
import Miso.Subscription.Mouse

default (MisoString)

main :: IO ()
main = startApp emptyModel update view [ mouseSub HandleMouse ] defaultEvents

emptyModel :: Model
emptyModel = Model (0,0) 0

update :: Action -> Model -> Effect Model Action
update (HandleMouse newCoords) model = noEff newModel
  where
    newModel = model { coords = newCoords }
update AddOne model@Model{..} = noEff model { val = val + 1 }
update SubOne model@Model{..} = noEff model { val = val - 1 }
update Id model = noEff model
update (Focus id') model =
  model <# do
    focus id'
    pure Id

update (Blur id') model =
  model <# do
    blur id'
    pure Id

data Action
  = HandleMouse (Int, Int)
  | AddOne
  | SubOne
  | Focus MisoString
  | Blur MisoString
  | Id

data Model = Model {
   coords :: (Int, Int)
 , val :: Int
} deriving (Show, Eq)

view :: Model -> View Action
view Model{..} = div_ [] [
   text (show coords)
 , div_ [ ] [
       button_ [ onClick AddOne ] [ text (pack "+") ]
     , text (show val)
     , button_ [ onClick SubOne ] [ text (pack "-") ]
   ]
 , div_ [ ] [
       button_ [ onClick (Focus "input") ] [ text $ pack "focus on input" ]
     , input_ [ type_ "text", id_ "input", autofocus_ True ] []
     , button_ [ onClick (Blur "input") ] [ text $ pack "blur input"]
     ]
 ]


