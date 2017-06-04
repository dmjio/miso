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

import qualified Data.Set          as S
import           Miso

main :: IO ()
main = startApp App {..}
  where
    model = Model (0,0) (0,0) mempty 0
    events = defaultEvents
    subs = [ mouseSub HandleMouse
           , keyboardSub HandleKeys
           , windowSub HandleWindow
           ]
    update = updateModel
    view = appView

updateModel :: Action -> Model -> Effect Model Action
updateModel (HandleMouse newCoords) model = noEff newModel
  where
    newModel = model { coords = newCoords }
updateModel (HandleKeys newKeys) model = noEff newModel
  where
    newModel = model { keys = newKeys }
updateModel (HandleWindow newWindow) model = noEff newModel
  where
    newModel = model { window = newWindow }
updateModel AddOne model@Model{..} = noEff model { val = val + 1 }
updateModel SubOne model@Model{..} = noEff model { val = val - 1 }
updateModel Id model = noEff model
updateModel (Focus id') model =
  model <# do
    focus id'
    pure Id
updateModel (Blur id') model =
  model <# do
    blur id'
    pure Id

data Action
  = HandleMouse (Int, Int)
  | HandleKeys (S.Set Int)
  | HandleWindow (Int,Int)
  | AddOne
  | SubOne
  | Focus MisoString
  | Blur MisoString
  | Id

data Model = Model {
   coords :: (Int, Int)
 , window :: (Int, Int)
 , keys :: S.Set Int
 , val :: Int
} deriving (Show, Eq)

appView :: Model -> View Action
appView Model{..} = div_ [] [
   div_ [ ] [ text (show coords) ]
 , div_ [ ] [ text (show keys) ]
 , div_ [ ] [ text (show window) ]
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


