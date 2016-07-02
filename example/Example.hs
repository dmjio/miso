{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import           Control.Monad
import qualified Data.Map                   as M
import           Miso

import           GHCJS.DOM.Event
import           GHCJS.DOM.HTMLInputElement

import           GHCJS.DOM.UIEvent

data Model = Model {
    nextTaskNum :: Int
  , tasks :: M.Map Int Task 
  , taskValue :: String
  , turnOn :: Bool
  } deriving (Show, Eq)

data Task = Task {
    taskCompleted :: Bool
  , taskContent :: String
  } deriving (Show, Eq)

data Action =
    AddTask String
  | RemoveTask Int
  | ToggleCompleted Int Bool
  | TaskValue String
  | ToggleAllCompleted
  | Switch
  deriving (Show)

update :: Action -> Model -> Model
update Switch model = model { turnOn = True }
update (TaskValue x) model = model { taskValue = x }
update (RemoveTask n) model@Model{..} = model { tasks = M.delete n tasks }
update (AddTask str) model@Model{..} =
  model { nextTaskNum = nextTaskNum + 1
        , tasks = M.insert (nextTaskNum + 1) (Task False str) tasks
        , taskValue = mempty }
update (ToggleCompleted taskId x) model =
  model { tasks = M.adjust modifyTask taskId (tasks model) }
    where
      modifyTask task = task { taskCompleted = x }        

main :: IO ()
main = do
  (sig, send) <- signal Switch
  runSignal ["keydown", "click", "change", "input"] $
    view send <$> foldp update (Model 0 mempty mempty False) sig
    where
     
      view send Model { .. } = div_ [ ] [
            h1_ [] [ text_ "todos"]
          , input_ [ attr "type" "text"
                   , prop "value" taskValue
                   , prop "autofocus" True
                   , attr "placeholder" "What needs to be done?"
                   , on "keydown" $ \e -> do
                       key <- getKeyCode (castToUIEvent e :: UIEvent)
                       when (key == 13) $ send $ AddTask taskValue
                   , on "input" $ \e -> do
                       Just ele <- fmap castToHTMLInputElement <$> getTarget e
                       Just value <- getValue ele
                       send $ TaskValue value
                   ] [ ]
          , ul_ [] $ flip map (M.toList tasks) $ \(taskId, Task {..}) ->
             li_ [] [
              div_ [] [
                  flip input_ mempty
                      [ attr "type" "checkbox"
                      , prop "checked" taskCompleted 
                      , on "change" $ \e -> do
                          Just ele <- fmap castToHTMLInputElement <$> getTarget e
                          checked <- getChecked ele
                          send $ ToggleCompleted taskId checked
                      ] 
                  , if taskCompleted then
                      s_ [] [ text_ taskContent ]
                    else
                      text_ taskContent 
                 , btn_ [ on "click" $ \_ -> send (RemoveTask taskId) ]
                        [ text_ "x" ]
               ]
             ]
           ]
