{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad
import qualified Data.Map                   as M
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.Event
import           GHCJS.DOM.HTMLInputElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types
import           GHCJS.DOM.UIEvent

import           Miso

data Model = Model {
    nextTaskNum :: Int
  , tasks       :: M.Map Int Task 
  , taskValue   :: String
  , turnOn      :: Bool
  } deriving (Show, Eq)

data Task = Task {
    taskCompleted :: Bool
  , taskContent :: String
  } deriving (Show, Eq)

data Action =
    AddTask String
  | RemoveTask Int
  | ToggleCompleted Int Bool
  | RemoveCompleted
  | TaskValue String
  | Switch
  deriving (Show)

addCss ::  IO ()
addCss = do
  Just doc <- currentDocument
  Just head' <- getHead doc
  Just el <- createElement doc (Just ("link" :: String))
  setAttribute el ("href" :: String) urlLocal
  setAttribute el ("rel" :: String) ("stylesheet" :: String) 
  Just meta <- createElement doc (Just ("meta" :: String))
  setAttribute meta ("charset" :: String) ("utf-8" :: String) 
  () <$ appendChild head' (Just meta)
  () <$ appendChild head' (Just el)
  where
    urlLocal :: String
    urlLocal = "http://localhost:8000/index.css"
  -- url :: String
  -- url = "http://todomvc.com/examples/backbone/node_modules/todomvc-app-css/index.css"

update :: Action -> Model -> Model
update Switch model = model { turnOn = True }
update RemoveCompleted model = do
  model {
      tasks = M.filter (\x -> taskCompleted x == False) (tasks model)
    }
update (TaskValue x) model = model { taskValue = x }
update (ToggleCompleted taskId x) model =
  model { tasks = M.adjust modifyTask taskId (tasks model) }
    where
      modifyTask task = task { taskCompleted = x }
update (RemoveTask n) model@Model{..} = model { tasks = M.delete n tasks }
update (AddTask str) model@Model{..} =
  model { nextTaskNum = nextTaskNum + 1
        , tasks = M.insert (nextTaskNum + 1) (Task False str) tasks
        , taskValue = mempty
        }

main :: IO ()
main = do
  addCss
  (sig, send) <- signal Switch
  runSignal ["keydown", "click", "change", "input"] $
    view send <$> foldp update emptyModel sig

emptyModel :: Model
emptyModel = Model 0 mempty mempty False

view :: (Action -> IO ()) -> Model -> VTree      
view send Model { .. } =
  section_ [ attr "class" "todoapp" ] [
    header_ [] [
      h1_ [] [ text_ "todos" ]
    , input_ [
        type_ "text"
      , class_ "new-todo"
      , autofocus True
      , prop "value" taskValue
      , placeholder "What needs to be done?"

      , on "keydown" $ \(e :: Event) -> do
          key <- getKeyCode (castToUIEvent e)
          when (key == 13) $ send $ AddTask taskValue

      , on "input" $ \(e :: Event) -> do
          Just ele <- fmap castToHTMLInputElement <$> getTarget e
          Just value <- getValue ele
          send $ TaskValue value
       ] [ ]
    ]
    , section_ [ class_ "main" ] [
         input_ [ class_ "toggle-all", id_ "toggle-all", type_ "checkbox" ] []
       , label_ [ attr "for" "toggle-all" ] [ text_ "Mark all as complete" ]
       , ul_ [ class_ "todo-list" ] $
           flip map (M.toList tasks) $ \(taskId, Task {..}) ->
             li_ [] [
               
             ]
       , footer_ [ class_ "footer" ] [ ]
       ]
    , footer_ [ class_ "info" ] [
        p_ [] [ text_ "Double-click to edit a node" ]
     ,  p_ [] [ text_ "Written by "
              , a_ [ href_ "https://github.com/dmjio/" ] [ text_ "David Johnson" ]
              ]
     ,  p_ [] [ text_ "Part of "
              , a_ [ href_ "https://todomvc.com" ] [ text_ "TodoMVC" ]
              ]
     ]
   ]

tasksRemaining :: M.Map k Task -> Int
tasksRemaining m =
  M.size $ flip M.filter m $ \x ->
    taskCompleted x == False

  -- div_ [ ] [
  --     h1_ [] [ text_ "todos" ]
  --   , input_ [ attr "type" "text"
  --            , prop "value" taskValue
  --            , prop "autofocus" True
  --            , attr "placeholder" "What needs to be done?"
  --            , on "keydown" $ \(e :: Event) -> do
  --                key <- getKeyCode (castToUIEvent e)
  --                when (key == 13) $ send $ AddTask taskValue
  --            , on "input" $ \(e :: Event) -> do
  --                Just ele <- fmap castToHTMLInputElement <$> getTarget e
  --                Just value <- getValue ele
  --                send $ TaskValue value
  --            ] [ ]
  --     , ul_ [] $ flip map (M.toList tasks) $ \(taskId, Task {..}) ->
  --         li_ [] [
  --           div_ [] [
  --                flip input_ mempty
  --                     [ attr "type" "checkbox"
  --                     , prop "checked" taskCompleted 
  --                     , on "change" $ \e -> do
  --                         Just ele <- fmap castToHTMLInputElement <$> getTarget e
  --                         checked <- getChecked ele
  --                         send $ ToggleCompleted taskId checked
  --                     ] 
  --                 , if taskCompleted then
  --                     s_ [] [ text_ taskContent ]
  --                   else
  --                     text_ taskContent 
  --                , btn_ [ on "click" $ const $ send (RemoveTask taskId) ]
  --                       [ text_ "x" ]
  --              ]
  --            ]
  --         , btn_ [
  --              on "click" $ \_ -> send RemoveCompleted
  --             ] [ text_ "Clear Completed" ]
  --         , div_ [] [
  --             text_ $ "Items left: " <> show (tasksRemaining tasks)
  --           ]
  --         ]



