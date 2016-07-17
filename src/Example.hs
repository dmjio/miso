{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Monad
import           Data.Aeson    hiding (Object)
import           Data.Bool
import           Data.Monoid
import qualified Data.Text     as T
import           GHC.Generics

import           Miso

data Model = Model
  { entries :: [Entry]
  , field :: T.Text
  , uid :: Int
  , visibility :: T.Text
  , start :: Bool
  } deriving (Show, Generic)

data Entry = Entry
  { description :: T.Text
  , completed :: Bool
  , editing :: Bool
  , eid :: Int
  , focussed :: Bool
  } deriving (Show, Generic)

instance ToJSON Entry
instance ToJSON Model

instance FromJSON Entry
instance FromJSON Model

emptyModel :: Model
emptyModel = Model
  { entries = []
  , visibility = "All"
  , field = mempty
  , uid = 0
  , start = False
  }

newEntry :: T.Text -> Int -> Entry
newEntry desc eid = Entry
  { description = desc
  , completed = False
  , editing = False
  , eid = eid
  , focussed = False
  }

data Msg
  = NoOp
  | UpdateField T.Text
  | EditingEntry Int Bool
  | UpdateEntry Int T.Text
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility T.Text
   deriving Show

instance HasConfig Model where
  getConfig =
    AppConfig {
        useStorage = True
      , storageKey = "todo-mvc"
      }

main :: IO ()
main = do
  m@Model{..} <- getFromStorage >>= \case
    Left x -> putStrLn x >> pure emptyModel
    Right m -> pure m
  (sig, send) <- signal NoOp
  runSignal defaultEvents $ view send <$> foldp update m sig

update :: Msg -> Model -> Model
update NoOp model = model
update Add model@Model{..} = 
  model {
    uid = uid + 1
  , field = mempty
  , entries = entries <> [ newEntry field uid | not $ T.null field ]
  }

update (UpdateField str) model = model { field = str }
update (EditingEntry id' isEditing) model@Model{..} = 
  model { entries = newEntries }
    where
      newEntries = filterMap entries (\t -> eid t == id') $
         \t -> t { editing = isEditing, focussed = isEditing }

update (UpdateEntry id' task) model@Model{..} =
  model { entries = newEntries }
    where
      newEntries =
        filterMap entries ((==id') . eid) $ \t ->
           t { description = task }

update (Delete id') model@Model{..} =
  model { entries = filter (\t -> eid t /= id') entries }

update DeleteComplete model@Model{..} =
  model { entries = filter (not . completed) entries }

update (Check id' isCompleted) model@Model{..} =
  model { entries = newEntries }
    where
      newEntries =
        filterMap entries (\t -> eid t == id') $ \t ->
          t { completed = isCompleted }

update (CheckAll isCompleted) model@Model{..} =
  model { entries = newEntries }
    where
      newEntries =
        filterMap entries (const True) $
          \t -> t { completed = isCompleted }

update (ChangeVisibility v) model =
  model { visibility = v }

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go xs
  where
    go [] = []
    go (y:ys)
     | predicate y = f y : go ys
     | otherwise   = y : go ys

view :: Address -> Model -> VTree
view send m@Model{..} = 
 div_
    [ class_ "todomvc-wrapper"
    , style_ "visibility:hidden;"
    ]
    [ section_
        [ class_ "todoapp" ]
        [ viewInput m send field
        , viewEntries send visibility entries
        , viewControls m send visibility entries
        ]
    , infoFooter
    ]

viewEntries :: Address -> T.Text -> [ Entry ] -> VTree
viewEntries send visibility entries =
  section_
    [ class_ "main"
    , style_ $ T.pack $ "visibility:" <> cssVisibility <> ";" 
    ]
    [ input_
        [ class_ "toggle-all"
        , type_ "checkbox"
        , attr "name" "toggle"
        , checked_ allCompleted
        , onClick $ send $ CheckAll (not allCompleted)
        ] []
      , label_
          [ attr "for" "toggle-all", attr "draggable" "true" ]
          [ text_ "Mark all as complete" ]
      , ul_ [ class_ "todo-list" ] $
         flip map (filter isVisible entries) $ \t ->
           viewKeyedEntry send t
      ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all (==True) $ completed <$> entries
    isVisible Entry {..} =
      case visibility of
        "Completed" -> completed
        "Active" -> not completed
        _ -> True

viewKeyedEntry :: Address -> Entry -> VTree
viewKeyedEntry = viewEntry

viewEntry :: Address -> Entry -> VTree 
viewEntry send Entry {..} = 
  li_
    [ class_ $ T.intercalate " " $
       [ "completed" | completed ] <> [ "editing" | editing ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ completed
            , onClick $ send $ Check eid (not completed)
            ] []
        , label_
            [ onDoubleClick $ send (EditingEntry eid True) ]
            [ text_ description ]
        , btn_
            [ class_ "destroy"
            , onClick $ send (Delete eid) 
            ] []
        ]
    , input_
        [ class_ "edit"
        , prop "value" description
        , name_ "title"
        , autofocus focussed
        , id_ $ "todo-" <> T.pack (show eid)
        , onInput $ \value -> send (UpdateEntry eid value)
        , onBlur $ send (EditingEntry eid False)
        , onEnter $ send (EditingEntry eid False)
        ]
        []
    ]

viewControls :: Model -> Address -> T.Text -> [ Entry ] -> VTree
viewControls model send visibility entries =
  footer_  [ class_ "footer"
           , prop "hidden" (null entries)
           ]
      [ viewControlsCount entriesLeft
      , viewControlsFilters send visibility
      , viewControlsClear model send entriesCompleted
      ]
  where
    entriesCompleted = length . filter completed $ entries
    entriesLeft = length entries - entriesCompleted

viewControlsCount :: Int -> VTree
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count" ]
     [ strong_ [] [ text_ $ T.pack (show entriesLeft) ]
     , text_ (item_ <> " left")
     ]
  where
    item_ = bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: Address -> T.Text -> VTree 
viewControlsFilters send visibility =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap send "#/" "All" visibility
    , text_ " "
    , visibilitySwap send "#/active" "Active" visibility
    , text_ " "
    , visibilitySwap send "#/completed" "Completed" visibility
    ]

visibilitySwap :: Address -> T.Text -> T.Text -> T.Text -> VTree 
visibilitySwap send uri visibility actualVisibility =
  li_ [  ]
      [ a_ [ href_ uri
           , class_ $ T.concat [ "selected" | visibility == actualVisibility ]
           , onClick $ send (ChangeVisibility visibility)
           ] [ text_ visibility ]
      ]

viewControlsClear :: Model -> Address -> Int -> VTree 
viewControlsClear _ send entriesCompleted =
  btn_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick $ send DeleteComplete 
    ]
    [ text_ $ "Clear completed (" <> T.pack (show entriesCompleted) <> ")" ]

type Address = Msg -> IO ()

viewInput :: Model -> Address -> T.Text -> VTree 
viewInput _ send task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text_ "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , prop "value" task
        , attr "name" "newTodo"
        , onInput $ \val -> send (UpdateField val)
        , onEnter $ send Add 
        ] []
    ]

onEnter :: IO () -> Attribute 
onEnter action =
  onKeyDown $ \k ->
    when (k == 13) action

infoFooter :: VTree
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ [] [ text_ "Double-click to edit a todo" ]
    , p_ []
        [ text_ "Written by "
        , a_ [ href_ "https://github.com/dmjio" ] [ text_ "David Johnson" ]
        ]
    , p_ []
        [ text_ "Part of "
        , a_ [ href_ "http://todomvc.com" ] [ text_ "TodoMVC" ]
        ]
    ]

