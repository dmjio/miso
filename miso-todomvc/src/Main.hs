{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Data.Aeson         hiding (Object)
import           Data.Bool
import qualified Data.Map           as M
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text          as T
import           GHC.Generics
import           Miso
import           Miso.Html

data Model = Model
  { entries :: [Entry]
  , field :: T.Text
  , uid :: Int
  , visibility :: T.Text
  } deriving (Show, Generic, Eq)

data Entry = Entry
  { description :: T.Text
  , completed :: Bool
  , editing :: Bool
  , eid :: Int
  , focussed :: Bool
  } deriving (Show, Generic, Eq)

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
  | CurrentTime Int
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

stepConfig :: Proxy '[]
stepConfig = Proxy

getInitialModel :: IO Model
getInitialModel = do
  getFromStorage "todo-mvc" >>= \case
    Left x  -> putStrLn x >> pure emptyModel
    Right m -> pure m

main :: IO ()
main = do
  m <- getInitialModel
  startApp m view update defaultSettings

update :: Msg -> Model -> Effect Msg Model
update NoOp m = noEff m
update (CurrentTime n) m = print n >> pure NoOp <# m
update Add model@Model{..} =
  noEff model {
    uid = uid + 1
  , field = mempty
  , entries = entries <> [ newEntry field uid | not $ T.null field ]
  }

update (UpdateField str) model = noEffect model { field = str }
update (EditingEntry id' isEditing) model@Model{..} =
  noEff model { entries = newEntries }
    where
      newEntries = filterMap entries (\t -> eid t == id') $
         \t -> t { editing = isEditing, focussed = isEditing }

update (UpdateEntry id' task) model@Model{..} =
  noEff model { entries = newEntries }
    where
      newEntries =
        filterMap entries ((==id') . eid) $ \t ->
           t { description = task }

update (Delete id') model@Model{..} =
  noEff model { entries = filter (\t -> eid t /= id') entries }

update DeleteComplete model@Model{..} =
  noEff model { entries = filter (not . completed) entries }

update (Check id' isCompleted) model@Model{..} =
  eff <# model { entries = newEntries }
    where
      eff =
        putStrLn "clicked check" >>
          pure NoOp

      newEntries =
        filterMap entries (\t -> eid t == id') $ \t ->
          t { completed = isCompleted }

update (CheckAll isCompleted) model@Model{..} =
  noEff model { entries = newEntries }
    where
      newEntries =
        filterMap entries (const True) $
          \t -> t { completed = isCompleted }

update (ChangeVisibility v) model =
  noEff model { visibility = v }

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go xs
  where
    go [] = []
    go (y:ys)
     | predicate y = f y : go ys
     | otherwise   = y : go ys

view :: Model -> View Msg
view m@Model{..} =
 div_
    [ class_ "todomvc-wrapper"
    , style_  $ M.singleton "visibility" "hidden"
    ]
    [ section_
        [ class_ "todoapp" ]
        [ viewInput m field
        , viewEntries visibility entries
        , viewControls m visibility entries
        ]
    , infoFooter
    ]

viewEntries :: T.Text -> [ Entry ] -> View Msg
viewEntries visibility entries =
  section_
    [ class_ "main"
    , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ input_
        [ class_ "toggle-all"
        , type_ "checkbox"
        , attr "name" "toggle"
        , checked_ allCompleted
        , onClick $ CheckAll (not allCompleted)
        ] []
      , label_
          [ attr "for" "toggle-all", attr "draggable" "true" ]
          [ text_ "Mark all as complete" ]
      , ul_ [ class_ "todo-list" ] $
         flip map (filter isVisible entries) $ \t ->
           viewKeyedEntry t
      ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all (==True) $ completed <$> entries
    isVisible Entry {..} =
      case visibility of
        "Completed" -> completed
        "Active" -> not completed
        _ -> True

viewKeyedEntry :: Entry -> View Msg
viewKeyedEntry = viewEntry

viewEntry :: Entry -> View Msg
viewEntry Entry {..} = liKeyed_ (toKey eid)
    [ class_ $ T.intercalate " " $
       [ "completed" | completed ] <> [ "editing" | editing ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ completed
            , onClick $ Check eid (not completed)
            ] []
        , label_
            [ onDoubleClick $ EditingEntry eid True ]
            [ text_ description ]
        , btn_
            [ class_ "destroy"
            , onClick $ Delete eid
            ] []
        ]
    , input_
        [ class_ "edit"
        , prop "value" description
        , name_ "title"
        , autofocus focussed
        , id_ $ "todo-" <> T.pack (show eid)
        , onInput $ UpdateEntry eid
        , onBlur $ EditingEntry eid False
        , onEnter $ EditingEntry eid False
        ]
        []
    ]

viewControls :: Model ->  T.Text -> [ Entry ] -> View Msg
viewControls model visibility entries =
  footer_  [ class_ "footer"
           , prop "hidden" (null entries)
           ]
      [ viewControlsCount entriesLeft
      , viewControlsFilters visibility
      , viewControlsClear model entriesCompleted
      ]
  where
    entriesCompleted = length . filter completed $ entries
    entriesLeft = length entries - entriesCompleted

viewControlsCount :: Int -> View Msg
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count" ]
     [ strong_ [] [ text_ $ T.pack (show entriesLeft) ]
     , text_ (item_ <> " left")
     ]
  where
    item_ = bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: T.Text -> View Msg
viewControlsFilters visibility =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap "#/" "All" visibility
    , text_ " "
    , visibilitySwap "#/active" "Active" visibility
    , text_ " "
    , visibilitySwap "#/completed" "Completed" visibility
    ]

visibilitySwap :: T.Text -> T.Text -> T.Text -> View Msg
visibilitySwap uri visibility actualVisibility =
  li_ [  ]
      [ a_ [ href_ uri
           , class_ $ T.concat [ "selected" | visibility == actualVisibility ]
           , onClick (ChangeVisibility visibility)
           ] [ text_ visibility ]
      ]

viewControlsClear :: Model -> Int -> View Msg
viewControlsClear _ entriesCompleted =
  btn_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text_ $ "Clear completed (" <> T.pack (show entriesCompleted) <> ")" ]

viewInput :: Model -> T.Text -> View Msg
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text_ "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , prop "value" task
        , attr "name" "newTodo"
        , onInput UpdateField
        , onEnter Add
        ] []
    ]

onEnter :: Msg -> Attribute Msg
onEnter action =
  onKeyDown $ bool NoOp action . (== 13)

infoFooter :: View Msg
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
