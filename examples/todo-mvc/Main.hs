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
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE CPP                        #-}
module Main where

import           Data.Aeson hiding (Object)
import           Data.Bool
import qualified Data.Map as M
import           Data.Monoid
import           GHC.Generics
import           Miso
import           Miso.String (MisoString)
import qualified Miso.String as S

import           Control.Monad.IO.Class

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

default (MisoString)

data Model = Model
  { entries :: [Entry]
  , field :: MisoString
  , uid :: Int
  , visibility :: MisoString
  , step :: Bool
  } deriving (Show, Generic, Eq)

data Entry = Entry
  { description :: MisoString
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
  , step = False
  }

newEntry :: MisoString -> Int -> Entry
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
  | UpdateField MisoString
  | EditingEntry Int Bool
  | UpdateEntry Int MisoString
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility MisoString
   deriving Show

main :: IO ()
main = runApp $ startApp App { initialAction = NoOp, ..}
  where
    model      = emptyModel
    update     = updateModel
    view       = viewModel
    events     = defaultEvents
    mountPoint = Nothing
    subs       = []
    logLevel   = Off

updateModel :: Msg -> Model -> Effect Msg Model
updateModel NoOp m = noEff m
updateModel (CurrentTime n) m =
  m <# do liftIO (print n) >> pure NoOp
updateModel Add model@Model{..} =
  noEff model {
    uid = uid + 1
  , field = mempty
  , entries = entries <> [ newEntry field uid | not $ S.null field ]
  }
updateModel (UpdateField str) model = noEff model { field = str }
updateModel (EditingEntry id' isEditing) model@Model{..} =
  model { entries = newEntries } <# do
    focus $ S.pack $ "todo-" ++ show id'
    pure NoOp
    where
      newEntries = filterMap entries (\t -> eid t == id') $
         \t -> t { editing = isEditing, focussed = isEditing }

updateModel (UpdateEntry id' task) model@Model{..} =
  noEff model { entries = newEntries }
    where
      newEntries =
        filterMap entries ((==id') . eid) $ \t ->
           t { description = task }

updateModel (Delete id') model@Model{..} =
  noEff model { entries = filter (\t -> eid t /= id') entries }

updateModel DeleteComplete model@Model{..} =
  noEff model { entries = filter (not . completed) entries }

updateModel (Check id' isCompleted) model@Model{..} =
   model { entries = newEntries } <# eff
    where
      eff =
        liftIO (putStrLn "clicked check") >>
          pure NoOp

      newEntries =
        filterMap entries (\t -> eid t == id') $ \t ->
          t { completed = isCompleted }

updateModel (CheckAll isCompleted) model@Model{..} =
  noEff model { entries = newEntries }
    where
      newEntries =
        filterMap entries (const True) $
          \t -> t { completed = isCompleted }

updateModel (ChangeVisibility v) model =
  noEff model { visibility = v }

filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go' xs
  where
    go' [] = []
    go' (y:ys)
     | predicate y = f y : go' ys
     | otherwise   = y : go' ys

viewModel :: Model -> View Msg
viewModel m@Model{..} =
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
    , link_
        [ rel_ "stylesheet"
        , href_ "https://d33wubrfki0l68.cloudfront.net/css/d0175a264698385259b5f1638f2a39134ee445a0/style.css"
        ]
    ]

viewEntries :: MisoString -> [ Entry ] -> View Msg
viewEntries visibility entries =
  section_
    [ class_ "main"
    , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ input_
        [ class_ "toggle-all"
        , type_ "checkbox"
        , name_ "toggle"
        , checked_ allCompleted
        , onClick $ CheckAll (not allCompleted)
        ]
      , label_
        [ for_ "toggle-all" ]
          [ text $ S.pack "Mark all as complete" ]
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
    [ class_ $ S.intercalate " " $
       [ "completed" | completed ] <> [ "editing" | editing ]
    ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ completed
            , onClick $ Check eid (not completed)
            ]
        , label_
            [ onDoubleClick $ EditingEntry eid True ]
            [ text description ]
        , button_
            [ class_ "destroy"
            , onClick $ Delete eid
            ] []
        ]
    , input_
        [ class_ "edit"
        , value_ description
        , name_ "title"
        , id_ $ "todo-" <> S.ms eid
        , onInput $ UpdateEntry eid
        , onBlur $ EditingEntry eid False
        , onEnter $ EditingEntry eid False
        ]
    ]

viewControls :: Model ->  MisoString -> [ Entry ] -> View Msg
viewControls model visibility entries =
  footer_  [ class_ "footer"
           , hidden_ (null entries)
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
     [ strong_ [] [ text $ S.ms entriesLeft ]
     , text (item_ <> " left")
     ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: MisoString -> View Msg
viewControlsFilters visibility =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap "#/" "All" visibility
    , text " "
    , visibilitySwap "#/active" "Active" visibility
    , text " "
    , visibilitySwap "#/completed" "Completed" visibility
    ]

visibilitySwap :: MisoString -> MisoString -> MisoString -> View Msg
visibilitySwap uri visibility actualVisibility =
  li_ [  ]
      [ a_ [ href_ uri
           , class_ $ S.concat [ "selected" | visibility == actualVisibility ]
           , onClick (ChangeVisibility visibility)
           ] [ text visibility ]
      ]

viewControlsClear :: Model -> Int -> View Msg
viewControlsClear _ entriesCompleted =
  button_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text $ "Clear completed (" <> S.ms entriesCompleted <> ")" ]

viewInput :: Model -> MisoString -> View Msg
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ task
        , name_ "newTodo"
        , onInput UpdateField
        , onEnter Add
        ]
    ]

onEnter :: Msg -> Attribute Msg
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

infoFooter :: View Msg
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ [] [ text "Double-click to edit a todo" ]
    , p_ []
        [ text "Written by "
        , a_ [ href_ "https://github.com/dmjio" ] [ text "David Johnson" ]
        ]
    , p_ []
        [ text "Part of "
        , a_ [ href_ "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]
