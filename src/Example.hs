{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson                 hiding (Object)
import           Data.Bool
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           GHC.Generics
import           GHCJS.DOM.Element
import           GHCJS.DOM.Event
import           GHCJS.DOM.HTMLInputElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.UIEvent
import           Miso

data Model = Model
  { entries :: [Entry]
  , field :: String
  , uid :: Int
  , visibility :: String
  , start :: Bool
  } deriving (Show, Eq, Generic)

data Entry = Entry
  { description :: String
  , completed :: Bool
  , editing :: Bool
  , eid :: Int
  } deriving (Show, Eq, Generic)

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

newEntry :: String -> Int -> Entry
newEntry desc eid = Entry
  { description = desc
  , completed = False
  , editing = False
  , eid = eid
  }

data Msg
  = Start Bool
  | UpdateField String
  | EditingEntry Int Bool
  | UpdateEntry Int String
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility String
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
  (sig, send) <- signal $ Start (not start)
  let events = [ "keydown", "click", "change", "input", "blur", "dblclick" ] 
  runSignal events $ view send <$> foldp update m sig

update :: Msg -> Model -> Model
update (Start x) model = model { start = x }
update Add model@Model{..} = 
  model {
    uid = uid + 1
  , field = mempty
  , entries = entries ++ [ newEntry field uid | not $ null field ]
  }

update (UpdateField str) model = model { field = str }
update (EditingEntry id' isEditing) model@Model{..} = 
  model { entries = newEntries }
    where
      newEntries = filterMap entries (\t -> eid t == id') $
         \t -> t { editing = isEditing }

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

view :: Address -> Model -> VTree "click"
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

data ClickEvent

onClick :: IO () -> Attribute "click"
onClick = on (Proxy :: Proxy "click") . const

viewEntries :: Address -> String -> [ Entry ] -> VTree "click"
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

viewKeyedEntry :: Address -> Entry -> VTree "click"
viewKeyedEntry = viewEntry

viewEntry :: Address -> Entry -> VTree "click"
viewEntry send Entry {..} = 
  li_
    [ class_ $ T.intercalate " " $ [ "completed" | completed ] ++ [ "editing" | editing ] ]
    [ div_
        [ class_ "view" ]
        [ input_
            [ class_ "toggle"
            , type_ "checkbox"
            , checked_ completed
            , onClick $ send (Check eid (not completed))
            ] []
        , label_
            [ on "dblclick" $ \e -> do
                Just lbl <- fmap castToNode <$> getTarget e
                Just p <- getParentNode lbl
                Just inp <- fmap castToElement <$> getNextSibling p
                send (EditingEntry eid True)
                void $ forkIO $ do
                  threadDelay 100000
                  focus inp
            ]
            [ text_ description ]
        , btn_
            [ class_ "destroy"
            , onClick $ send (Delete eid) 
            ]
           []
        ]
    , input_
        [ class_ "edit"
        , prop "value" description
        , name_ "title"
        , id_ $ T.pack $ "todo-" ++ show eid
        , on "input" $ \(e :: Event) -> do
            Just ele <- fmap castToHTMLInputElement <$> getTarget e
            Just value <- getValue ele
            send (UpdateEntry eid value)
        , on "blur" $ \_ -> send (EditingEntry eid False)
        , onEnter $ send (EditingEntry eid False )
        ]
        []
    ]

viewControls :: Model -> Address -> String -> [ Entry ] -> VTree a
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

viewControlsCount :: Int -> VTree a
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count" ]
     [ strong_ [] [ text_ (show entriesLeft) ]
     , text_ (item_ ++ " left")
     ]
  where
    item_ = bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: Address -> String -> VTree a
viewControlsFilters send visibility =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap send "#/" "All" visibility
    , text_ " "
    , visibilitySwap send "#/active" "Active" visibility
    , text_ " "
    , visibilitySwap send "#/completed" "Completed" visibility
    ]

visibilitySwap :: Address -> String -> String -> String -> VTree a
visibilitySwap send uri visibility actualVisibility =
  li_ [  ]
      [ a_ [ href_ $ T.pack uri
           , class_ $ T.concat [ "selected" | visibility == actualVisibility ]
           , onClick $ send (ChangeVisibility visibility)
           ] [ text_ visibility ]
      ]

viewControlsClear :: Model -> Address -> Int -> VTree a
viewControlsClear _ send entriesCompleted =
  btn_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick $ send DeleteComplete 
    ]
    [ text_ $ "Clear completed (" ++ show entriesCompleted ++ ")" ]

type Address = Msg -> IO ()

type instance EventHandler "input" = Event

viewInput :: Model -> Address -> String -> VTree a
viewInput _ send task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text_ "todos" ]
    , input_
        [ class_ "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        , prop "value" task
        , attr "name" "newTodo"
        , on (Proxy :: Proxy "input") $ \(x :: Event) -> do
            Just target <- fmap castToHTMLInputElement <$> getTarget x
            Just val <- getValue target
            send $ UpdateField val
        , onEnter $ send Add 
        ] []
    ]

onEnter :: IO () -> Attribute "keydown"
onEnter action = 
  on (Proxy :: Proxy "keydown") $ \(e :: Event) -> do
    k <- getKeyCode (castToUIEvent e)
    when (k == 13) action

onInput :: (Event -> IO ()) -> Attribute "input"
onInput = on (Proxy :: Proxy "input")

onBlur :: (Event -> IO ()) -> Attribute "blur"
onBlur = on (Proxy :: Proxy "blur")

infoFooter :: VTree a
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

