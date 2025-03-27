{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.State
import           Data.Aeson
import           Data.Bool
import qualified Data.Map as M
import           GHC.Generics

import           Miso
import           Miso.String (MisoString)
import qualified Miso.String as S

#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ startApp app
  { events = defaultEvents <> keyboardEvents
  , subs =
    [ websocketSub url protocols HandleWebSocket
    ]
  } where
      url = URL "wss://echo.websocket.org"
      protocols = Protocols []

app :: App Effect Model Action ()
app = defaultApp emptyModel updateModel appView

emptyModel :: Model
emptyModel = Model (Message "") mempty

updateModel :: Action -> Effect Model Action ()
updateModel (HandleWebSocket (WebSocketMessage (Message m))) =
  modify $ \model -> model { received = m }
updateModel (SendMessage msg) =
  io (send msg)
updateModel (UpdateMessage m) = do
  modify $ \model -> model { msg = Message m }
updateModel _ = pure ()

instance ToJSON Message
instance FromJSON Message

newtype Message = Message MisoString
  deriving (Eq, Show, Generic)

data Action
  = HandleWebSocket (WebSocket Message)
  | SendMessage Message
  | UpdateMessage MisoString
  | Id

data Model = Model
  { msg :: Message
  , received :: MisoString
  } deriving (Show, Eq)

appView :: Model -> View Action
appView Model{..} =
    div_
        [style_ $ M.fromList [("text-align", "center")]]
        [ link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"]
        , h1_ [style_ $ M.fromList [("font-weight", "bold")]] [a_ [href_ "https://github.com/dmjio/miso"] [text $ S.pack "Miso Websocket Example"]]
        , h3_ [] [text $ S.pack "wss://echo.websocket.org"]
        , input_
            [ type_ "text"
            , onInput UpdateMessage
            , onEnter (SendMessage msg)
            ]
        , button_
            [ onClick (SendMessage msg)
            ]
            [text (S.pack "Send to echo server")]
        , div_ [] [p_ [] [text received | not . S.null $ received]]
        ]

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool Id action . (== KeyCode 13)
