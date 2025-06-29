{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad.State
import           Data.Aeson
import           Data.Bool
import           GHC.Generics

import           Miso
import           Miso.String (MisoString)
import qualified Miso.String as S

import qualified Miso.Style as CSS

#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run (startComponent app)

app :: Component "websocket" Model Action
app = (defaultComponent emptyModel updateModel appView)
  { events = defaultEvents <> keyboardEvents
  , subs =
    [ websocketSub url protocols HandleWebSocket
    ]
  } where
      url = URL "wss://echo.websocket.org"
      protocols = Protocols []


emptyModel :: Model
emptyModel = Model (Message "") mempty

updateModel :: Action -> Effect Model Action
updateModel (HandleWebSocket (WebSocketMessage (Message m))) =
  modify $ \model -> model { received = m }
updateModel (SendMessage msg) =
  io_ (send msg)
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
        [ CSS.style_ [ CSS.textAlign "center" ] ]
        [ link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"]
        , h1_ [ CSS.style_ [CSS.fontWeight "bold"]
              ]
          [ a_
            [ href_ "https://github.com/dmjio/miso"]
            [ text $ S.pack "Miso Websocket Example"]
          ]
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
