{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
module Main where

import Data.Aeson
import GHC.Generics
import Miso

main :: IO ()
main = startApp App {..}
  where
    model = Model Ok
    events = defaultEvents
    subs = [ websocketSub uri prots HandleWebSocket ]
    update = updateModel
    view = appView

uri = URL "ws://echo.websocket.org"
prots = Protocols [ ]


updateModel :: Action -> Model -> Effect Model Action
updateModel (HandleWebSocket (WebSocketMessage m)) model = noEff newModel
  where
    newModel = model { msg = m }
updateModel SendOk model =
  model <# do
    send Ok >> pure Id
updateModel SendHello model =
  model <# do
    send Hello >> pure Id
updateModel CloseSocket model =
  model <# do
    putStrLn "closing..."
    close Nothing Nothing >> pure Id
updateModel GetStatus model =
  model <# do
    putStrLn "socket state"
    print =<< getSocketState
    pure Id
updateModel ConnectSocket model =
  model <# do
    connect uri prots >> pure Id
updateModel Id model = noEff model
updateModel _ model = noEff model

data Message
  = Hello
  | Goodbye
  | Ok
  deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

data Action
  = HandleWebSocket (WebSocket Message)
  | SendOk
  | SendHello
  | CloseSocket
  | ConnectSocket
  | GetStatus
  | Id

data Model = Model {
  msg :: Message
} deriving (Show, Eq)

appView :: Model -> View Action
appView Model{..} = div_ [] [
   div_ [ ] [ text (show msg) ]
 , button_ [ onClick SendOk ] [ text (pack "say hi") ]
 , button_ [ onClick SendHello ] [ text (pack "say hello") ]
 , button_ [ onClick CloseSocket ] [ text (pack "close") ]
 , button_ [ onClick ConnectSocket ] [ text (pack "connect") ]
 , button_ [ onClick GetStatus ] [ text (pack "get status") ]
 ]

