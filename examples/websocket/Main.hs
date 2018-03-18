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

import           Data.Aeson
import           GHC.Generics
import           Data.Bool
import qualified Data.Map as M

import           Miso
import           Miso.String  (MisoString)
import qualified Miso.String  as S

main :: IO ()
main = startApp App { initialAction = Id, ..}
  where
    model = Model mempty mempty
    events = defaultEvents
    subs = [ websocketSub uri protocols HandleWebSocket ]
    update = updateModel
    view = appView
    uri = URL "wss://echo.websocket.org"
    protocols = Protocols [ ]
    mountPoint = Nothing

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleWebSocket (WebSocketMessage (Message m))) model
  = noEff model { received = m }
updateModel (SendMessage msg) model = model <# do send msg >> pure Id
updateModel (UpdateMessage m) model = noEff model { msg = Message m }
updateModel _ model = noEff model

instance ToJSON Message
instance FromJSON Message

newtype Message = Message MisoString
  deriving (Eq, Show, Generic, Monoid)

data Action
  = HandleWebSocket (WebSocket Message)
  | SendMessage Message
  | UpdateMessage MisoString
  | Id

data Model = Model {
    msg :: Message
  , received :: MisoString
  } deriving (Show, Eq)

appView :: Model -> View Action
appView Model{..} = div_ [ style_ $ M.fromList [("text-align", "center")] ] [
   h1_ [style_ $ M.fromList [("font-weight", "bold")] ] [ a_ [ href_ "https://github.com/dmjio/miso" ] [ text $ S.pack "Miso Websocket Example" ] ]
 , h3_ [] [ text $ S.pack "wss://echo.websocket.org" ]
 , input_  [ type_ "text"
           , onInput UpdateMessage
           , onEnter (SendMessage msg)
           ]
 , button_ [ onClick (SendMessage msg)
           ] [ text (S.pack "Send to echo server") ]
 , div_ [ ] [ p_ [ ] [ text received | not . S.null $ received ] ]
 ]

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool Id action . (== KeyCode 13)
