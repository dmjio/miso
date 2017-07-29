module Common where

import Data.Proxy
import Servant.API

import Miso

data Model = Model { modelUri :: URI,  modelMsg :: String }
  deriving (Show, Eq)

data Action
  = ServerMsg String
  | NoOp
  | ChangeURI URI
  | HandleURI URI
  deriving (Show, Eq)

home :: Model -> View Action
home (Model _ msg) =
  div_ [] [div_ [] [h3_ [] [text ("SSE Example" :: String)]], text msg]

-- There is only a single route in this example
type ClientRoutes = Home

type Home = View Action

handlers :: Model -> View Action
handlers = home

the404 :: View Action
the404 =
  div_
    []
    [ text ("404: Page not found")
    , a_ [onClick $ ChangeURI goHome] [text " - Go Home"]
    ]

goHome :: URI
goHome = safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy Home)
