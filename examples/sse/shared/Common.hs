{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Common where

import Data.Proxy
import Servant.API
#if MIN_VERSION_servant(0,10,0)
import Servant.Utils.Links
#endif

import Miso
import Miso.String

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
  div_ [] [div_ [] [h3_ [] [text "SSE (Server-sent events) Example"]], text $ ms msg]

-- There is only a single route in this example
type ClientRoutes = Home

type Home = View Action

handlers :: Model -> View Action
handlers = home

the404 :: View Action
the404 =
  div_
    []
    [ text "404: Page not found"
    , a_ [onClick $ ChangeURI goHome] [text " - Go Home"]
    ]

goHome :: URI
goHome =
#if MIN_VERSION_servant(0,10,0)
  linkURI (safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy Home))
#else
  safeLink (Proxy :: Proxy ClientRoutes) (Proxy :: Proxy Home)
#endif
