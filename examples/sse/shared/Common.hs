{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common (
    -- * Component
    sseComponent,

    -- * Types
    Model,
    Action,

    -- * Exported links
    goHome,
    the404,
) where

import Data.Proxy
import Servant.API
import Servant.Links

import Miso
import Miso.String

data Model = Model
    { modelUri :: URI
    , modelMsg :: String
    }
    deriving (Show, Eq)

data Action
    = ServerMsg String
    | NoOp
    | ChangeURI URI
    | HandleURI URI
    deriving (Show, Eq)

home :: Model -> View Action
home (Model _ msg) =
    div_
        []
        [ div_
            []
            [ h3_
                []
                [ text "SSE (Server-sent events) Example"
                ]
            ]
        , text (ms msg)
        ]

-- There is only a single route in this example
type ClientRoutes = View Action

the404 :: View Action
the404 =
    div_
        []
        [ text "404: Page not found"
        , a_ [onClick $ ChangeURI goHome] [text " - Go Home"]
        ]

goHome :: URI
goHome = allLinks' linkURI (Proxy :: Proxy ClientRoutes)

sseComponent :: URI -> Component "sse" Model Action
sseComponent currentURI =
    component
        App
            { initialAction = NoOp
            , model = Model currentURI "No event received"
            , ..
            }
  where
    update = updateModel
    view m
        | Right r <- route (Proxy :: Proxy ClientRoutes) home modelUri m =
            r
        | otherwise = the404
    events = defaultEvents
    subs =
        [ sseSub "/sse" handleSseMsg
        , uriSub HandleURI
        ]
    mountPoint = Nothing
    logLevel = Off

handleSseMsg :: SSE String -> Action
handleSseMsg (SSEMessage msg) = ServerMsg msg
handleSseMsg SSEClose = ServerMsg "SSE connection closed"
handleSseMsg SSEError = ServerMsg "SSE error"

updateModel :: Action -> Model -> Effect Action Model
updateModel (ServerMsg msg) m = pure (m{modelMsg = "Event received: " ++ msg})
updateModel (HandleURI u) m = m { modelUri = u } <# pure NoOp
updateModel (ChangeURI u) m = m <# NoOp <$ pushURI u
updateModel NoOp m = noEff m
