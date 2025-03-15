{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Proxy
import Miso
import Servant.API
import Servant.Links

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Model
data Model = Model
    { uri :: URI
    -- ^ current URI of application
    }
    deriving (Eq, Show)

-- | Action
data Action
    = HandleURI URI
    | ChangeURI URI
    | NoOp
    deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main =
    run $ do
        currentURI <- getCurrentURI
        startApp App{model = Model currentURI, initialAction = NoOp, ..}
  where
    update = updateModel
    events = defaultEvents
    subs = [uriSub HandleURI]
    view = viewModel
    mountPoint = Nothing
    logLevel = Off

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleURI u) m = m { uri = u } <# pure NoOp
updateModel (ChangeURI u) m = m <# NoOp <$ pushURI u
updateModel _ m             = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel model = view
  where
    view =
        either (const the404) id $
            runRoute (Proxy :: Proxy API) handlers uri model
    handlers = about :<|> home
    home (_ :: Model) =
        div_
            []
            [ div_ [] [text "home"]
            , button_ [onClick goAbout] [text "go about"]
            ]
    about (_ :: Model) =
        div_
            []
            [ div_ [] [text "about"]
            , button_ [onClick goHome] [text "go home"]
            ]
    the404 =
        div_
            []
            [ text "the 404 :("
            , button_ [onClick goHome] [text "go home"]
            ]

-- | Type-level routes
type API = About :<|> Home
type Home = View Action
type About = "about" :> View Action

-- | Type-safe links used in `onClick` event handlers to route the application
aboutUri, homeUri :: URI
aboutUri :<|> homeUri = allLinks' linkURI (Proxy @API)

goHome, goAbout :: Action
goHome = ChangeURI homeUri
goAbout = ChangeURI aboutUri
