{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad.State (modify)
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
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = run $
  miso $ \uri ->
    (defaultApp (Model uri) updateModel viewModel)
       { subs = [uriSub HandleURI]
       }

-- | Update your model
updateModel :: Action -> Effect Action Model ()
updateModel (HandleURI u) = modify $ \m -> m { uri = u }
updateModel (ChangeURI u) = scheduleIO_ (pushURI u)

-- | View function, with routing
viewModel :: Model -> View Action
viewModel model = view
  where
    view =
        either (const the404) id $
            route (Proxy :: Proxy API) handlers uri model
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
