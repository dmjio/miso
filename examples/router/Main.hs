{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Proxy
import Servant.API

import Miso

-- | Model
data Model
  = Model
  { uri :: URI
    -- ^ current URI of application
  } deriving (Eq, Show)

-- | Action
data Action
  = HandleURI URI
  | ChangeURI URI
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  currentRoute <- getURI
  startApp App { model = Model currentRoute, ..}
  where
    update = updateModel
    events = defaultEvents
    subs   = [ uriSub HandleURI ]
    view   = viewModel

-- | Update your model
updateModel :: Action -> Model -> Effect Model Action
updateModel (HandleURI u) m = m { uri = u } <# do
  pure NoOp
updateModel (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel _ m = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} =
  case runRoute uri (Proxy :: Proxy API) handlers of
    Left _ -> the404
    Right v -> v
  where
    handlers = about :<|> home
    home = div_ [] [
        div_ [] [ text "home" ]
      , button_ [ onClick goAbout ] [ text "go about" ]
      ]
    about = div_ [] [
        div_ [] [ text "about" ]
      , button_ [ onClick goHome ] [ text "go home" ]
      ]
    the404 = div_ [] [
        text "the 404 :("
      , button_ [ onClick goHome ] [ text "go home" ]
      ]

-- | Type-level routes
type API = About :<|> Home
type Home = View Action
type About = "about" :> View Action

-- | Type-safe links used in `onClick` event handlers to route the application
goAbout, goHome :: Action
(goHome, goAbout) = (goto api home, goto api about)
  where
    goto a b = ChangeURI (safeLink a b)
    home  = Proxy :: Proxy Home
    about = Proxy :: Proxy About
    api   = Proxy :: Proxy API

