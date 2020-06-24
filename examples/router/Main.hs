{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE CPP                 #-}
module Main where

import Data.Proxy
import Servant.API
#if MIN_VERSION_servant(0,14,1)
import Servant.Links
#elif MIN_VERSION_servant(0,10,0)
import Servant.Utils.Links
#endif

import Miso

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

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
main =
  runApp $ do
    currentURI <- getCurrentURI
    startApp App { model = Model currentURI, initialAction = NoOp, ..}
  where
    update = updateModel
    events = defaultEvents
    subs   = [ uriSub HandleURI ]
    view   = viewModel
    mountPoint = Nothing
    logLevel = Off

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleURI u) m = m { uri = u } <# do
  pure NoOp
updateModel (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel _ m = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel model = view
  where
    view =
      either (const the404) id
        $ runRoute (Proxy :: Proxy API) handlers uri model
    handlers = about :<|> home
    home (_ :: Model) = div_ [] [
        div_ [] [ text "home" ]
      , button_ [ onClick goAbout ] [ text "go about" ]
      ]
    about (_ :: Model) = div_ [] [
        div_ [] [ text "about" ]
      , button_ [ onClick goHome ] [ text "go home" ]
      ]
    the404 = div_ [] [
        text "the 404 :("
      , button_ [ onClick goHome ] [ text "go home" ]
      ]

-- | Type-level routes
type API   = About :<|> Home
type Home  = View Action
type About = "about" :> View Action

-- | Type-safe links used in `onClick` event handlers to route the application
goAbout, goHome :: Action
(goHome, goAbout) = (goto api home, goto api about)
  where
#if MIN_VERSION_servant(0,10,0)
    goto a b = ChangeURI (linkURI (safeLink a b))
#else
    goto a b = ChangeURI (safeLink a b)
#endif
    home  = Proxy :: Proxy Home
    about = Proxy :: Proxy About
    api   = Proxy :: Proxy API

