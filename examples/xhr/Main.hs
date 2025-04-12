{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.State
import           Data.Aeson
import qualified Data.Map as M
import           Data.Maybe
import           GHC.Generics
import           Language.Javascript.JSaddle (eval, fromJSValUnchecked)

import           Miso hiding (defaultOptions)
import           Miso.String
import           Miso.Lens

-- | Model
newtype Model = Model
  { _info :: Maybe APIInfo
  } deriving (Eq, Show)

info :: Lens Model (Maybe APIInfo)
info = lens _info $ \r x -> r { _info = x }

-- | Action
data Action
  = FetchGitHub
  | SetGitHub APIInfo
  deriving (Show, Eq)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Main entry point
main :: IO ()
main = run (startApp app)

app :: App Effect Model Action ()
app = defaultApp emptyModel updateModel viewModel

emptyModel :: Model
emptyModel = Model Nothing

-- | Update your model
updateModel :: Action -> Effect Model Action ()
updateModel FetchGitHub =
  io (SetGitHub <$> getGitHubAPIInfo)
updateModel (SetGitHub apiInfo) =
  info ?= apiInfo

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model{..} = view
  where
    view =
        div_
            [ style_ $
                M.fromList
                    [ (pack "text-align", pack "center")
                    , (pack "margin", pack "200px")
                    ]
            ]
            [ h1_ [class_ $ pack "title"] [text $ pack "Miso XHR Example"]
            , button_
                attrs
                [ text $ pack "Fetch JSON from https://api.github.com via XHR"
                ]
            , case info of
                Nothing -> div_ [] [text $ pack "No data"]
                Just APIInfo{..} ->
                    table_
                        [class_ $ pack "table is-striped"]
                        [ thead_
                            []
                            [ tr_
                                []
                                [ th_ [] [text $ pack "URLs"]
                                ]
                            ]
                        , tbody_
                            []
                            [ tr_ [] [td_ [] [text current_user_url]]
                            , tr_ [] [td_ [] [text emojis_url]]
                            , tr_ [] [td_ [] [text emails_url]]
                            , tr_ [] [td_ [] [text events_url]]
                            , tr_ [] [td_ [] [text gists_url]]
                            , tr_ [] [td_ [] [text feeds_url]]
                            , tr_ [] [td_ [] [text followers_url]]
                            , tr_ [] [td_ [] [text following_url]]
                            ]
                        ]
            ]
      where
        attrs =
            [ onClick FetchGitHub
            , class_ $ pack "button is-large is-outlined"
            ]
                ++ [disabled_ True | isJust info]

data APIInfo = APIInfo
    { current_user_url :: MisoString
    , current_user_authorizations_html_url :: MisoString
    , authorizations_url :: MisoString
    , code_search_url :: MisoString
    , commit_search_url :: MisoString
    , emails_url :: MisoString
    , emojis_url :: MisoString
    , events_url :: MisoString
    , feeds_url :: MisoString
    , followers_url :: MisoString
    , following_url :: MisoString
    , gists_url :: MisoString
    , hub_url :: MisoString
    , issue_search_url :: MisoString
    , issues_url :: MisoString
    , keys_url :: MisoString
    , notifications_url :: MisoString
    , organization_repositories_url :: MisoString
    , organization_url :: MisoString
    , public_gists_url :: MisoString
    , rate_limit_url :: MisoString
    , repository_url :: MisoString
    , repository_search_url :: MisoString
    , current_user_repositories_url :: MisoString
    , starred_url :: MisoString
    , starred_gists_url :: MisoString
    , user_url :: MisoString
    , user_organizations_url :: MisoString
    , user_repositories_url :: MisoString
    , user_search_url :: MisoString
    } deriving (Show, Eq, Generic)

instance FromJSON APIInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

getGitHubAPIInfo :: IO APIInfo
getGitHubAPIInfo = do
  json <- fromJSValUnchecked =<< eval "fetch('https://api.github.com').then(response => response.json())"
  case eitherDecodeStrict json :: Either String APIInfo of
    Left s -> error s
    Right j -> pure j
