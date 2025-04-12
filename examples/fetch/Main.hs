{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.Map as M
import           Data.Maybe
import           GHC.Generics
import           Language.Javascript.JSaddle (JSM)
import           Data.Proxy
import           Servant.API
----------------------------------------------------------------------------
import           Miso hiding (defaultOptions)
import           Miso.String
import           Miso.Lens
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | Main entry point
main :: IO ()
main = run $ startApp app
  { styles =
    [ Href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"
    , Href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    ]
  }
----------------------------------------------------------------------------
-- | Model
newtype Model = Model
  { _info :: Maybe GitHub
  } deriving (Eq, Show)
----------------------------------------------------------------------------
-- | Lens for info field
info :: Lens Model (Maybe GitHub)
info = lens _info $ \r x -> r { _info = x }
----------------------------------------------------------------------------
-- | Action
data Action
  = FetchGitHub
  | SetGitHub GitHub
  | ErrorHandler MisoString
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | WASM support
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
app :: App Effect Model Action ()
app = defaultApp emptyModel updateModel viewModel
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model Nothing
----------------------------------------------------------------------------
-- | GitHub API method
type GithubAPI = Get '[JSON] GitHub
----------------------------------------------------------------------------
-- | Uses servant to reify typesafe calls to the Fetch API
getGithubAPI
  :: (GitHub -> JSM ())
  -- ^ Successful callback
  -> (MisoString -> JSM ())
  -- ^ Errorful callback
  -> JSM ()
getGithubAPI = fetch (Proxy @GithubAPI) "https://api.github.com"
----------------------------------------------------------------------------
updateModel :: Action -> Effect Model Action ()
updateModel FetchGitHub
  = tell
  [ \snk -> getGithubAPI (snk . SetGitHub) (snk . ErrorHandler)
  ]
updateModel (SetGitHub apiInfo) =
  info ?= apiInfo
updateModel (ErrorHandler msg) =
  io (consoleError msg)
----------------------------------------------------------------------------
-- | View function, with routing
viewModel :: Model -> View Action
viewModel m = view
  where
    view =
      div_
      [ style_ $ M.fromList
        [ (pack "text-align", pack "center")
        , (pack "margin", pack "200px")
        ]
      ]
      [ h1_
        [ class_ $ pack "title"
        ]
        [ text $ pack "Miso Fetch Example"
        ]
      , button_
        attrs
        [ text $ pack "Fetch JSON from https://api.github.com via Fetch API"
        ]
      , case m ^. info of
          Nothing ->
            div_
            []
            [ "No data"
            ]
          Just GitHub {..} ->
            table_
            [ class_ "table is-striped" ]
            [ thead_
              []
              [ tr_
                []
                [ th_
                  []
                  [ text "URLs"
                  ]
                ]
              ]
            , tbody_
              []
              [ tr current_user_url
              , tr emojis_url
              , tr emails_url
              , tr events_url
              , tr gists_url
              , tr feeds_url
              , tr followers_url
              , tr following_url
              ]
            ]
      ]

    tr :: MisoString -> View action
    tr x = tr_ [] [ td_ [] [ text x ] ]

    attrs :: [Attribute Action]
    attrs =
          [ onClick FetchGitHub
          , class_ (pack "button is-large is-outlined")
          ] ++
          [ disabled_ True
          | isJust (m ^. info)
          ]
----------------------------------------------------------------------------
-- | Structure to capture the JSON returned from https://api.github.com
data GitHub
  = GitHub
  { current_user_url                     :: MisoString
  , current_user_authorizations_html_url :: MisoString
  , authorizations_url                   :: MisoString
  , code_search_url                      :: MisoString
  , commit_search_url                    :: MisoString
  , emails_url                           :: MisoString
  , emojis_url                           :: MisoString
  , events_url                           :: MisoString
  , feeds_url                            :: MisoString
  , followers_url                        :: MisoString
  , following_url                        :: MisoString
  , gists_url                            :: MisoString
  , hub_url                              :: MisoString
  , issue_search_url                     :: MisoString
  , issues_url                           :: MisoString
  , keys_url                             :: MisoString
  , notifications_url                    :: MisoString
  , organization_repositories_url        :: MisoString
  , organization_url                     :: MisoString
  , public_gists_url                     :: MisoString
  , rate_limit_url                       :: MisoString
  , repository_url                       :: MisoString
  , repository_search_url                :: MisoString
  , current_user_repositories_url        :: MisoString
  , starred_url                          :: MisoString
  , starred_gists_url                    :: MisoString
  , user_url                             :: MisoString
  , user_organizations_url               :: MisoString
  , user_repositories_url                :: MisoString
  , user_search_url                      :: MisoString
  } deriving (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON GitHub where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
----------------------------------------------------------------------------
