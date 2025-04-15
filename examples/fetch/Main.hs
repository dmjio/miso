-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
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
app :: App Model Action
app = defaultApp emptyModel updateModel viewModel
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model Nothing
----------------------------------------------------------------------------
-- | GitHub API method
type GithubAPI = Get '[JSON] GitHub
----------------------------------------------------------------------------
-- | Uses servant to reify type-safe calls to the Fetch API
getGithubAPI
  :: (GitHub -> JSM ())
  -- ^ Successful callback
  -> (MisoString -> JSM ())
  -- ^ Errorful callback
  -> JSM ()
getGithubAPI = fetch (Proxy @GithubAPI) "https://api.github.com"
----------------------------------------------------------------------------
updateModel :: Action -> Effect Model Action ()
updateModel FetchGitHub = withSink $ \snk -> getGithubAPI (snk . SetGitHub) (snk . ErrorHandler)
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
        [ "🍜 Miso Fetch API"
        ]
      , button_
        attrs
        [ "Fetch JSON from https://api.github.com"
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
              [ tr currentUserUrl
              , tr emojisUrl
              , tr emailsUrl
              , tr eventsUrl
              , tr gistsUrl
              , tr feedsUrl
              , tr followersUrl
              , tr followingUrl
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
  { currentUserUrl                   :: MisoString
  , currentUserAuthorizationsHtmlUrl :: MisoString
  , authorizationsUrl                :: MisoString
  , codeSearchUrl                    :: MisoString
  , commitSearchUrl                  :: MisoString
  , emailsUrl                        :: MisoString
  , emojisUrl                        :: MisoString
  , eventsUrl                        :: MisoString
  , feedsUrl                         :: MisoString
  , followersUrl                     :: MisoString
  , followingUrl                     :: MisoString
  , gistsUrl                         :: MisoString
  , hubUrl                           :: MisoString
  , issueSearchUrl                   :: MisoString
  , issuesUrl                        :: MisoString
  , keysUrl                          :: MisoString
  , notificationsUrl                 :: MisoString
  , organizationRepositoriesUrl      :: MisoString
  , organizationUrl                  :: MisoString
  , publicGistsUrl                   :: MisoString
  , rateLimitUrl                     :: MisoString
  , repositoryUrl                    :: MisoString
  , repositorySearchUrl              :: MisoString
  , currentUserRepositoriesUrl       :: MisoString
  , starredUrl                       :: MisoString
  , starredGistsUrl                  :: MisoString
  , userUrl                          :: MisoString
  , userOrganizationsUrl             :: MisoString
  , userRepositoriesUrl              :: MisoString
  , userSearchUrl                    :: MisoString
  } deriving (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON GitHub where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
----------------------------------------------------------------------------
