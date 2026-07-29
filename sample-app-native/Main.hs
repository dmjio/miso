-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE StaticPointers              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso hiding (text_)
import           Miso.Native
import           Miso.Native.X.Element (input_)
import           Miso.Native.X.Element.Input (onInput, InputEvent(..), placeholder_)
import           Miso.Native.Element.ScrollView (scrollOrientation_, enableScroll_)
import           Miso.Native.Element.View.Event (onTap)
import           Miso.Fetch (getJSON, Response(..))
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON, FromJSON, Value, encode)
import           Miso.String
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
import           GHC.Generics
-----------------------------------------------------------------------------
-- | Application actions. These are all *background* handlers (no 'mainThread'),
-- because the fetch and the model update run on the BTS.
data Action
  = SetQuery MisoString
  -- ^ input changed; stash the typed @owner/repo@ in the model.
  | Fetch
  -- ^ button tapped; GET the repo JSON from GitHub.
  | Got Value
  -- ^ response body (the JSON text, or GitHub's @{"message":"Not Found"}@).
  | Failed
  -- ^ the request itself errored (network failure).
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
data Status
  = Idle
  | Loading
  | Loaded Value
  | Errored
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
data Model = Model
  { query  :: MisoString
  , result :: Status
  } deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
-- | Type @owner/repo@ (e.g. @dmjio/miso@), tap Fetch, and the GitHub API JSON
-- is printed below. A bad repo prints GitHub's own @{"message":"Not Found"}@.
main :: IO ()
main = startApp (nativeEvents <> nativeXEvents) (static (mount_ fetchComponent))
-----------------------------------------------------------------------------
fetchComponent :: App Model Action
fetchComponent = component (Model "dmjio/miso" Idle) updateModel viewModel
-----------------------------------------------------------------------------
githubUrl :: MisoString -> MisoString
githubUrl repo = "https://api.github.com/repos/" <> repo
-----------------------------------------------------------------------------
-- | Error callback for 'getText'; typed here so @getText@'s @error@ variable is
-- unambiguous (we ignore the error body and just flag failure).
onError :: Response MisoString -> Action
onError _ = Failed
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  SetQuery q ->
    modify $ \m -> m { query = q }
  Fetch -> do
    repo <- gets query
    io_ $ do
      consoleLog "sending repo"
      consoleLog repo
    modify $ \m -> m { result = Loading }
    -- runs on the BTS: fetch the raw JSON text, dispatch Got / Failed
    getJSON (githubUrl repo) [] (Got . body) onError
  Got json ->
    modify $ \m -> m { result = Loaded json }
  Failed ->
    modify $ \m -> m { result = Errored }
-----------------------------------------------------------------------------
viewModel :: context -> props -> Model -> View context Action
viewModel _ _ Model{..} = view_
  [ CSS.style_
    [ CSS.height "100vh"
    , CSS.width "100%"
    , CSS.display "flex"
    , CSS.flexDirection "column"
    ]
  ]
  [ input_
    [ event (static (onInput (SetQuery . inputValue)))
    , placeholder_ "owner/repo  (e.g. dmjio/miso)"
    , CSS.style_
      [ CSS.height "56px"
      , CSS.width "100%"
      , CSS.flexShrink 0
      , CSS.fontSize "20px"
      , CSS.backgroundColor CSS.lightgray
      ]
    ]
  , view_
    [ event (static (onTap Fetch))
    , CSS.style_
      [ CSS.height "56px"
      , CSS.width "100%"
      , CSS.flexShrink 0
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.backgroundColor CSS.blue
      ]
    ]
    [ text_ [ CSS.style_ [ CSS.fontSize "22px", CSS.color CSS.gold ] ] [ text "Fetch" ] ]
  , scrollView_
    [ scrollOrientation_ "vertical"
    , enableScroll_ True
    , CSS.style_
      [ CSS.flexGrow 1
      , CSS.width "100%"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      ]
    ]
    [ text_
      [ CSS.style_ [ CSS.fontSize "15px", CSS.flexShrink 0 ] ]
      [ text (renderStatus result) ]
    ]
  ]
-----------------------------------------------------------------------------
renderStatus :: Status -> MisoString
renderStatus = \case
  Idle       -> "Type owner/repo above and tap Fetch."
  Loading    -> "Loading\8230"
  Loaded json -> encode json
  Errored    -> "not found (request failed)"
-----------------------------------------------------------------------------
