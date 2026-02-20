----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
import Miso
    ( View
    , App
    , Effect
    , component
    , text
    , Component
    , toMisoString
    , mount_
    , io_
    , consoleLog
    , miso
    , defaultEvents
    )
----------------------------------------------------------------------------
import qualified Miso as M
import qualified Miso.Html as M
import qualified Miso.Html.Property as M
import Miso.Lens (Lens, lens, (%=))
import Miso.Binding ((-->), (<--))
import GHC.Generics
import Miso.JSON (FromJSON, ToJSON)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = miso defaultEvents $ const $ rootApp 0
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

type Action = ()

data Model = Model
    { valueA :: Int
    , valueB :: Int
    } deriving (Generic, ToJSON, FromJSON, Eq)


valueALens :: Lens Model Int
valueALens = lens valueA (\m x -> m { valueA = x })


valueBLens :: Lens Model Int
valueBLens = lens valueB (\m x -> m { valueB = x })


type AppComponent = Component Model Model Action


rootApp :: Int -> App Model Action
rootApp depth =
    (component initialModel update_ (rootView depth))
        { M.logLevel = M.DebugAll }


rootView :: Int -> Model -> View Model Action
rootView depth m =
    M.div_ []
    (
        M.button_
            [ M.id_ "CLICKME"
            , M.onClick ()
            ]
            [ text "Click Me" ]
        : modelElems m
        ++ [ mount_ $ innerApp depth ]
    )


innerApp :: Int -> AppComponent
innerApp 0 =
    (component initialModel update_ (view_ 0))
        { M.logLevel = M.DebugAll
        , M.bindings =
            [ valueALens --> valueALens
            , valueBLens <-- valueALens
            ]
        }

innerApp idx =
    (component initialModel update_ (view_ idx))
        { M.logLevel = M.DebugAll
        , M.bindings =
            [ valueALens --> valueALens
            , valueBLens <-- valueBLens
            ]
        }


initialModel :: Model
initialModel = Model 0 0


update_ :: Action -> Effect a Model Action
update_ = const $ do
    io_ $ consoleLog "CLICKED"
    valueALens %= (+ 1)


view_ :: Int -> Model -> View Model Action
view_ 0 m = M.div_ []
    (
        M.button_
            [ M.onClick () ]
            [ text "Clickme (innermost)" ]
        :
        modelElems m
    )
view idx m =
    M.div_ []
        ( modelElems m
        ++
        [ mount_ $ innerApp (idx - 1) ]
        )


modelElems :: Model -> [ View Model Action ]
modelElems m =
    [ M.div_ []
        [ text (toMisoString $ show $ valueA m)
        ]
    , M.div_ []
        [ text (toMisoString $ show $ valueB m)
        ]
    ]

