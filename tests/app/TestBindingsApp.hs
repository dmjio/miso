{-
 -
 -
 - Generate a cyclical grpah: A -> B -> C -> A
 -      Let's say A gets clicked, changes A.a,
 -      B is bound to A.a, C is bound to B.a,
 -      C is bound to A.b.
 -          - does binding C and A happen in A or in C?
 -
 - Click gets caught in A's update
 -      - after state is changed, use notify to schedule an Action
 -      - in that update (for the next action, still in A),
 -          assert that A.b has been changed to A.a
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module TestBindingsApp where

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
    )
import qualified Miso as M
import qualified Miso.Html as M
import qualified Miso.Html.Property as M
import Miso.Lens (Lens, lens, (%=))
import Miso.Binding ((-->), (<--))
import GHC.Generics
import Miso.JSON (FromJSON, ToJSON)

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
    (component initialModel update (rootView depth))
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
    (component initialModel update (view 0))
        { M.logLevel = M.DebugAll
        , M.bindings =
            [ valueALens --> valueALens
            , valueBLens <-- valueALens
            ]
        }

innerApp idx =
    (component initialModel update (view idx))
        { M.logLevel = M.DebugAll
        , M.bindings =
            [ valueALens --> valueALens
            , valueBLens <-- valueBLens
            ]
        }


initialModel :: Model
initialModel = Model 0 0


update :: Action -> Effect a Model Action
update = const $ do
    io_ $ consoleLog "CLICKED"
    valueALens %= (+ 1)


view :: Int -> Model -> View Model Action
view 0 m = M.div_ []
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
