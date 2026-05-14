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
    , io
    , io_
    , consoleLog
    , MisoString
    , fromMisoString
    )
import qualified Miso as M
import qualified Miso.Html as M
import qualified Miso.Html.Property as M
import Miso.Lens (Lens, lens, (%=))
import Miso.Binding ((-->), (<--))
import GHC.Generics
import Miso.JSON (FromJSON, ToJSON)
import Miso.DSL ((#), (!))

data Action
    = Initialize
    | ClickButtons
    | Assert
    | OnClick
    | AddB

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
        { M.logLevel = M.DebugAll
        , M.mount = Just Initialize
        }


rootView :: Int -> Model -> View Model Action
rootView depth m =
    M.div_ []
    (
        M.button_
            [ M.id_ "assert"
            , M.onClick Assert
            ]
            [ text "Assert" ]
        : M.button_
            [ M.id_ "CLICKME_CLICKME"
            , M.onClick OnClick
            ]
            [ text "Click Me" ]
        : modelElems (-1) m
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


clickById :: MisoString -> IO ()
clickById elemId = do
    e <- M.getElementById elemId
    _ <- e # ("click" :: MisoString) $ ([] :: [ MisoString ])
    pure ()


getTextContent :: MisoString -> IO (Maybe MisoString)
getTextContent elemId = do
    e <- M.getElementById elemId
    e ! "textContent" >>= M.fromJSVal


update :: Action -> Effect a Model Action
update OnClick = do
    io_ $ consoleLog "CLICKED"
    valueALens %= (+ 1)

update AddB = do
    io_ $ consoleLog "CLICKED B"
    valueBLens %= (+ 1)

update Initialize = io $ do
    consoleLog "TestBindingsApp INITALIZE"
    return ClickButtons

update ClickButtons = io_ $ do
    consoleLog "TestBindingsApp ClickButtons"
    clickById "CLICKME_CLICKME"
    clickById "click_innermost"
    clickById "assert"

update Assert = io_ $ do
    mValueA0Text <- getTextContent "valueA-0"
    mValueB0Text <- getTextContent "valueB-0"

    let
        result = do
            valueA0Text <- mValueA0Text
            valueB0Text <- mValueB0Text

            let valueA0 = read $ fromMisoString valueA0Text
                valueB0 = read $ fromMisoString valueB0Text

            return $ (valueA0, valueB0) == (expectedA, expectedB)

    case result of
        Just True ->
            consoleLog "SUCCESS"
        x -> do
            consoleLog "ERROR - TestBindingsApp Assertion FAIL"
            consoleLog $ toMisoString $ show x

    where
        expectedA = 1 :: Int
        expectedB = 2 :: Int


view :: Int -> Model -> View Model Action
view 0 m = M.div_ []
    (
        M.button_
            [ M.onClick OnClick
            , M.id_ "click_innermost"
            ]
            [ text "Clickme (innermost)" ]
        :
        modelElems 0 m
    )

view idx m =
    M.div_ []
        ( M.button_ [ M.onClick OnClick ] [ text "add a" ]
        : M.button_ [ M.onClick AddB ] [ text "add b" ]
        : modelElems idx m ++ [ mount_ $ innerApp (idx - 1) ]
        )


modelElems :: Int -> Model -> [ View Model Action ]
modelElems i m =
    [ M.div_ []
        [ M.span_ [] [ "a:" ]
        , M.span_
            [ M.id_ ("valueA-" <> toMisoString j) ]
            [ text (toMisoString $ show $ valueA m) ]
        ]
    , M.div_ []
        [ M.span_ [] [ "b:" ]
        , M.span_
            [ M.id_ ("valueB-" <> toMisoString j) ]
            [ text (toMisoString $ show $ valueB m) ]
        ]
    ]

    where
        j = i + 1
