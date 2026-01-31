{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module TestApp where

import Miso
    ( App
    , component
    , View
    , consoleLog
    , io_
    , LogLevel (..)
    , Effect
    , ROOT
    )
import qualified Miso as M
import qualified Miso.Html as M
import qualified Miso.Html.Property as M
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Language.Javascript.JSaddle ((#), JSString)

import HtmlGen (HTML, render)

data Action = Initialize | Clicked

type MainComponent = App TestData Action

type MainView = View TestData Action

data TestData = TestData
    { randomHtml :: HTML
    } deriving (Generic, ToJSON, FromJSON, Eq)

app :: TestData -> MainComponent
app td =
    (component td update view)
        { M.initialAction = Just Initialize
        , M.logLevel      = DebugAll
        }

    where
        update :: Action -> Effect ROOT TestData Action
        update Clicked =
            io_ $ consoleLog "SUCCESS"

        update Initialize = io_ $ do
            mElem <- M.getElementById "CLICKME_CLICKME"
            mElem # ("click" :: JSString) $ ([] :: [ JSString ])

        view :: TestData -> MainView
        view TestData { randomHtml = html } = render aElem html

        aElem = M.a_
            [ M.id_ "CLICKME_CLICKME"
            , M.onClick Clicked
            ]
            [ M.text "Click on this Element" ]
