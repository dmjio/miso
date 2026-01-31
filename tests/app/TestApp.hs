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
    )
import qualified Miso as M
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

import HtmlGen (HTML, render)

type MainComponent = App TestData ()

type MainView = View TestData ()

data TestData = TestData
    { randomHtml :: HTML
    } deriving (Generic, ToJSON, FromJSON, Eq)

app :: TestData -> MainComponent
app td =
    (component td update view)
        { M.initialAction = Just ()
        , M.logLevel      = DebugAll
        }

    where
        update _ =
            io_ $ consoleLog "SUCCESS"

        view :: TestData -> MainView
        view TestData { randomHtml = html } = render html
