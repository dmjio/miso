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
    , MisoString
    )
import Miso.DSL ((#))
import qualified Miso as M
import qualified Miso.Html as M
import qualified Miso.Html.Property as M
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad (void)

import HtmlGen (HTML, render)

data Action = Initialize | Clicked

type MainComponent = App Model Action

type MainView = View Model Action

newtype Model = Model
    { randomHtml :: HTML
    } deriving (Generic, ToJSON, FromJSON, Eq)

app :: Model -> MainComponent
app td =
    (component td update view)
        { M.mount    = Just Initialize
        , M.logLevel = DebugAll
        }

    where
        update :: Action -> Effect ROOT Model Action
        update Clicked =
            io_ $ consoleLog "SUCCESS"

        update Initialize = io_ $ do
            mElem <- M.getElementById "CLICKME_CLICKME"
            void $ mElem # ("click" :: MisoString) $ ([] :: [ MisoString ])

        view :: Model -> MainView
        view Model { randomHtml = html } = render aElem html

        aElem = M.a_
            [ M.id_ "CLICKME_CLICKME"
            , M.onClick Clicked
            ]
            [ M.text "Click on this Element" ]
