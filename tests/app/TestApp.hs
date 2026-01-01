{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestApp where

import Miso
    ( App
    , component
    , text
    )
import Miso.Html
    ( div_
    )
import GHC.Generics
import Data.Aeson (ToJSON)

type MainComponent = App () ()

data TestData = TestData
    { randomSeed :: Int
    } deriving (Generic, ToJSON)

app :: MainComponent
app = component initialModel update view
    where
        initialModel = ()
        update = const $ return ()
        view = const $ div_ [] [ text "Hello World" ]
