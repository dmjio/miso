{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestApp where

import Miso
    ( App
    , component
    , View
    )
import Miso.Html (div_)
import GHC.Generics
import Data.Aeson (ToJSON)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

import HtmlGen2 (genHtml)
import HtmlGen (genBodyContent)

type MainComponent = App TestData ()

type MainView = View TestData ()

data TestData = TestData
    { randomSeed :: Int
    } deriving (Generic, ToJSON, Eq)

app :: TestData -> MainComponent
app td =
    component td update view

    where
        update = const $ return ()

        view :: TestData -> MainView
        view TestData { randomSeed = r } =
            unGen genHtml qcGen 30

            where
                qcGen = mkQCGen r

        view2 :: TestData -> MainView
        view2 TestData { randomSeed = r } = div_ [] $
            unGen genBodyContent qcGen 30

            where
                qcGen = mkQCGen r


-- TODO:
--  Take Elements module and make a data structure from the defs
--  do the same with props list i guess
--  need to do this because we need a function to generate
--  appropriate attributes for an Element
--      - actually still don't need to do this
--        can just define appropriate pairs of function, attribute generator
--  Make a graph from the data structure
--  Render graph
