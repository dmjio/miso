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
import Miso.Html (div_)
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

import HtmlGen2 (genHtml)
import HtmlGen (genBodyContent)

import Debug.Trace (trace)

type MainComponent = App TestData ()

type MainView = View TestData ()

data TestData = TestData
    { randomSeed :: Int
    } deriving (Generic, ToJSON, FromJSON, Eq)

app :: TestData -> MainComponent
app td =
    (component td update view)
        { M.initialAction = Just ()
        , M.logLevel      = DebugAll
        }

    where
        update _ = do
            io_ $ consoleLog "SUCCESS"
            return ()

        view :: TestData -> MainView
        view TestData { randomSeed = r } =
            unGen genHtml qcGen 30

            where
                qcGen = mkQCGen (trace ("mkQCGen with seed: " ++ show r) r)

        view2 :: TestData -> MainView
        view2 TestData { randomSeed = r } = div_ [] $
            unGen genBodyContent qcGen 30

            where
                qcGen = mkQCGen r
