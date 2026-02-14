{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import Miso (MisoString)
import Miso.JSON
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , (.:)
    , (.=)
    , object
    , Parser
    )

import qualified TestApp as TA

data TestData
    = TestAppModel TA.Model
    | TestBindingsModel Int
    deriving Eq

instance ToJSON TestData where
    toJSON (TestAppModel x) =
        object
            [ "tag"      .= ("TestAppModel" :: MisoString)
            , "contents" .= x
            ]
    toJSON (TestBindingsModel n) =
        object
            [ "tag"      .= ("TestBindingsModel" :: MisoString)
            , "contents" .= n
            ]

instance FromJSON TestData where
    parseJSON (Object obj) = do
        tag <- (obj .: "tag") :: Parser MisoString

        case tag of
            "TestAppModel"      -> TestAppModel      <$> ((obj .: "contents") :: Parser TA.Model)
            "TestBindingsModel" -> TestBindingsModel <$> obj .: "contents"
            _ -> fail "Unknown constructor tag for TestData"

    parseJSON _ = fail "Expected JSON object for TestData"
