{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestTypes where

import Miso (MisoString)
import Miso.JSON (FromJSON, ToJSON)
import GHC.Generics

import qualified TestApp as TA

data TestData
    = TestAppModel TA.Model
    | TestBindingsModel Int
    deriving (Eq, Generic, FromJSON, ToJSON)
