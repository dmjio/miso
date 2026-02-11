{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestTypes where

import GHC.Generics
import Miso.JSON (FromJSON, ToJSON)

import qualified TestApp as TA

data TestData
    = TestAppModel TA.Model
    | TestBindingsModel Int
    deriving (Generic, ToJSON, FromJSON, Eq)
