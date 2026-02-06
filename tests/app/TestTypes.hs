{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestTypes where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

import qualified TestApp as TA

data TestData
    = TestAppModel TA.Model
    | TestBindingsModel Int
    deriving (Generic, ToJSON, FromJSON, Eq)
