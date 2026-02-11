{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module TestTypes where

import GHC.Generics
#ifdef VANILLA
import Data.Aeson (FromJSON, ToJSON)
#else
import Miso.JSON (FromJSON, ToJSON)
#endif

import qualified TestApp as TA

data TestData
    = TestAppModel TA.Model
    | TestBindingsModel Int
    deriving (Generic, ToJSON, FromJSON, Eq)
