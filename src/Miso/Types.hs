{-# LANGUAGE DeriveGeneric #-}
module Miso.Types where

import GHC.Generics

-- Shared types
type Coords = (Int, Int)
type Action m = m -> m
type Sink m = Action m -> IO ()
type Sub m = Sink m -> IO ()

data Options = Options {
    preventDefault :: Bool
  , stopPropagation :: Bool
  } deriving (Show, Eq, Generic)

defaultOptions :: Options
defaultOptions = Options False False

newtype AllowDrop = AllowDrop Bool deriving (Show, Eq)
