{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Miso
import Miso.String

-- | Model
data Model
  = Model
  { lat :: Double
  , lon :: Double
  } deriving (Eq, Show)

-- | Action
data Action
  = HandleGeo Geo
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  startApp App { model = Model 0 0, initialAction = NoOp, ..}
  where
    update = updateModel
    events = defaultEvents
    subs   = [ geoSub defGeoOptions HandleGeo ]
    view   = viewModel

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleGeo (GeoCoords lat lon)) _ =
  noEff (Model lat lon)
updateModel (HandleGeo (GeoError msg code)) m = m <# do
  print (msg, code)
  pure NoOp
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel Model {..} =
  div_
  []
  [ text $ ms (show lat) <> " " <> ms (show lon) ]

