module Miso.Storage 
  ( getFromStorage
  ) where

import Data.Aeson
import Miso.Html.Internal

getFromStorage
  :: FromJSON model
  => MisoString
  -> IO (Either String model)
getFromStorage -- key =
  = undefined
  -- Just w <- currentWindow
  -- Just s <- getLocalStorage w
  -- maybeVal <- S.getItem s key
  -- pure $ case maybeVal of
  --   Nothing -> Left "Not found"
  --   Just m -> eitherDecode (CS.cs (m :: T.Text))

