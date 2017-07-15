{-# LANGUAGE RecordWildCards #-}
module Main where

import Common
import Data.Proxy
import Network.URI

import Miso
import Miso.String

instance HasURI Model where
  lensURI = makeLens getter setter
    where
      getter = uri
      setter = \m u -> m { uri = u }

main :: IO ()
main = do
  currentURI <- getCurrentURI
  miso App { model = Model currentURI, ..}
    where
      initialAction = NoOp
      update = updateModel
      events = defaultEvents
      subs = [ uriSub HandleURI ]
      view m =
        either (const $ the404 m) id $
          runRoute (Proxy :: Proxy ClientRoutes) handlers m

updateModel :: Action -> Model -> Effect Model Action
updateModel (HandleURI u) m = m { uri = u } <# do
  pure NoOp
updateModel (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel Alert m@Model{..} = m <# do
  alert $ pack (show uri)
  pure NoOp
updateModel NoOp m = noEff m
