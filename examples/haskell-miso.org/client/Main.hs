{-# LANGUAGE RecordWildCards #-}
module Main where

import Common
import Data.Proxy

import Miso
import Miso.String

main :: IO ()
main = miso $ \currentURI -> App
        { model = Model currentURI False
        , view = viewModel
        , ..
        }
    where
      initialAction = NoOp
      mountPoint = Nothing
      update = updateModel
      events = defaultEvents
      subs = [ uriSub ChangeURI ]
      viewModel m = LocatedView (Just (uri m)) $
        case runRoute (Proxy :: Proxy ClientRoutes) handlers uri m of
          Left _ -> the404 m
          Right v -> v

updateModel :: Action -> Model -> Effect Action Model
updateModel (ChangeURI u) m = m { uri = u, navMenuOpen = False } <# do
  pure NoOp
updateModel Alert m@Model{..} = m <# do
  alert $ pack (show uri)
  pure NoOp
updateModel ToggleNavMenu m@Model{..} = m { navMenuOpen = not navMenuOpen } <# do
  pure NoOp
updateModel NoOp m = noEff m
