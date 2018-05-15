{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Data.Proxy

import Miso

main :: IO ()
main = do
  miso $ \currentURI ->
    App { initialAction = NoOp
        , model = Model currentURI "No event received"
        , ..
        }
  where
    update = updateModel
    view m = LocatedView (Just (modelUri m)) $
             case runRoute (Proxy :: Proxy ClientRoutes) handlers modelUri m of
               Left _ -> the404
               Right m -> m
    events = defaultEvents
    subs   = [ sseSub "/sse" handleSseMsg
             , uriSub ChangeURI
             ]
    mountPoint = Nothing

handleSseMsg :: SSE String -> Action
handleSseMsg (SSEMessage msg) = ServerMsg msg
handleSseMsg SSEClose = ServerMsg "SSE connection closed"
handleSseMsg SSEError = ServerMsg "SSE error"

updateModel :: Action -> Model -> Effect Action Model
updateModel (ServerMsg msg) m = pure (m {modelMsg = "Event received: " ++ msg})
updateModel (ChangeURI u) m = m {modelUri = u} <# pure NoOp
updateModel NoOp m = noEff m
