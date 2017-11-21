{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Data.Proxy

import Miso

-- 'runRoute' requires that our model includes the current URI.
instance HasURI Model where
  lensURI = makeLens getter setter
    where
      getter = modelUri
      setter = \m u -> m { modelUri = u }

main :: IO ()
main = do
  currentURI <- getCurrentURI
  miso
    App {initialAction = NoOp, model = Model currentURI "No event received", ..}
  where
    update = updateModel
    view
      -- If 'runRoute' fails, we fall back to displaying a 404 page.
     =
      either (const the404) id . runRoute (Proxy :: Proxy ClientRoutes) handlers
    events = defaultEvents
    subs = [sseSub "/sse" handleSseMsg, uriSub HandleURI]
    mountPoint = Nothing

handleSseMsg :: SSE String -> Action
handleSseMsg (SSEMessage msg) = ServerMsg msg
handleSseMsg SSEClose = ServerMsg "SSE connection closed"
handleSseMsg SSEError = ServerMsg "SSE error"

updateModel :: Action -> Model -> Effect Action Model
updateModel (ServerMsg msg) m = noEff (m {modelMsg = "Event received: " ++ msg})
updateModel (HandleURI u) m = m {modelUri = u} <# pure NoOp
updateModel (ChangeURI u) m = m <# (pushURI u >> pure NoOp)
updateModel NoOp m = noEff m
