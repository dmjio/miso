-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso
import Miso.Lens
import Miso.String
-----------------------------------------------------------------------------
data Action = ServerMsg MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
type Model = MisoString
-----------------------------------------------------------------------------
viewModel :: Model -> View Action
viewModel msg =
  div_
    []
    [ h3_
      []
      [ text "SSE (Server-sent events) Example"
      ]
    , text (ms msg)
    ]
-----------------------------------------------------------------------------
sse :: Component Model Action
sse = (component "No event received" updateModel viewModel)
  { subs = [ sseSub "https://echo.websocket.org/.sse" handleSseMsg ]
  }
-----------------------------------------------------------------------------
handleSseMsg :: SSE MisoString -> Action
handleSseMsg = \case
  SSEMessage msg -> ServerMsg msg
  SSEClose -> ServerMsg "SSE connection closed"
  SSEError -> ServerMsg "SSE error"
-----------------------------------------------------------------------------
updateModel :: Action -> Effect Model Action
updateModel (ServerMsg msg) = do
  io_ (consoleLog ("Received: " <> ms msg))
  _id .= msg
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run (startComponent sse)
-----------------------------------------------------------------------------
