{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import           Common
import           Control.Concurrent
import           Control.Monad
import           Data.Binary.Builder
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Time.Clock
import qualified Lucid as L
import           Lucid.Base
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.EventSource
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           Servant
import qualified System.IO as IO


import           Miso

port :: Int
port = 3003

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr ("Running on port " <> show port <> "...")
  chan <- newChan
  _ <- forkIO (sendEvents chan)
  run port $ logStdout (compress (app chan))
    where
      compress = gzip def { gzipFiles = GzipCompress }

-- Send 1 event/s containing the current server time
sendEvents :: Chan ServerEvent -> IO ()
sendEvents chan =
  forever $ do
    time <- getCurrentTime
    writeChan
      chan
      (ServerEvent Nothing Nothing [putStringUtf8 (show (show time))])
    threadDelay (10 ^ (6 :: Int))

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) =
    L.doctypehtml_ $ do
      L.head_ $ do
        L.meta_ [L.charset_ "utf-8"]
        jsRef "static/all.js" -- Include the frontend
      L.body_ (L.toHtml x)
    where
      jsRef href =
        L.with
          (L.script_ mempty)
          [ makeAttribute "src" href
          , makeAttribute "async" mempty
          , makeAttribute "defer" mempty
          ]

type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

handle404 :: Application
handle404 _ respond =
  respond $
  responseLBS status404 [("Content-Type", "text/html")] $
  renderBS $
  toHtml $
  Wrapper $ the404

type API = "static" :> Raw :<|> "sse" :> Raw :<|> ServerRoutes :<|> Raw

app :: Chan ServerEvent -> Application
app chan =
  serve
    (Proxy @API)
#if MIN_VERSION_servant(0,11,0)
    (static :<|> Tagged (sseApp chan) :<|> (serverHandlers :<|> Tagged handle404))
#else
    (static :<|> sseApp chan :<|> (serverHandlers :<|> handle404))
#endif
  where
    static = serveDirectory "static"

sseApp :: Chan ServerEvent -> Application
sseApp chan = eventSourceAppChan chan

serverHandlers :: Server ServerRoutes
serverHandlers = homeHandler
  where
    send f u =
      pure $ Wrapper $ f Model {modelUri = u, modelMsg = "No event received"}
    homeHandler = send home goHome
