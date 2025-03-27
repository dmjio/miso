{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Common

import Control.Concurrent
import Control.Monad
import Data.Binary.Builder
import Data.Monoid ((<>))
import Data.Proxy
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Servant
import qualified System.IO as IO

import Miso hiding (SSE, run)

type Website = StaticFiles :<|> SSE :<|> ServerRoutes :<|> The404
type ServerRoutes = Get '[HTML] Page
type StaticFiles = "static" :> Raw
type SSE = "sse" :> Raw
type The404 = Raw

app :: Chan ServerEvent -> Application
app chan = serve (Proxy @Website) website
  where
    website =
        serveDirectoryFileServer "static"
            :<|> Tagged (eventSourceAppChan chan)
            :<|> pure (Page (sse goHome))
            :<|> Tagged handle404

port :: Int
port = 3003

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr ("Running on port " <> show port <> "...")
    chan <- newChan
    _ <- forkIO (sendEvents chan)
    run port $ logStdout (compress (app chan))
  where
    compress = gzip def{gzipFiles = GzipCompress}

-- Send 1 event/s containing the current server time
sendEvents :: Chan ServerEvent -> IO ()
sendEvents chan =
    forever $ do
        time <- getCurrentTime
        writeChan
            chan
            (ServerEvent Nothing Nothing [putStringUtf8 (show (show time))])
        threadDelay (10 ^ (6 :: Int))

-- | Page for setting HTML doctype and header
newtype Page = Page (App Effect Model Action ())

instance ToHtml Page where
    toHtml (Page x) =
        toHtml
            [ doctype_
            , head_
                []
                [ meta_ [charset_ "utf-8"]
                , jsRef "static/all.js" -- Include the frontend
                ]
            , body_ [] [toView x]
            ]
      where
        jsRef href =
            script_
                [ src_ href
                , async_ "true"
                , defer_ "true"
                ]
                ""

handle404 :: Application
handle404 _ respond =
    respond $
        responseLBS status404 [("Content-Type", "text/html")] $
            toHtml the404
