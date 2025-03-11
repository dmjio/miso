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
import           Servant.HTML.Lucid
import qualified System.IO as IO

import           Miso hiding (run, SSE)

type Website      = StaticFiles :<|> SSE :<|> ServerRoutes :<|> The404
type ServerRoutes = Get '[HTML] (Page (Component "sse" Model Action))
type StaticFiles  = "static" :> Raw
type SSE          = "sse" :> Raw
type The404       = Raw

app :: Chan ServerEvent -> Application
app chan = serve (Proxy @Website) website
  where
    website
      = serveDirectoryFileServer "static"
      :<|> Tagged (eventSourceAppChan chan)
      :<|> pure (Page (sseComponent goHome))
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

-- | Page for setting HTML doctype and header
newtype Page a = Page a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (Page a) where
  toHtmlRaw = L.toHtml
  toHtml (Page x) =
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

handle404 :: Application
handle404 _ respond
  = respond
  $ responseLBS status404 [("Content-Type", "text/html")]
  $ renderBS
  $ toHtml
  $ Page
  $ the404
