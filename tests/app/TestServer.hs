{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

import Prelude hiding (writeFile, readFile)
import System.Directory (getCurrentDirectory)
import Data.Proxy
import Servant.Server
    ( Server
    , serve
    , Handler
    )
import qualified Network.Wai as Wai
import Miso.Html
    ( ToHtml (..)
    , doctype_
    , html_
    , head_
    , meta_
    , body_
    , script_
    )
import Miso.Html.Property
    ( charset_
    , name_
    , content_
    , type_
    , class_
    , src_
    , language_
    , defer_
    )
import Miso.Html.Element (title_)
import qualified Servant
import Servant.API
import Miso.String (toMisoString)
import Servant.Miso.Html (HTML)
import Miso
    ( MisoString
    , mount
    )
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Data.Text.Lazy (toStrict)
import Data.Aeson.Text (encodeToLazyText)
import System.Environment (lookupEnv)
import Test.QuickCheck
    ( forAll
    , quickCheck
    , arbitrary
    , ioProperty
    , Gen
    , Property
    )
import Data.Aeson (encode, decode)
import Control.Concurrent (forkIO, killThread)
import Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    , httpLbs
    , parseRequest
    , Response (responseStatus)
    , responseBody
    )
import Network.HTTP.Types (statusCode)
import Data.ByteString.Lazy (writeFile, readFile)
import Control.Monad (unless)
import Data.Maybe (fromJust)

import qualified HtmlGen3 as Html
import qualified TestApp as App

type ServerRoutes = Routes (Get '[HTML] IndexPageData)

newtype IndexPageData = IndexPageData (App.TestData, App.MainComponent)

type RouteIndexPage a = a
type Routes a = RouteIndexPage a

type StaticRoute = "static" :> Servant.Raw

type API = StaticRoute :<|> ServerRoutes

instance ToHtml IndexPageData where
    toHtml (IndexPageData (initial_data, app)) = toHtml
        [ doctype_
        , html_
            []
            [ head_
                []
                [ meta_ [ charset_ "utf-8" ]
                , meta_
                    [ name_ "viewport"
                    , content_ "width=device-width, initial-scale=1.0"
                    ]
                , script_
                    [ class_ "initial-data"
                    , type_ "application/json"
                    ]
                    (toMisoString $ toStrict $ encodeToLazyText initial_data)

                , title_ [] [ "Miso Tests" ]

                -- , js_wasm $ static_root <> "/init.js"
                , js_js $ static_root <> "/all.js" -- Uncomment this and comment out the previous line to load the javascript version (TODO: make this a commandline flag or something)
                ]
            , body_ [] [ mount (app :: App.MainComponent) ]
            ]
        ]

        where
            static_root :: MisoString
            static_root = "/static"

            js_wasm href =
                script_
                    [ type_ "module"
                    , src_ $ toMisoString href
                    ]
                    ""

            js_js href =
                script_
                    [ language_ "javascript"
                    , src_ $ toMisoString href
                    , defer_ "true"
                    ]
                    ""


server :: FilePath -> App.TestData -> Wai.Application
server serve_static_dir_path_ appData =
    serve
        (Proxy @API)
        (staticHandler :<|> mainView appData)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer serve_static_dir_path_


mainView :: App.TestData -> Handler IndexPageData
mainView appData = pure $ IndexPageData (appData, App.app appData)


httpGet :: String -> IO Int
httpGet url = do
    httpManager <- newManager defaultManagerSettings
    request <- parseRequest url
    response <- httpLbs request httpManager
    print $ responseBody response
    return $ statusCode $ responseStatus response


failFilename :: FilePath
failFilename = "/tmp/failing_case.json"


prop_testIO :: EnvSettings -> Property
prop_testIO envSettings = forAll (arbitrary :: Gen Html.HTML) $
    \html -> ioProperty $ do
        let appData = App.TestData { App.randomHtml = html } :: App.TestData

        putStrLn $ "Beginning to listen on " <> show port_
        serverTid <- forkIO $ Wai.run port_ $ Wai.logStdout (server serve_static_dir_path_ appData)

        playwrightResponse <- httpGet $ "http://localhost:" ++ show (playwrightPort envSettings) ++ "/test?port=" ++ show port_ ++ "&wait=true"

        killThread serverTid

        let ok = 200 <= playwrightResponse && playwrightResponse <= 300

        unless ok $
            writeFile failFilename (encode html)

        return ok

    where
        port_ = port envSettings
        serve_static_dir_path_ = serve_static_dir_path envSettings


data EnvSettings = EnvSettings
    { serve_static_dir_path :: FilePath
    , port :: Int
    , playwrightPort :: Int
    }


serveFailed :: EnvSettings -> IO ()
serveFailed envSettings = do
    bytes <- readFile failFilename
    let html = fromJust $ decode bytes

    let appData = App.TestData { App.randomHtml = html } :: App.TestData

    putStrLn $ "Beginning to listen on " <> show port_
    Wai.run port_ $ Wai.logStdout (server serve_static_dir_path_ appData)

    where
        port_ = port envSettings
        serve_static_dir_path_ = serve_static_dir_path envSettings



main :: IO ()
main = do
    staticDir_ <- lookupEnv "STATIC_DIR"
    staticDir <-
            case staticDir_ of
                Nothing -> do
                    cwd <- getCurrentDirectory
                    return $ cwd <> "/static"
                Just d -> return d



    portStr <- lookupEnv "PORT"
    playwrightPortStr <- lookupEnv "PLAYWRIGHT_PORT"

    let envSettings = EnvSettings
            { serve_static_dir_path = staticDir
            , port = maybe 8888 read portStr
            , playwrightPort = maybe 8888 read playwrightPortStr
            }

    -- serveFailed envSettings
    putStrLn "Begin Quickchecks"
    quickCheck $ prop_testIO envSettings
