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
    , App
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
import Data.Aeson (encode, decode, ToJSON)
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
import Control.Exception (bracket)

import qualified HtmlGen as Html
import qualified TestApp as App
import qualified TestBindingsApp as AppB

data Backend = GHCJS | WASM deriving (Show, Read)

data EnvSettings = EnvSettings
    { serve_static_dir_path :: FilePath
    , port :: Int
    , playwrightPort :: Int
    , backend :: Backend
    } deriving Show

type ServerRoutes model action = Routes (Get '[HTML] (IndexPageData model action))

data IndexPageData model action =
    (ToJSON model, Eq model) => IndexPageData (Backend, model, App model action)

type RouteIndexPage a = a
type Routes a = RouteIndexPage a

type StaticRoute = "static" :> Servant.Raw

type API model action = StaticRoute :<|> ServerRoutes model action

instance ToHtml (IndexPageData model action) where
    toHtml (IndexPageData (backend_, initial_data, app)) = toHtml
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
                , js backend_
                ]
            , body_ [] [ mount app ]
            ]
        ]

        where
            static_root :: MisoString
            static_root = "/static"

            js WASM = js_wasm $ static_root <> "/init_integration_wasm_client.js"
            js GHCJS = js_js $ static_root <> "/all.js"

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


server
    :: (ToJSON model, Eq model)
    => EnvSettings
    -> Proxy (API model action)
    -> App model action
    -> model
    -> Wai.Application
server envSettings apiProxy app appData =
    serve
        apiProxy
        (staticHandler :<|> mainHandler envSettings app appData)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer (serve_static_dir_path envSettings)


mainHandler
    :: (ToJSON model, Eq model)
    => EnvSettings
    -> App model action
    -> model
    -> Handler (IndexPageData model action)
mainHandler envSettings app appData = pure $
    IndexPageData (backend envSettings, appData, app)


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
        -- Wai.run port_ $ Wai.logStdout (server envSettings serve_static_dir_path_ appData)

        playwrightResponse <- bracket
            ( forkIO $ Wai.run port_ $ Wai.logStdout
                ( server
                    envSettings
                    (Proxy @(API App.TestData App.Action))
                    (App.app appData)
                    appData
                )
            )
            killThread
            ( const $
                httpGet $
                    "http://localhost:"
                    ++ show (playwrightPort envSettings)
                    ++ "/test?port=" ++ show port_
                    ++ "&wait=true"
            )

        let ok = 200 <= playwrightResponse && playwrightResponse <= 300

        unless ok $
            writeFile failFilename (encode html)

        return ok

    where
        port_ = port envSettings


prop_testBindings :: EnvSettings -> Property
prop_testBindings envSettings = forAll (arbitrary :: Gen Int) $
    \nnodes -> ioProperty $ do
        putStrLn $ "Beginning to listen on " <> show port_
        Wai.run port_ $ Wai.logStdout
            ( server
                envSettings
                (Proxy @(API Int AppB.Action))
                (AppB.app nnodes)
                nnodes
            )
        return $ 1 == (1 :: Int)

    where
        port_ = port envSettings



serveFailed :: EnvSettings -> IO ()
serveFailed envSettings = do
    bytes <- readFile failFilename
    let html = fromJust $ decode bytes

    let appData = App.TestData { App.randomHtml = html } :: App.TestData

    putStrLn $ "Beginning to listen on " <> show port_
    Wai.run port_ $ Wai.logStdout
        ( server
            envSettings
            (Proxy @(API App.TestData App.Action))
            (App.app appData)
            appData
        )

    where
        port_ = port envSettings


main :: IO ()
main = do
    staticDir_ <- lookupEnv "STATIC_DIR"
    staticDir <-
            case staticDir_ of
                Nothing -> do
                    cwd <- getCurrentDirectory
                    return $ cwd <> "/static"
                Just d -> return d

    portStr           <- lookupEnv "PORT"
    playwrightPortStr <- lookupEnv "PLAYWRIGHT_PORT"
    backendStr        <- lookupEnv "BACKEND"

    let envSettings = EnvSettings
            { serve_static_dir_path = staticDir
            , port = maybe 8888 read portStr
            , playwrightPort = maybe 8889 read playwrightPortStr
            , backend = maybe GHCJS read backendStr
            }

    putStrLn $ "TestServer settings: " ++ show envSettings

    -- serveFailed envSettings
    putStrLn "Begin Quickchecks"
    quickCheck $ prop_testIO envSettings
    quickCheck $ prop_testBindings envSettings
    -- quickCheck $ prop_testBindings (envSettings { port = port envSettings + 1 })
