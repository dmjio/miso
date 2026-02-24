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
    , mount_
    , App
    )
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import System.Environment (lookupEnv)
import Test.QuickCheck
    ( forAll
    , quickCheck
    , arbitrary
    , ioProperty
    , Gen
    , Property
    , NonNegative (..)
    )
import Miso.JSON (ToJSON, encode)
import Control.Concurrent (forkIO)
import Network.HTTP.Client
    ( defaultManagerSettings
    , newManager
    , httpLbs
    , parseRequest
    , Response (responseStatus)
    , responseBody
    )
import Network.HTTP.Types (statusCode)
import Control.Exception (bracket)
import Network.Socket
    ( Socket
    , openSocket
    , defaultHints
    , close
    , AddrInfoFlag (AI_PASSIVE)
    , SocketType (Stream)
    , getAddrInfo
    , AddrInfo (..)
    , SocketOption (ReuseAddr)
    , setSocketOption
    )

import qualified HtmlGen as Html
import qualified TestApp as App
import qualified TestBindingsApp as AppB
import TestTypes (TestData (..))

-- Misnomer - this is the backend the front-end was compiled with
data Backend = GHCJS | WASM deriving (Show, Read)

data EnvSettings = EnvSettings
    { serve_static_dir_path :: FilePath
    , port :: Int
    , playwrightPort :: Int
    , backend :: Backend
    } deriving Show

type ServerRoutes model action =
    Routes (Get '[HTML] (IndexPageData model action))

data IndexPageData model action =
    (ToJSON model, Eq model) =>
    IndexPageData (Backend, TestData, App model action)

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
                    (encode initial_data)
                    --(toMisoString $ toStrict $ encodeToLazyText initial_data)

                , title_ [] [ "Miso Tests" ]
                , js backend_
                ]
            , body_ [] [ mount_ app ]
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
                    , defer_ True
                    ]
                    ""


server
    :: (ToJSON model, Eq model)
    => EnvSettings
    -> Proxy (API model action)
    -> App model action
    -> TestData
    -> Wai.Application
server envSettings apiProxy app testData =
    serve
        apiProxy
        (staticHandler :<|> mainHandler envSettings app testData)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer (serve_static_dir_path envSettings)


mainHandler
    :: (ToJSON model, Eq model)
    => EnvSettings
    -> App model action
    -> TestData
    -> Handler (IndexPageData model action)
mainHandler envSettings app testData = pure $
    IndexPageData (backend envSettings, testData, app)


httpGet :: String -> IO Int
httpGet url = do
    httpManager <- newManager defaultManagerSettings
    request <- parseRequest url
    response <- httpLbs request httpManager
    print $ responseBody response
    return $ statusCode $ responseStatus response


openTCPSocketOnPort :: Int -> IO Socket
openTCPSocketOnPort p = do
    addrInfo:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- openSocket addrInfo
    setSocketOption sock ReuseAddr 1
    return sock

    where
        hints =
            defaultHints
                { addrFlags = [ AI_PASSIVE ]
                , addrSocketType = Stream
                }


prop_testIO :: EnvSettings -> Property
prop_testIO envSettings = forAll (arbitrary :: Gen Html.HTML) $
    \html -> ioProperty $ do
        let appData = App.Model { App.randomHtml = html } :: App.Model

        putStrLn $ "Beginning to listen on " <> show port_
        -- Wai.run port_ $ Wai.logStdout (server envSettings serve_static_dir_path_ appData)

        playwrightResponse <- bracket
            ( do
                sock <- openTCPSocketOnPort port_

                let settings = Wai.setPort port_ Wai.defaultSettings

                _ <- forkIO $ Wai.runSettingsSocket settings sock $ Wai.logStdout
                    ( server
                        envSettings
                        (Proxy @(API App.Model App.Action))
                        (App.app appData)
                        (TestAppModel appData)
                    )

                return sock
            )
            (\sock -> do
                -- putStrLn "KILLING THREAD"
                putStrLn "Closing socket"
                close sock
                -- killThread tid
            )
            ( const $
                httpGet $
                    "http://localhost:"
                    ++ show (playwrightPort envSettings)
                    ++ "/test?port=" ++ show port_
                    ++ "&wait=true"
            )

        let ok = 200 <= playwrightResponse && playwrightResponse <= 300

        return ok

    where
        port_ = port envSettings


prop_testBindings :: EnvSettings -> Property
prop_testBindings envSettings = forAll (arbitrary :: Gen (NonNegative Int)) $
    \(NonNegative depth) -> ioProperty $ do
        putStrLn $ "Beginning to listen on " <> show port_
        Wai.run port_ $ Wai.logStdout
            ( server
                envSettings
                (Proxy @(API AppB.Model AppB.Action))
                (AppB.rootApp depth)
                (TestBindingsModel depth)
            )
        return $ 1 == (1 :: Int)

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

    putStrLn "Begin Quickchecks"
    quickCheck $ prop_testIO envSettings
    quickCheck $ prop_testBindings envSettings
