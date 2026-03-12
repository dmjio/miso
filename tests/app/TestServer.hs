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
import Miso.String (toMisoString, fromMisoString)
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
    , arbitrary
    , ioProperty
    , Gen
    , Property
    , NonNegative (..), isSuccess, quickCheckResult
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
    , setSocketOption, bind, listen
    )
import System.Exit (exitSuccess, exitFailure)

import qualified HtmlGen as Html
import qualified TestApp as App
import qualified TestBindingsApp as AppB
import TestTypes (TestData (..))

import HtmlGen hiding (HTML)

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
    bind sock (addrAddress addrInfo)
    listen sock 2048
    return sock

    where
        hints =
            defaultHints
                { addrFlags = [ AI_PASSIVE ]
                , addrSocketType = Stream
                }


ok :: Int -> Bool
ok responseCode = 200 <= responseCode && responseCode <= 300


browserVsServer :: EnvSettings -> Wai.Application -> IO Bool
browserVsServer envSettings appServer = ok <$>
   bracket
      ( do
          putStrLn $ "Beginning to listen on " <> show port_
          sock <- openTCPSocketOnPort port_

          let settings = Wai.setPort port_ Wai.defaultSettings

          _ <- forkIO $ Wai.runSettingsSocket
                  settings
                  sock $
                  Wai.logStdout
                  appServer

          putStrLn "forked thread, returning sock"
          return sock
      )
      (\sock -> do
          putStrLn "Closing socket"
          close sock
      )
      ( const $ do
          -- threadDelay 30_000_000 -- one second
          httpGet $
              "http://localhost:"
              ++ show (playwrightPort envSettings)
              ++ "/test?port=" ++ show port_
              ++ "&wait=true"
      )

    where
        port_ = port envSettings


prop_testHtmlHydration :: EnvSettings -> Property
prop_testHtmlHydration envSettings = forAll (arbitrary :: Gen Html.HTML) $
    \html -> ioProperty $ do
        let appData = App.Model { App.randomHtml = html } :: App.Model
        browserVsServer envSettings
          ( server
              envSettings
              (Proxy @(API App.Model App.Action))
              (App.app appData)
              (TestAppModel appData)
          )



testHtmlHydration :: EnvSettings -> IO ()
testHtmlHydration envSettings = do
    let
        html = Elem H1 [] [Elem Span [] [Elem Span [(Title,"6-H")] [Elem Em [(Class,"LmLO4Czl")] [VoidElem Wbr [(Title,"-.QVYNE")]],Elem Span [(Class,"ePZ-YyeK")] [Text "\1334\8362_"],Elem Span [(Class,"kBVC7Ik0zQu058"),(Title,"Os-,_4mmpl")] [Text "\1510&\242\128710\8691\12506\10089\8359"],Elem Strong [] [Text "\2356\2421d\8522"],Elem Span [(Title,"Pn2rg1l!B9")] [Text "\128206\127984\1301\12398\1093\1375\128719"],Elem Strong [(Title,"9_DMUaQ")] [VoidElem Br [(Class,"xy_hH6VhVaL5Ms2"),(Title,"0zy")]],Elem Span [(Class,"QnuOsYmAAoQfs5"),(Title,"P")] [Elem Span [] [Text "\9664\1291"],Elem Em [] [VoidElem Img [(Class,"i0Tx9eGMFM6"),(Title,"W"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Span [(Class,"ur7v-S"),(Title,"1y!6q,")] [VoidElem Img [(Class,"a3HmfHAa6Hclz1"),(Title,"2!hwS"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Strong [] [Elem Strong [] [VoidElem Br []],Elem Em [(Class,"EyeNkCWBpM_i5")] [Text "\8807\9648\128503\2425\9633\10110\8501\9893l"],Elem Strong [(Class,"JHiTcqsJkM")] [Elem Strong [(Class,"rtHRO"),(Title,"gthM9Wh")] [Text "\127819\1724IC"],Elem Em [(Class,"E")] [Text "L\128710\1303\12452\1782\1193A\9632"],Elem Em [(Title,"gl_JbHBn33")] [Elem Em [] [Elem Em [(Class,"aP-RGU"),(Title,"6")] [VoidElem Br []],Elem Strong [(Class,"KeuGxrRq")] [VoidElem Br []],Elem Strong [(Class,"oe1Ie_W-P"),(Title,"o,I")] [Elem Strong [(Title,"Zg")] [VoidElem Input [(Type,"password"),(Title,"31TJ"),(Value,"test-value")]],Elem Em [(Title,"0")] [Text "\9707a\40653\\\1366\128577\1312\8378\12441"],UserSuppliedElement]]]]]]]]]
        appData = App.Model { App.randomHtml = html } :: App.Model
    putStrLn $ "Beginning to listen on " <> show port_
    Wai.run port_ $ Wai.logStdout
        ( server
            envSettings
            (Proxy @(API App.Model App.Action))
            (App.app appData)
            (TestAppModel appData)
        )

    where
        port_ = port envSettings


prop_testBindings :: EnvSettings -> Property
prop_testBindings envSettings = forAll (arbitrary :: Gen (NonNegative Int)) $
    \(NonNegative depth) -> ioProperty $ do
        -- putStrLn $ "Beginning to listen on " <> show port_
        -- Wai.run port_ $ Wai.logStdout
        --     ( server
        --         envSettings
        --         (Proxy @(API AppB.Model AppB.Action))
        --         (AppB.rootApp depth)
        --         (TestBindingsModel depth)
        --     )
        -- return $ 1 == (1 :: Int)

    -- where
    --     port_ = port envSettings

        browserVsServer envSettings
            ( server
                envSettings
                (Proxy @(API AppB.Model AppB.Action))
                (AppB.rootApp depth)
                (TestBindingsModel depth)
            )


runProps :: [Property] -> IO ()
runProps props = do
    results <- mapM quickCheckResult props
    if all isSuccess results
        then exitSuccess
        else exitFailure


main :: IO ()
main = do
    let s = "\9707a\40653\\\1366\128577\1312\8378\12441" :: MisoString
    let v = "\9707a\40653\\\1366\128577\1312\8378\12441" :: String
    putStrLn v
    putStrLn $ fromMisoString $ encode s
    putStrLn $ fromMisoString $ encode $ toMisoString v
    --exitFailure

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
    testHtmlHydration envSettings
    -- runProps
    --     [ prop_testHtmlHydration envSettings
    --     -- , prop_testBindings envSettings
    --     ]
