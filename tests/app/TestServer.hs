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


-- testHtmlHydration :: EnvSettings -> IO ()
-- testHtmlHydration envSettings = do
--     let
--         -- html = Elem H1 [] [Elem Span [] [Elem Span [(Title,"6-H")] [Elem Em [(Class,"LmLO4Czl")] [VoidElem Wbr [(Title,"-.QVYNE")]],Elem Span [(Class,"ePZ-YyeK")] [Text "\1334\8362_"],Elem Span [(Class,"kBVC7Ik0zQu058"),(Title,"Os-,_4mmpl")] [Text "\1510&\242\128710\8691\12506\10089\8359"],Elem Strong [] [Text "\2356\2421d\8522"],Elem Span [(Title,"Pn2rg1l!B9")] [Text "\128206\127984\1301\12398\1093\1375\128719"],Elem Strong [(Title,"9_DMUaQ")] [VoidElem Br [(Class,"xy_hH6VhVaL5Ms2"),(Title,"0zy")]],Elem Span [(Class,"QnuOsYmAAoQfs5"),(Title,"P")] [Elem Span [] [Text "\9664\1291"],Elem Em [] [VoidElem Img [(Class,"i0Tx9eGMFM6"),(Title,"W"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Span [(Class,"ur7v-S"),(Title,"1y!6q,")] [VoidElem Img [(Class,"a3HmfHAa6Hclz1"),(Title,"2!hwS"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Strong [] [Elem Strong [] [VoidElem Br []],Elem Em [(Class,"EyeNkCWBpM_i5")] [Text "\8807\9648\128503\2425\9633\10110\8501\9893l"],Elem Strong [(Class,"JHiTcqsJkM")] [Elem Strong [(Class,"rtHRO"),(Title,"gthM9Wh")] [Text "\127819\1724IC"],Elem Em [(Class,"E")] [Text "L\128710\1303\12452\1782\1193A\9632"],Elem Em [(Title,"gl_JbHBn33")] [Elem Em [] [Elem Em [(Class,"aP-RGU"),(Title,"6")] [VoidElem Br []],Elem Strong [(Class,"KeuGxrRq")] [VoidElem Br []],Elem Strong [(Class,"oe1Ie_W-P"),(Title,"o,I")] [Elem Strong [(Title,"Zg")] [VoidElem Input [(Type,"password"),(Title,"31TJ"),(Value,"test-value")]],Elem Em [(Title,"0")] [Text "\9707a\40653\\\1366\128577\1312\8378\12441"],UserSuppliedElement]]]]]]]]]
--         html = Elem Em [] [Text "\\"]
--         -- Elem P [(Title,"rua,y_0o")] [Elem Span [] [Text "\22301\128565\206\1151\8886\10101\9770\9638\10120\&0\2390\8759\1083"],Elem Em [] [Text "\128733\128683\8617\128039\8495\1515\1171"],Elem Strong [] [Text "\128548\129319\9749\127764\2424\8371"],Elem Strong [] [Text "\1180\8393G\10129"],Elem Span [(Title,".")] [VoidElem Br [(Class,"XXy_qbW")]],Elem Strong [(Class,"o1AkUGVJnCWQn")] [Text "j3\128085\10051\8379D"],Elem Span [(Title,"?gq02?")] [VoidElem Input [(Type,"submit"),(Title,"FMIYqzU?"),(Value,"Submit")]],Elem Strong [(Class,"TFh"),(Title,"o209t.R0,L")] [VoidElem Input [(Type,"tel"),(Value,"+1 (555)-5555")]],Elem Strong [(Class,"T2lt")] [VoidElem Br []],Elem Span [] [Text "\10098"],Elem Span [] [Elem Span [(Title,"q?o0E")] [VoidElem Br [(Title,"!0Hw9_Alhr6v")]],Elem Span [(Class,"oOwYA6hu")] [VoidElem Input [(Type,"tel"),(Class,"bCji"),(Title,"?H,?2"),(Value,"+1 (555)-5555")]],Elem Strong [] [VoidElem Img [(Title,"?_p?-o7im03FP5"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Span [(Title,"I_77N?I")] [VoidElem Br [(Class,"i2TJt8"),(Title,"-??!VJ4")]],Elem Span [(Title,"0yWM73-340ZA")] [VoidElem Img [(Class,"ax9TN7"),(Title,"Ir"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Em [(Class,"kR8mXB-U")] [Text "\1319\129470\8365x\12429\9665\128670\1322\8647\8487\1043\1636\204\1049"],Elem Em [] [VoidElem Br [(Class,"g0qUbb1Ofy7kW")]],Elem Span [(Class,"moLy")] [VoidElem Br [(Title,"5_aoFy.hVA66!d")]],Elem Em [(Title,"8v.J")] [Elem Span [(Title,"O4")] [VoidElem Wbr [(Class,"zy2uv9NaRaWIB"),(Title,"E?f")]],Elem Strong [(Class,"jHTG8pX"),(Title,"B,QnO")] [Text "\8513\128519\&1j*\8927,\128655"],Elem Span [(Title,"W_we0W")] [VoidElem Input [(Type,"text"),(Class,"pCP93yjtiV"),(Title,"7f-R8!Am-ZRAq"),(Value,"test-value")]],Elem Em [] [Text "\8457\12509\&0\1223\1322\8735b\1207"],Elem Em [] [Text "\1324c\2326\1252\8655\8389\9763\8366\10011\1243\36401"],Elem Span [(Title,"xNpY.780izW.s77")] [VoidElem Input [(Type,"submit"),(Value,"Submit")]],Elem Strong [(Class,"tjE3o40zyEbAXRu")] [VoidElem Br [(Title,"_M9em")]],Elem Strong [(Class,"VF"),(Title,"X6ivn!wD2T!yO2")] [Elem Strong [(Class,"MKhACiUAgQ")] [Text "\1122\25205\20866\1707\12491"],Elem Em [(Class,"abDeu0ApaAnL1E")] [Text "\37407"],Elem Em [(Class,"Hrj-E")] [VoidElem Img [(Class,"AZTw-YC-ng0b"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Strong [(Class,"c")] [Elem Em [(Class,"qwBETfK9")] [Text "\12484\128667u\8378\127747\12447\2407\8653\2335\128659"],Elem Strong [(Title,"9n4J?_Q-?U132R-")] [Elem Strong [] [VoidElem Img [(Class,"XjjuiEqhrViO"),(Title,".5SdlJ_pl5?G7g"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Em [] [Elem Strong [(Title,"p_r__RHQ!-")] [VoidElem Wbr [(Class,"eCi")]],Elem Em [] [Text "\2390F\8384\128641\21103\2366\&2\9921\12360"],Elem Em [] [Text "\10115\9684\1205\128562\128557\2425\129481\1668\8610\1395\8744\9779T\2370\1131"],Elem Strong [] [Text "\128541\128554\2328\8499\2379\128719\9762m\8635\8522e\12404\&2"],Elem Span [(Title,"4!Z")] [VoidElem Wbr [(Title,"LVF0.m02O?1W")]],Elem Em [] [Text "\8462\1315\12524\9687\9904\8676\9651\10073\129440"],Elem Strong [] [Elem Em [(Title,"4eZ.97i!?vg-!")] [VoidElem Wbr [(Class,"kv_p_f"),(Title,"2?I-.i1L.5")]],Elem Strong [(Class,"mel")] [Text "\213\9723\9982\1302\8864\9647\8504\1175u\10015\1317\9954"],Elem Strong [] [VoidElem Img [(Class,"gLIA"),(Title,"_,!DT8FI84llS3"),(Src,"data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"),(Alt,"Test image")]],Elem Em [(Class,"StAN")] [Text "\9637\1328j\1474\1226\23929\1307\\\12391\8355q\1121"],Elem Em [] [VoidElem Wbr [(Class,"CuCu"),(Title,"eqqTwq1")]],Elem Span [(Title,"y")] [VoidElem Wbr [(Title,",,3G_qW,uL")]],Elem Span [(Class,"Xjz")] [Text "\1607\12414\1409\10051\128068\128726\9922"],Elem Strong [(Title,"F-9jWcc?")] [Elem Strong [(Title,"4fIdmc.a4zH_w")] [Text "\8780\12524\8370\1733\1206\9718\1666\1317"],Elem Strong [] [Text "\127950\128298O\12419\128573\1741"],Elem Span [] [Text "\2378\9688\128733\2336\&3\9691\128545C"],Elem Strong [(Class,"L1Hk")] [VoidElem Br []],Elem Span [(Class,"WBxSfTycC"),(Title,"2ep7XJ")] [Text "k\9691\1135\1303\2325\12532"],Elem Strong [(Class,"wRt6")] [VoidElem Wbr [(Class,"Col6SMGYqEk-")]],Elem Em [(Class,"RfzoV5cjq7i"),(Title,".P59vX")] [VoidElem Input [(Type,"tel"),(Value,"+1 (555)-5555")]],Elem Span [(Class,"N")] [Elem Strong [(Class,"HRPs8Ca")] [Text "\9639B\1162\10032\8698\128057\12493\2313\8658\1521"],Elem Strong [(Class,"eGX"),(Title,"?5-cdAD!p")] [Text "\9922"],Elem Strong [] [Elem Strong [(Class,"bwe"),(Title,"S.")] [Text "\128724\8736\1076\10073\8515\10016\12396\12360\1153\12475\2391\128532\1333"],Elem Span [(Class,"ux")] [Elem Em [(Class,"am"),(Title,"y,17L!3BOqB-")] [VoidElem Wbr []],Elem Em [(Title,"2D")] [Elem Em [(Class,"s")] [VoidElem Br [(Class,"UtRyGO91ZF")]],Elem Strong [(Class,"xWzzA1"),(Title,"67py05EvO3L")] [Text "\2408\8517s\129518"],Elem Span [(Title,"ZN")] [Elem Span [(Class,"x-Tfp8mO6R")] [Text "\1202\1223\128572"],UserSuppliedElement]]]]]]]]]]]]]]
--         -- uncaught exception in Haskell main thread: SyntaxError: Unexpected token 'で', ...":"▥԰jׂӊ嵹ԛ\で₣qѡ"}],"t"... is not valid JSON
--         appData = App.Model { App.randomHtml = html } :: App.Model
-- 
--     putStrLn "testHtmlHydration"
--     print html
--     print $ ((eitherDecode $ encode html) :: Either MisoString Html.HTML)
--     -- putStrLn $ fromMisoString $ encode $ toMisoString v
-- 
--     putStrLn $ "Beginning to listen on " <> show port_
--     Wai.run port_ $ Wai.logStdout
--         ( server
--             envSettings
--             (Proxy @(API App.Model App.Action))
--             (App.app appData)
--             (TestAppModel appData)
--         )
-- 
--     where
--         port_ = port envSettings


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
    -- testHtmlHydration envSettings
    runProps
        [ prop_testHtmlHydration envSettings
        , prop_testBindings envSettings
        ]
