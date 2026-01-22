{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

import Data.Int (Int32)
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
    , chooseAny
    , ioProperty
    , Gen
    , Property
    )

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


prop_testIO :: EnvSettings -> Property
prop_testIO envSettings = forAll (chooseAny :: Gen Int32) $
    \i -> ioProperty $ do
        print i
        let appData = App.TestData { App.randomSeed = fromEnum i } :: App.TestData

        putStrLn $ "Beginning to listen on " <> show port_
        Wai.run port_ $ Wai.logStdout (server serve_static_dir_path_ appData)
        return $ i == i

    where
        port_ = port envSettings
        serve_static_dir_path_ = serve_static_dir_path envSettings


data EnvSettings = EnvSettings
    { serve_static_dir_path :: FilePath
    , port :: Int
    }


main :: IO ()
main = do
    cwd <- getCurrentDirectory

    portStr <- lookupEnv "PORT"

    let envSettings = EnvSettings
            { serve_static_dir_path = cwd <> "/static"
            , port = maybe 8888 read portStr
            }

    putStrLn "Begin Quickchecks"
    quickCheck $ prop_testIO envSettings
