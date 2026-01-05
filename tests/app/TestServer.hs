{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

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
    , link_
    , body_
    , script_
    )
import Miso.Html.Property
    ( charset_
    , name_
    , content_
    , rel_
    , href_
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
import Data.Aeson (ToJSON)
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

data IndexPageData = forall b. (ToJSON b) => IndexPageData (b, App.MainComponent)

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

                , title_ [] [ "Chandlr" ]

                , js_wasm $ static_root <> "/init.js"
                -- , js_js $ static_root <> "/all.js" -- Uncomment this and comment out the previous line to load the javascript version (TODO: make this a commandline flag or something)
                , css $ static_root <> "/style.css"
                ]
            , body_ [] [ mount (app :: App.MainComponent) ]
            ]
        ]

        where
            static_root :: MisoString
            static_root = "/static"

            css href =
                link_
                    [ rel_ "stylesheet"
                    , type_ "text/css"
                    , href_ $ toMisoString href
                    ]

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
server serve_static_dir_path initial_data =
    serve
        (Proxy @API)
        (staticHandler :<|> mainView initial_data)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer serve_static_dir_path


mainView :: App.TestData -> Handler IndexPageData
mainView initial_data = pure $ IndexPageData (initial_data, App.app)

prop_testIO :: Property
prop_testIO = forAll (chooseAny :: Gen Int) $
    \i -> ioProperty $ do
        print i
        return $ i == i

testMain :: IO ()
testMain = do
    putStrLn "Begin Quickchecks"
    quickCheck prop_testIO


main :: IO ()
main = do
    cwd <- getCurrentDirectory

    let serve_static_dir_path = cwd <> "/static"

    portStr <- lookupEnv "PORT"
    let port = maybe 8888 read portStr

    let initialData = App.TestData { App.randomSeed = 1 } :: App.TestData

    putStrLn $ "Beginning to listen on " <> show port

    testMain

    -- Wai.run port $ Wai.logStdout (server serve_static_dir_path initialData)
