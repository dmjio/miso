{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Miso
    ( consoleLog
    , miso
    , MisoString
    , fromMisoString
    , toMisoString
    , defaultEvents
    )
import Miso.JSON (decode)
import Miso.DSL
    ( jsg
    , (#)
    , fromJSVal
    , JSVal
    , isNull
    , isUndefined
    , (!)
    )

import qualified TestApp as App
import qualified TestBindingsApp as AppB
import TestTypes

import HtmlGen
import qualified Miso.JSON as MJ

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype ParentNode = ParentNode JSVal

getDocument :: IO Document
getDocument = Document <$> jsg ("document" :: MisoString)

querySelector :: ParentNode -> MisoString -> IO (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # "querySelector" $ [s]) >>= maybeNullOrUndefined)

    where
        maybeNullOrUndefined x = do
            nullYes <- isNull x

            if nullYes then
                return Nothing
            else do
                undefYes <- isUndefined x

                if undefYes then
                    return Nothing
                else
                    return $ Just x


textContent :: Element -> IO (Maybe MisoString)
textContent (Element e) = e ! "textContent" >>= fromJSVal

getScriptContents :: MisoString -> IO (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e

main :: IO ()
main = do
    -- let
    --     html  = Elem Ol [(Title,"IQb")] []
    --     html' = (MJ.eitherDecode $ MJ.encode html) :: Either MisoString HTML

    -- consoleLog $ MJ.encode html
    -- consoleLog $ toMisoString $ show $ (Right html) == html'

    -- case html' of
    --     Left s -> consoleLog s
    --     Right _ -> consoleLog "html encode/decode sanity test successfull"

    let
        str  = "\9707a\40653\\\1366\128577\1312\8378\12441" :: MisoString
        str' = (MJ.eitherDecode $ MJ.encode str) :: Either MisoString MisoString

    consoleLog $ MJ.encode str
    consoleLog $ toMisoString $ show $ (Right str) == str'

    case str' of
        Left s -> consoleLog s
        Right _ -> consoleLog "str encode/decode sanity test successfull"


    consoleLog "Hello World"

    rawTestData <- getScriptContents "initial-data"

    let mTestData = decode =<< rawTestData

    case mTestData of
        Nothing -> consoleLog "ERROR - client couldn't load initial-data"
        Just (TestAppModel appData) -> do
            consoleLog "TestClient - load App TestAppModel"
            miso defaultEvents $ const $ App.app appData
        Just (TestBindingsModel depth) -> do
            consoleLog $ "TestClient - load AppB TestBindingsModel " <> toMisoString (show depth)
            miso defaultEvents $ const $ AppB.rootApp depth
