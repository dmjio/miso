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
    consoleLog "Hello World"

    rawTestData <- getScriptContents "initial-data"

    -- consoleLog $ "stringified data: " <> toMisoString (show rawTestData)

    let mTestData = decode =<< rawTestData

    case mTestData of
        Nothing -> consoleLog "ERROR - client couldn't load initial-data"
        Just (TestAppModel appData) ->
            miso defaultEvents $ const $ App.app appData
        Just (TestBindingsModel depth) ->
            miso defaultEvents $ const $ AppB.rootApp depth
