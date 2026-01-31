{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Miso
    ( consoleLog
    , run
    , miso
    , MisoString
    , fromMisoString
    , toMisoString
    , JSM
    )
import Language.Javascript.JSaddle
    (JSVal
    , js
    , fromJSVal
    , maybeNullOrUndefined
    , (#)
    , jsg
    )
import Control.Lens.Operators ((^.))
import Data.Aeson (decodeStrict)

import qualified TestApp as App

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype ParentNode = ParentNode JSVal

getDocument :: JSM Document
getDocument = Document <$> jsg ("document" :: MisoString)

querySelector :: ParentNode -> MisoString -> JSM (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # ("querySelector" :: MisoString) $ [s]) >>= maybeNullOrUndefined)

textContent :: Element -> JSM (Maybe MisoString)
textContent (Element e) = (e ^. js ("textContent" :: MisoString)) >>= fromJSVal

getScriptContents :: MisoString -> JSM (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e

main :: IO ()
main = run $ do
    consoleLog "Hello World"

    rawTestData <- getScriptContents "initial-data"

    -- consoleLog $ "stringified data: " <> toMisoString (show rawTestData)

    let mTestData =
            (decodeStrict . fromMisoString) =<< rawTestData

    case mTestData of
        Nothing -> consoleLog "ERROR - client couldn't load initial-data"
        Just testData ->
            -- consoleLog $ toMisoString $ encode testData
            miso $ const $ App.app testData
