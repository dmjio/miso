{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
   ( windowAddEventListener
   , windowRemoveEventListener
   , windowInnerHeight
   , windowInnerWidth
   , now
   , consoleLog
   , stringify
   , parseJSValAsJson
   , unsafeParseJSValAsJson
   , unsafeParseJsonString
   , parseJsonString
   , parseJsonToJSVal
   , copyDOMIntoVTree
   , item
   , jsvalToValue
   , delegateEvent
   , clearBody
   ) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                 as AE
import           Data.Aeson                 hiding (Object)
import qualified Data.HashMap.Strict        as H
import           Data.JSString
import qualified Data.JSString.Text         as JSS
import           Data.Maybe
import           Data.Scientific
import qualified Data.Vector                as V
import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array.Internal
import qualified JavaScript.Object.Internal as OI
import           Unsafe.Coerce

-- | Convert JSVal to Maybe `Value`
jsvalToValue :: JSVal -> IO (Maybe Value)
jsvalToValue r = do
  case jsonTypeOf r of
    JSONNull -> return (Just Null)
    JSONInteger -> liftM (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer))
         <$> fromJSVal r
    JSONFloat -> liftM (AE.Number . (fromFloatDigits :: Double -> Scientific))
         <$> fromJSVal r
    JSONBool -> liftM AE.Bool <$> fromJSVal r
    JSONString -> liftM AE.String <$> fromJSVal r
    JSONArray -> do
      xs :: [Value] <-
        catMaybes <$>
          forM (toList (unsafeCoerce r)) jsvalToValue
      pure . pure $ Array . V.fromList $ xs
    JSONObject -> do
        Just (props :: [JSString]) <- fromJSVal =<< getKeys (OI.Object r)
        runMaybeT $ do
            propVals <- forM props $ \p -> do
              v <- MaybeT (jsvalToValue =<< OI.getProp p (OI.Object r))
              return (JSS.textFromJSString p, v)
            return (AE.Object (H.fromList propVals))

-- | Retrieves keys
foreign import javascript unsafe "$r = Object.keys($1);"
  getKeys :: OI.Object -> IO JSVal

-- | Adds event listener to window
foreign import javascript unsafe "window.addEventListener($1, $2);"
  windowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

-- | Removes event listener from window
foreign import javascript unsafe "window.removeEventListener($1, $2);"
  windowRemoveEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

-- | Retrieves inner height
foreign import javascript unsafe "$r = window.innerHeight;"
  windowInnerHeight :: IO Int

-- | Retrieves outer height
foreign import javascript unsafe "$r = window.innerWidth;"
  windowInnerWidth :: IO Int

-- | Retrieve high performance time stamp
foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

-- | Console-logging
foreign import javascript unsafe "console.log($1);"
  consoleLog :: JSVal -> IO ()

-- | Converts a JS object into a JSON string
foreign import javascript unsafe "$r = JSON.stringify($1);"
  stringify' :: JSVal -> IO JSString

-- | Parse a raw string into JSON. Might return an exception result, so
-- should only be used by 'parseJson' which will wrap the result in an Either.
foreign import javascript unsafe
  "try {$r = JSON.parse($1);} catch (e) {$r = e;}"
  parseJson' :: JSString -> IO JSVal

-- | Check if a value is an error
foreign import javascript unsafe "$r = $1 instanceof Error;"
  jsvalIsError' :: JSVal -> IO Bool

-- | Return an exception's message
foreign import javascript unsafe "$r = $1.message;"
  getErrorMessage' :: JSVal -> IO JSString

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> IO JSString
{-# INLINE stringify #-}
stringify j = stringify' =<< toJSVal (toJSON j)

-- | Parse a string as JSON, checking for parse errors.
parseJsonToJSVal :: JSString -> IO (Either JSString JSVal)
parseJsonToJSVal s = do
  result <- parseJson' s
  jsvalIsError' result >>= \case
    True -> Left <$> getErrorMessage' result
    False -> pure $ Right result

-- | Parses a string, throwing errors as exceptions.
unsafeParseJsonString :: FromJSON json => JSString -> IO json
{-# INLINE unsafeParseJsonString #-}
unsafeParseJsonString s = parseJsonToJSVal s >>= \case
  Left err -> error ("When parsing JSON: " ++ unpack err)
  Right parsed -> do
    Just val <- jsvalToValue parsed
    case fromJSON val of
      Error err' -> error ("When parsing JSON: " ++ err')
      Success obj -> pure obj

-- | Parses a JSVal as a JSON string. This will fail if the input is
-- not a valid input to JSON.parse, if it's not valid JSON, or if it
-- doesn't parse as a member of the output type.
unsafeParseJSValAsJson :: FromJSON json => JSVal -> IO json
{-# INLINE unsafeParseJSValAsJson #-}
unsafeParseJSValAsJson = unsafeParseJsonString . unsafeCoerce

-- | Parse a JSVal as Json, assuming it's a string.
parseJSValAsJson :: FromJSON json => JSVal -> IO (Result json)
parseJSValAsJson = parseJsonString . unsafeCoerce

-- | Parse a JSVal into a result, so that it can be checked.
parseJsonString :: FromJSON obj => JSString -> IO (Result obj)
{-# INLINE parseJsonString #-}
parseJsonString s = parseJsonToJSVal s >>= \case
  Left err -> pure $ Error (unpack err)
  Right jval -> jsvalToValue jval >>= \case
    Nothing -> pure $ Error "Couldn't convert JSVal to Value"
    Just val -> pure $ fromJSON val

-- | Indexing into a JS object
foreign import javascript unsafe "$r = $1[$2];"
  item :: JSVal -> JSString -> IO JSVal

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree :: JSVal -> IO ()

-- | Event delegation FFI, routes events received on body through the virtual dom
-- Invokes event handler when found
foreign import javascript unsafe "delegate($1, $2);"
  delegateEvent
     :: JSVal               -- ^ Events
     -> Callback (IO JSVal) -- ^ Virtual DOM callback
     -> IO ()

-- | Clear the document body. This is particularly useful to avoid
-- creating multiple copies of your app when running in GHCJSi.
foreign import javascript unsafe "document.body.innerHTML = '';"
  clearBody :: IO ()
