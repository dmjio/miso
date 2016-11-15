{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Effect.XHR where

import Data.JSString
import GHCJS.Foreign.Callback
import GHCJS.Nullable
import GHCJS.Types
import Prelude                hiding (lines)

data ReadyState
  = UNSENT
  -- ^ Client has been created. open() not called yet.
  | OPENED
  -- ^ open() has been called.
  | HEADERS_RECEIVED
  -- ^ send() has been called, and headers and status are available.
  | LOADING
  -- ^ Downloading; responseText holds partial data.
  | DONE
  -- ^ The operation is complete.
  deriving (Show, Eq, Enum)

data ResponseType
  = DOMStringType
  | ArrayBufferType
  | BlobType
  | DocumentType
  | JSONType
  | UnknownXHRType
  deriving (Show, Eq)

newtype Document = Document JSVal
newtype XHR = XHR JSVal

foreign import javascript unsafe "$r = new XMLHttpRequest();"
  newXHR :: IO XHR

foreign import javascript unsafe "$1.abort();"
  abort :: XHR -> IO ()

foreign import javascript unsafe "$r = $1.responseURL;"
  responseURL :: XHR -> IO JSString

foreign import javascript unsafe "$r = $1.readyState;"
  readyState' :: XHR -> IO Int

foreign import javascript unsafe "$r = $1.responseType;"
  responseType' :: XHR -> IO JSString

responseType :: XHR -> IO ResponseType
{-# INLINE responseType #-}
responseType xhr =
  responseType' xhr >>= \case
    "" -> pure DOMStringType
    "blob" -> pure BlobType
    "document" -> pure DocumentType
    "json" -> pure JSONType
    "arraybuffer" -> pure ArrayBufferType
    "text" -> pure DOMStringType
    _ -> pure UnknownXHRType

readyState :: XHR -> IO ReadyState
{-# INLINE readyState #-}
readyState xhr = toEnum <$> readyState' xhr

-- request.open("GET", "foo.txt", true);
foreign import javascript unsafe "$1.open($2, $3, $4);"
  open :: XHR -> JSString -> JSString -> Bool -> IO ()

foreign import javascript unsafe "$1.send();"
  send :: XHR -> IO ()

foreign import javascript unsafe "$1.setRequestHeader($2,$3);"
  setRequestHeader :: XHR -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1.onreadystatechanged = $2;"
  onReadyStateChanged :: XHR -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.getAllResponseHeaders();"
  getAllResponseHeaders' :: XHR -> IO (Nullable JSString)

foreign import javascript unsafe "$r = $1.getResponseHeader($2);"
  getResponseHeader' :: XHR -> JSString -> IO (Nullable JSString)

getResponseHeader :: XHR -> JSString -> IO (Maybe JSString)
{-# INLINE getResponseHeader #-}
getResponseHeader xhr key =
  nullableToMaybe <$> getResponseHeader' xhr key

foreign import javascript unsafe "$r = $1.response;"
  response' :: XHR -> IO (Nullable JSVal)

foreign import javascript unsafe "$r = $1.status;"
  status' :: XHR -> IO (Nullable Int)

foreign import javascript unsafe "$r = $1.statusText;"
  statusText' :: XHR -> IO (Nullable JSString)

foreign import javascript unsafe "$1.overrideMimeType($2);"
  overrideMimeType :: XHR -> JSString -> IO ()

foreign import javascript unsafe "$r = $1.timeout;"
  timeout :: XHR -> IO Int

foreign import javascript unsafe "$1.withCredentials = true;"
  withCredentials :: XHR -> IO ()

foreign import javascript unsafe "$1.response ? true : false"
  hasResponse :: XHR -> IO Bool

getAllResponseHeaders :: XHR -> IO (Maybe [JSString])
{-# INLINE getAllResponseHeaders #-}
getAllResponseHeaders = \xhr -> do
  result <- getAllResponseHeaders' xhr
  pure $ lines <$> nullableToMaybe result
