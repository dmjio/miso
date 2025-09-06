-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Fetch
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Module for interacting with the Fetch API <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API>
-- manually.
--
-- Refer to the miso README if you want to automatically interact with a Servant
-- API.
--
----------------------------------------------------------------------------
module Miso.Fetch
  ( -- ** JSON
    getJSON
  , postJSON
  , putJSON
  -- ** Text
  , getText
  , postText
  , putText
  -- ** Blob
  , getBlob
  , postBlob
  , putBlob
  -- ** FormData
  , getFormData
  , postFormData
  , putFormData
  -- ** Uint8Array
  , getUint8Array
  , postUint8Array
  , putUint8Array
  -- ** Image
  , postImage
  , putImage
  -- ** ArrayBuffer
  , getArrayBuffer
  , postArrayBuffer
  , putArrayBuffer
    -- ** Header helpers
  , accept
  , contentType
  , applicationJSON
  , textPlain
  , formData
    -- ** Types
  , Body
    -- ** Internal
  , fetch
  ) where
----------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Map.Strict as M
import           Language.Javascript.JSaddle (toJSVal, FromJSVal(..), JSVal)
----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Effect (Effect, withSink, Sink)
import           Miso.String (MisoString, ms)
import           Miso.Util ((=:))
import           Miso.FFI.Internal (Response(..), Blob, FormData, ArrayBuffer, Uint8Array, Image, fetch)
----------------------------------------------------------------------------
-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API>
--
-- @
--
-- data Action
--  = FetchGitHub
--  | SetGitHub GitHub
--  | ErrorHandler MisoString
--  deriving (Show, Eq)
--
-- updateModel :: Action -> Effect Model Action
-- updateModel = \case
--   FetchGitHub -> getJSON "https://api.github.com" [] SetGitHub ErrorHandler
--   SetGitHub apiInfo -> info ?= apiInfo
--   ErrorHandler msg -> io_ (consoleError msg)
--
-- @
--
getJSON
  :: forall body error action parent model . (FromJSON body, FromJSVal error)
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (Response body -> action)
  -- ^ successful callback
  -> (Response (Maybe error) -> action)
  -- ^ errorful callback
  -> Effect parent model action
getJSON url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing jsonHeaders
      (\jval -> handleJSON sink jval)
      (sink . errorful)
      "json" -- dmj: expected return type
  where
    jsonHeaders = biasHeaders headers_ [accept =: applicationJSON]

    handleJSON :: Sink action -> Response JSVal -> FFI.JSM ()
    handleJSON sink resp@Response {..} =
      fromJSON <$> fromJSValUnchecked body >>= \case
        Success result -> sink $ successful resp { body = result }
        Error msg -> sink $ errorful $ Response
          { body = Nothing
          , errorMessage = Just (ms msg)
          , ..
          }
----------------------------------------------------------------------------
postJSON
  :: (FromJSVal error, ToJSON body)
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postJSON url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body_
    FFI.fetch url "POST" (Just bodyVal) jsonHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
----------------------------------------------------------------------------
putJSON
  :: (FromJSVal error, ToJSON body)
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
putJSON url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body_
    FFI.fetch url "PUT" (Just bodyVal) jsonHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
----------------------------------------------------------------------------
getText
  :: (FromJSVal success, FromJSVal error)
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response success -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getText url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing textHeaders_
      (sink . successful)
      (sink . errorful)
      "text" -- dmj: expected return type
  where
    textHeaders_ = biasHeaders headers_ [accept =: textPlain]
----------------------------------------------------------------------------
postText
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postText url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body_
    FFI.fetch url "POST" (Just bodyVal) textHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
----------------------------------------------------------------------------
putText
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
putText url imageBody headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal imageBody
    FFI.fetch url "PUT" (Just body_) textHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
----------------------------------------------------------------------------
getBlob
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response Blob -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getBlob url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing blobHeaders_
      (sink . successful)
      (sink . errorful)
      "blob" -- dmj: expected return type
  where
    blobHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
postBlob
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Blob
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postBlob url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body_
    FFI.fetch url "POST" (Just bodyVal) blobHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    blobHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
putBlob
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Blob
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
putBlob url imageBody headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal imageBody
    FFI.fetch url "PUT" (Just body_) blobHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    blobHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
getFormData
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response FormData -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getFormData url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing formDataHeaders_
      (sink . successful)
      (sink . errorful)
      "formData" -- dmj: expected return type
  where
    formDataHeaders_ = biasHeaders headers_ [accept =: formData]
----------------------------------------------------------------------------
postFormData
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> FormData
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postFormData url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body_
    FFI.fetch url "POST" (Just bodyVal) formDataHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    formDataHeaders_ = biasHeaders headers_ [contentType =: formData]
----------------------------------------------------------------------------
putFormData
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> FormData
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error  -> action)
  -- ^ errorful callback
  -> Effect parent model action
putFormData url imageBody headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal imageBody
    FFI.fetch url "PUT" (Just body_) formDataHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    formDataHeaders_ = biasHeaders headers_ [contentType =: formData]
----------------------------------------------------------------------------
getArrayBuffer
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response ArrayBuffer -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getArrayBuffer url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing arrayBufferHeaders_
      (sink . successful)
      (sink . errorful)
      "arrayBuffer" -- dmj: expected return type
  where
    arrayBufferHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
postArrayBuffer
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> ArrayBuffer
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postArrayBuffer url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body_
    FFI.fetch url "POST" (Just bodyVal) arrayBufferHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    arrayBufferHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
putArrayBuffer
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> ArrayBuffer
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
putArrayBuffer url arrayBuffer_ headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal arrayBuffer_
    FFI.fetch url "PUT" (Just body_) arrayBufferHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    arrayBufferHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
getUint8Array
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response Uint8Array -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getUint8Array url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing uint8ArrayHeaders_
      (sink . successful)
      (sink . errorful)
      "bytes" -- dmj: expected return type
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
postUint8Array
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Uint8Array
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postUint8Array url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body_
    FFI.fetch url "POST" (Just bodyVal) uint8ArrayHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
putUint8Array
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Uint8Array
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
putUint8Array url uint8Array_ headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal uint8Array_
    FFI.fetch url "PUT" (Just body_) uint8ArrayHeaders_
      (sink . successful)
      (sink . errorful)
      "none"
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
postImage
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Image
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postImage url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body_
    FFI.fetch url "POST" (Just bodyVal) headers_
      (sink . successful)
      (sink . errorful)
      "none"
----------------------------------------------------------------------------
putImage
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Image
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response () -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
putImage url imageBody headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal imageBody
    FFI.fetch url "PUT" (Just body_) headers_
      (sink . successful)
      (sink . errorful)
      "none"
----------------------------------------------------------------------------
type Body = JSVal
----------------------------------------------------------------------------
accept :: MisoString
accept = "Accept"
----------------------------------------------------------------------------
contentType :: MisoString
contentType = "Content-Type"
----------------------------------------------------------------------------
applicationJSON :: MisoString
applicationJSON = "application/json"
----------------------------------------------------------------------------
textPlain :: MisoString
textPlain = "text/plain"
----------------------------------------------------------------------------
octetStream :: MisoString
octetStream = "application/octect-stream"
----------------------------------------------------------------------------
formData :: MisoString
formData = "multipart/form-data"
----------------------------------------------------------------------------
biasHeaders :: Ord k => [(k, a)] -> [(k, a)] -> [(k, a)]
biasHeaders userDefined contentSpecific
  = M.toList
  $ M.fromList userDefined <> M.fromList contentSpecific
----------------------------------------------------------------------------
