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
  , postJSON'
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
  , Response (..)
  , CONTENT_TYPE (..)
    -- ** Internal
  , fetch
  ) where
----------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Map.Strict as M
import           Language.Javascript.JSaddle (toJSVal, FromJSVal(..), JSVal)
----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Effect (Effect, withSink)
import           Miso.String (MisoString, ms)
import           Miso.Util ((=:))
import           Miso.FFI.Internal (Response(..), Blob, FormData, ArrayBuffer, Uint8Array, Image, fetch, CONTENT_TYPE(..))
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
  :: (FromJSON body, FromJSVal error)
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (Response body -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getJSON url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing jsonHeaders
      (handleJSON sink)
      (sink . errorful)
      JSON -- dmj: expected return type
  where
    jsonHeaders = biasHeaders headers_ [accept =: applicationJSON]
    handleJSON sink resp@Response {..} =
      fmap fromJSON <$> fromJSVal body >>= \case
        Nothing -> do
          err <- fromJSValUnchecked body
          sink $ errorful $ Response
            { body = err
            , errorMessage = Just "Not a valid JSON object"
            , ..
            }
        Just (Success result) -> 
          sink $ successful resp { body = result }
        Just (Error msg) -> do 
          err <- fromJSValUnchecked body
          sink $ errorful $ Response
            { body = err
            , errorMessage = Just (ms msg)
            , ..
            }
----------------------------------------------------------------------------
-- | POST request that uses JSON encoded data
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
      NONE
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
----------------------------------------------------------------------------
-- | POST request that uses JSON encoded data, and returns JSON encoded data
postJSON'
  :: (FromJSVal error, ToJSON body, FromJSON return)
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response return -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
postJSON' url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body_
    FFI.fetch url "POST" (Just bodyVal) jsonHeaders_
      (sink . successful)
      (sink . errorful)
      JSON
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON, accept =: applicationJSON]
----------------------------------------------------------------------------
-- | PUT request that uses JSON encoded data
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
      NONE
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
----------------------------------------------------------------------------
-- | GET request that uses Text encoded data
getText
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> (Response MisoString -> action)
  -- ^ successful callback
  -> (Response error -> action)
  -- ^ errorful callback
  -> Effect parent model action
getText url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing textHeaders_
      (sink . successful)
      (sink . errorful)
      TEXT -- dmj: expected return type
  where
    textHeaders_ = biasHeaders headers_ [accept =: textPlain]
----------------------------------------------------------------------------
-- | POST request that uses Text encoded data
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
      NONE
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
----------------------------------------------------------------------------
-- | PUT request that uses Text encoded data
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
      NONE
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
----------------------------------------------------------------------------
-- | GET request that uses binary encoded data
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
      BLOB -- dmj: expected return type
  where
    blobHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
-- | POST request that uses binary encoded data
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
      NONE
  where
    blobHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
-- | PUT request that uses binary encoded data
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
      NONE
  where
    blobHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
-- | GET request that uses FormData
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
      FORM_DATA -- dmj: expected return type
  where
    formDataHeaders_ = biasHeaders headers_ [accept =: formData]
----------------------------------------------------------------------------
-- | POST request that uses FormData
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
      NONE
  where
    formDataHeaders_ = biasHeaders headers_ [contentType =: formData]
----------------------------------------------------------------------------
-- | PUT request that uses FormData
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
      NONE
  where
    formDataHeaders_ = biasHeaders headers_ [contentType =: formData]
----------------------------------------------------------------------------
-- | GET request that uses ArrayBuffer
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
      ARRAY_BUFFER -- dmj: expected return type
  where
    arrayBufferHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
-- | POST request that uses ArrayBuffer
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
      NONE
  where
    arrayBufferHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
-- | PUT request that uses ArrayBuffer
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
      NONE
  where
    arrayBufferHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
-- | GET request that retrieves a byte array
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
      BYTES -- expected return type
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
-- | POST request that sends a byte array
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
      NONE
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
-- | PUT request that sends a byte array
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
      NONE
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [contentType =: octetStream]
----------------------------------------------------------------------------
-- | POST request that sends an 'Image'
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
      NONE
----------------------------------------------------------------------------
-- | PUT request that sends an 'Image'
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
      NONE
----------------------------------------------------------------------------
-- | Type synonym for Body
type Body = JSVal
----------------------------------------------------------------------------
-- | Value for specifying "Accept" in an HTTP header
accept :: MisoString
accept = "Accept"
----------------------------------------------------------------------------
-- | Value for specifying "Content-Type" in an HTTP header
contentType :: MisoString
contentType = "Content-Type"
----------------------------------------------------------------------------
-- | Value for specifying "application/json" in an HTTP header
applicationJSON :: MisoString
applicationJSON = "application/json"
----------------------------------------------------------------------------
-- | Value for specifying "text/plain" in an HTTP header
textPlain :: MisoString
textPlain = "text/plain"
----------------------------------------------------------------------------
-- | Value for specifying "application/octet-stream" in an HTTP header
octetStream :: MisoString
octetStream = "application/octect-stream"
----------------------------------------------------------------------------
-- | Value for specifying "multipart/form-data" in an HTTP header
formData :: MisoString
formData = "multipart/form-data"
----------------------------------------------------------------------------
-- | Helper function for the union of two header Map 
biasHeaders :: Ord k => [(k, a)] -> [(k, a)] -> [(k, a)]
biasHeaders userDefined contentSpecific
  = M.toList
  $ M.fromList userDefined <> M.fromList contentSpecific
----------------------------------------------------------------------------
