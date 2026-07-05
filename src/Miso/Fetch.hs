-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Fetch
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interface to the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API Fetch API>
-- for making HTTP requests inside Miso's 'Effect' monad.
--
-- Each function accepts a URL, optional request headers, a success callback,
-- and an error callback of the form @'Response' x -> action@. The resulting
-- 'Effect' dispatches the appropriate action into the MVU loop when the
-- response arrives.
--
-- Functions are grouped by HTTP method and body\/response type:
--
-- * __JSON__        — 'getJSON', 'postJSON', 'postJSON'', 'putJSON'
-- * __Text__        — 'getText', 'postText', 'putText'
-- * __Blob__        — 'getBlob', 'postBlob', 'putBlob'
-- * __FormData__    — 'getFormData', 'postFormData', 'putFormData'
-- * __Uint8Array__  — 'getUint8Array', 'postUint8Array', 'putUint8Array'
-- * __ArrayBuffer__ — 'getArrayBuffer', 'postArrayBuffer', 'putArrayBuffer'
-- * __Image__       — 'postImage', 'putImage'
--
-- Use 'getJSON' or 'postJSON' for typical REST calls; use 'postJSON'' when
-- the server also returns a JSON response body.
--
-- Every function above has a synchronous counterpart suffixed with @_@
-- (e.g. 'getJSON_', 'postJSON_') that blocks the calling thread until the
-- browser resolves the underlying promise, returning @'Right' response@ on
-- success or @'Left' response@ on failure instead of dispatching an action.
-- These are best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid
-- blocking the scheduler thread.
--
-- For Servant-style typed client generation, see the miso README.
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
    -- ** Synchronous API variants
  , getJSON_
  , postJSON_
  , postJSON'_
  , putJSON_
  , getText_
  , postText_
  , putText_
  , getBlob_
  , postBlob_
  , putBlob_
  , getFormData_
  , postFormData_
  , putFormData_
  , getUint8Array_
  , postUint8Array_
  , putUint8Array_
  , postImage_
  , putImage_
  , getArrayBuffer_
  , postArrayBuffer_
  , putArrayBuffer_
    -- ** Internal
  , fetch
  ) where
----------------------------------------------------------------------------
import           Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import           Miso.JSON
import qualified Data.Map.Strict as M
----------------------------------------------------------------------------
import           Miso.DSL (toJSVal, FromJSVal(..), JSVal)
import qualified Miso.FFI.Internal as FFI
import           Miso.Effect (Effect, withSink)
import           Miso.String (MisoString, ms)
import           Miso.Util ((=:))
import           Miso.FFI.Internal (Response(..), Blob, FormData, ArrayBuffer, Uint8Array, Image, fetch, CONTENT_TYPE(..))
----------------------------------------------------------------------------
-- | Retrieve a JSON resource via GET.
--
-- @
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
  -> Effect parent props model action
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
-- | Send a POST request with a JSON-encoded body; ignores the response body.
--
-- Sets @Content-Type: application\/json@ automatically. Use 'postJSON'' when
-- you also need to parse a JSON response body.
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
  -> Effect parent props model action
postJSON url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal (encode body_)
    FFI.fetch url "POST" (Just bodyVal) jsonHeaders_
      (sink . successful)
      (sink . errorful)
      NONE
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
----------------------------------------------------------------------------
-- | Send a POST request with a JSON-encoded body and parse a JSON response.
--
-- Sets both @Content-Type: application\/json@ and @Accept: application\/json@
-- automatically. Use 'postJSON' when the response body is not needed.
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
  -> Effect parent props model action
postJSON' url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal (encode body_)
    FFI.fetch url "POST" (Just bodyVal) jsonHeaders_
      (handleJSON sink)
      (sink . errorful)
      JSON
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON, accept =: applicationJSON]
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
-- | Send a PUT request with a JSON-encoded body; ignores the response body.
--
-- Sets @Content-Type: application\/json@ automatically.
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
  -> Effect parent props model action
putJSON url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal (encode body_)
    FFI.fetch url "PUT" (Just bodyVal) jsonHeaders_
      (sink . successful)
      (sink . errorful)
      NONE
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
----------------------------------------------------------------------------
-- | Retrieve a plain-text resource via GET.
--
-- Sets @Accept: text\/plain@ automatically. The response body is delivered as
-- a 'MisoString'.
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
  -> Effect parent props model action
getText url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing textHeaders_
      (sink . successful)
      (sink . errorful)
      TEXT -- dmj: expected return type
  where
    textHeaders_ = biasHeaders headers_ [accept =: textPlain]
----------------------------------------------------------------------------
-- | Send a POST request with a plain-text body; ignores the response body.
--
-- Sets @Content-Type: text\/plain@ automatically.
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
  -> Effect parent props model action
postText url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal (encode body_)
    FFI.fetch url "POST" (Just bodyVal) textHeaders_
      (sink . successful)
      (sink . errorful)
      NONE
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
----------------------------------------------------------------------------
-- | Send a PUT request with a plain-text body; ignores the response body.
--
-- Sets @Content-Type: text\/plain@ automatically.
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
  -> Effect parent props model action
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
-- | Retrieve a binary resource as a 'Blob' via GET.
--
-- Sets @Accept: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
getBlob url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing blobHeaders_
      (sink . successful)
      (sink . errorful)
      BLOB -- dmj: expected return type
  where
    blobHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
-- | Send a POST request with a 'Blob' body; ignores the response body.
--
-- Sets @Content-Type: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
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
-- | Send a PUT request with a 'Blob' body; ignores the response body.
--
-- Sets @Content-Type: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
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
-- | Retrieve a multipart resource as 'FormData' via GET.
--
-- Sets @Accept: multipart\/form-data@ automatically.
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
  -> Effect parent props model action
getFormData url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing formDataHeaders_
      (sink . successful)
      (sink . errorful)
      FORM_DATA -- dmj: expected return type
  where
    formDataHeaders_ = biasHeaders headers_ [accept =: formData]
----------------------------------------------------------------------------
-- | Send a POST request with a 'FormData' body; ignores the response body.
--
-- Sets @Content-Type: multipart\/form-data@ automatically.
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
  -> Effect parent props model action 
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
-- | Send a PUT request with a 'FormData' body; ignores the response body.
--
-- Sets @Content-Type: multipart\/form-data@ automatically.
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
  -> Effect parent props model action
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
-- | Retrieve a binary resource as an 'ArrayBuffer' via GET.
--
-- Sets @Accept: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
getArrayBuffer url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing arrayBufferHeaders_
      (sink . successful)
      (sink . errorful)
      ARRAY_BUFFER -- dmj: expected return type
  where
    arrayBufferHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
-- | Send a POST request with an 'ArrayBuffer' body; ignores the response body.
--
-- Sets @Content-Type: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
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
-- | Send a PUT request with an 'ArrayBuffer' body; ignores the response body.
--
-- Sets @Content-Type: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
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
-- | Retrieve a binary resource as a 'Uint8Array' via GET.
--
-- Sets @Accept: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
getUint8Array url headers_ successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing uint8ArrayHeaders_
      (sink . successful)
      (sink . errorful)
      BYTES -- expected return type
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [accept =: octetStream]
----------------------------------------------------------------------------
-- | Send a POST request with a 'Uint8Array' body; ignores the response body.
--
-- Sets @Content-Type: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
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
-- | Send a PUT request with a 'Uint8Array' body; ignores the response body.
--
-- Sets @Content-Type: application\/octet-stream@ automatically.
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
  -> Effect parent props model action
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
-- | Send a POST request with an 'Image' body; ignores the response body.
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
  -> Effect parent props model action
postImage url body_ headers_ successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body_
    FFI.fetch url "POST" (Just bodyVal) headers_
      (sink . successful)
      (sink . errorful)
      NONE
----------------------------------------------------------------------------
-- | Send a PUT request with an 'Image' body; ignores the response body.
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
  -> Effect parent props model action
putImage url imageBody headers_ successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal imageBody
    FFI.fetch url "PUT" (Just body_) headers_
      (sink . successful)
      (sink . errorful)
      NONE
----------------------------------------------------------------------------
-- | Type synonym for a raw JavaScript request body.
type Body = JSVal
----------------------------------------------------------------------------
-- | HTTP header name @\"Accept\"@.
-- Use with '=:' to build request headers, e.g. @accept =: applicationJSON@.
accept :: MisoString
accept = "Accept"
----------------------------------------------------------------------------
-- | HTTP header name @\"Content-Type\"@.
-- Use with '=:' to build request headers, e.g. @contentType =: textPlain@.
contentType :: MisoString
contentType = "Content-Type"
----------------------------------------------------------------------------
-- | MIME type @\"application\/json\"@.
applicationJSON :: MisoString
applicationJSON = "application/json"
----------------------------------------------------------------------------
-- | MIME type @\"text\/plain\"@.
textPlain :: MisoString
textPlain = "text/plain"
----------------------------------------------------------------------------
-- | MIME type @\"application\/octet-stream\"@. Used for binary payloads.
octetStream :: MisoString
octetStream = "application/octet-stream"
----------------------------------------------------------------------------
-- | MIME type @\"multipart\/form-data\"@.
formData :: MisoString
formData = "multipart/form-data"
----------------------------------------------------------------------------
-- | Merge two header lists, giving precedence to the first (user-supplied) list.
--
-- Duplicate keys in @contentSpecific@ are dropped when the same key already
-- appears in @userDefined@, allowing callers to override the library's default
-- @Content-Type@ and @Accept@ headers.
biasHeaders :: Ord k => [(k, a)] -> [(k, a)] -> [(k, a)]
biasHeaders userDefined contentSpecific
  = M.toList
  $ M.fromList userDefined <> M.fromList contentSpecific
----------------------------------------------------------------------------
-- | Synchronous API variant of 'getJSON'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
getJSON_
  :: (FromJSON body, FromJSVal error)
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> IO (Either (Response error) (Response body))
getJSON_ url headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response body)))
  FFI.fetch url "GET" Nothing jsonHeaders
    (handleJSON mvar)
    (putMVar mvar . Left)
    JSON
  takeMVar mvar
  where
    jsonHeaders = biasHeaders headers_ [accept =: applicationJSON]
    handleJSON mvar resp@Response {..} =
      fmap fromJSON <$> fromJSVal body >>= \case
        Nothing -> do
          err <- fromJSValUnchecked body
          putMVar mvar $ Left $ Response
            { body = err
            , errorMessage = Just "Not a valid JSON object"
            , ..
            }
        Just (Success result) ->
          putMVar mvar $ Right resp { body = result }
        Just (Error msg) -> do
          err <- fromJSValUnchecked body
          putMVar mvar $ Left $ Response
            { body = err
            , errorMessage = Just (ms msg)
            , ..
            }
{-# INLINE getJSON_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postJSON'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postJSON_
  :: (FromJSVal error, ToJSON body)
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postJSON_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal (encode body_)
  FFI.fetch url "POST" (Just bodyVal) jsonHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
{-# INLINE postJSON_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postJSON''.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postJSON'_
  :: (FromJSVal error, ToJSON body, FromJSON return)
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response return))
postJSON'_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response return)))
  bodyVal <- toJSVal (encode body_)
  FFI.fetch url "POST" (Just bodyVal) jsonHeaders_
    (handleJSON mvar)
    (putMVar mvar . Left)
    JSON
  takeMVar mvar
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON, accept =: applicationJSON]
    handleJSON mvar resp@Response {..} =
      fmap fromJSON <$> fromJSVal body >>= \case
        Nothing -> do
          err <- fromJSValUnchecked body
          putMVar mvar $ Left $ Response
            { body = err
            , errorMessage = Just "Not a valid JSON object"
            , ..
            }
        Just (Success result) ->
          putMVar mvar $ Right resp { body = result }
        Just (Error msg) -> do
          err <- fromJSValUnchecked body
          putMVar mvar $ Left $ Response
            { body = err
            , errorMessage = Just (ms msg)
            , ..
            }
{-# INLINE postJSON'_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putJSON'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putJSON_
  :: (FromJSVal error, ToJSON body)
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putJSON_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal (encode body_)
  FFI.fetch url "PUT" (Just bodyVal) jsonHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    jsonHeaders_ = biasHeaders headers_ [contentType =: applicationJSON]
{-# INLINE putJSON_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'getText'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
getText_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response MisoString))
getText_ url headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response MisoString)))
  FFI.fetch url "GET" Nothing textHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    TEXT
  takeMVar mvar
  where
    textHeaders_ = biasHeaders headers_ [accept =: textPlain]
{-# INLINE getText_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postText'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postText_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postText_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal (encode body_)
  FFI.fetch url "POST" (Just bodyVal) textHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
{-# INLINE postText_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putText'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putText_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putText_ url imageBody headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  body_ <- toJSVal imageBody
  FFI.fetch url "PUT" (Just body_) textHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    textHeaders_ = biasHeaders headers_ [contentType =: textPlain]
{-# INLINE putText_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'getBlob'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
getBlob_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response Blob))
getBlob_ url headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response Blob)))
  FFI.fetch url "GET" Nothing blobHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    BLOB
  takeMVar mvar
  where
    blobHeaders_ = biasHeaders headers_ [accept =: octetStream]
{-# INLINE getBlob_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postBlob'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postBlob_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Blob
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postBlob_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal body_
  FFI.fetch url "POST" (Just bodyVal) blobHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    blobHeaders_ = biasHeaders headers_ [contentType =: octetStream]
{-# INLINE postBlob_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putBlob'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putBlob_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Blob
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putBlob_ url imageBody headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  body_ <- toJSVal imageBody
  FFI.fetch url "PUT" (Just body_) blobHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    blobHeaders_ = biasHeaders headers_ [contentType =: octetStream]
{-# INLINE putBlob_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'getFormData'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
getFormData_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response FormData))
getFormData_ url headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response FormData)))
  FFI.fetch url "GET" Nothing formDataHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    FORM_DATA
  takeMVar mvar
  where
    formDataHeaders_ = biasHeaders headers_ [accept =: formData]
{-# INLINE getFormData_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postFormData'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postFormData_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> FormData
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postFormData_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal body_
  FFI.fetch url "POST" (Just bodyVal) formDataHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    formDataHeaders_ = biasHeaders headers_ [contentType =: formData]
{-# INLINE postFormData_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putFormData'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putFormData_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> FormData
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putFormData_ url imageBody headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  body_ <- toJSVal imageBody
  FFI.fetch url "PUT" (Just body_) formDataHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    formDataHeaders_ = biasHeaders headers_ [contentType =: formData]
{-# INLINE putFormData_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'getArrayBuffer'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
getArrayBuffer_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ArrayBuffer))
getArrayBuffer_ url headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ArrayBuffer)))
  FFI.fetch url "GET" Nothing arrayBufferHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    ARRAY_BUFFER
  takeMVar mvar
  where
    arrayBufferHeaders_ = biasHeaders headers_ [accept =: octetStream]
{-# INLINE getArrayBuffer_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postArrayBuffer'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postArrayBuffer_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> ArrayBuffer
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postArrayBuffer_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal body_
  FFI.fetch url "POST" (Just bodyVal) arrayBufferHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    arrayBufferHeaders_ = biasHeaders headers_ [contentType =: octetStream]
{-# INLINE postArrayBuffer_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putArrayBuffer'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putArrayBuffer_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> ArrayBuffer
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putArrayBuffer_ url arrayBuffer_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  body_ <- toJSVal arrayBuffer_
  FFI.fetch url "PUT" (Just body_) arrayBufferHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    arrayBufferHeaders_ = biasHeaders headers_ [contentType =: octetStream]
{-# INLINE putArrayBuffer_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'getUint8Array'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
getUint8Array_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response Uint8Array))
getUint8Array_ url headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response Uint8Array)))
  FFI.fetch url "GET" Nothing uint8ArrayHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    BYTES
  takeMVar mvar
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [accept =: octetStream]
{-# INLINE getUint8Array_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postUint8Array'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postUint8Array_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Uint8Array
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postUint8Array_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal body_
  FFI.fetch url "POST" (Just bodyVal) uint8ArrayHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [contentType =: octetStream]
{-# INLINE postUint8Array_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putUint8Array'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putUint8Array_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Uint8Array
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putUint8Array_ url uint8Array_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  body_ <- toJSVal uint8Array_
  FFI.fetch url "PUT" (Just body_) uint8ArrayHeaders_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
  where
    uint8ArrayHeaders_ = biasHeaders headers_ [contentType =: octetStream]
{-# INLINE putUint8Array_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'postImage'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
postImage_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Image
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
postImage_ url body_ headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  bodyVal <- toJSVal body_
  FFI.fetch url "POST" (Just bodyVal) headers_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
{-# INLINE postImage_ #-}
----------------------------------------------------------------------------
-- | Synchronous API variant of 'putImage'.
--
-- Blocks the calling thread until the browser resolves the promise, returning
-- @'Right' response@ on success or @'Left' response@ on failure.
--
-- __Note:__ best used with 'Miso.Effect.io' or 'Miso.Effect.io_', to avoid blocking the scheduler thread.
putImage_
  :: FromJSVal error
  => MisoString
  -- ^ url
  -> Image
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers_
  -> IO (Either (Response error) (Response ()))
putImage_ url imageBody headers_ = do
  mvar <- newEmptyMVar :: IO (MVar (Either (Response error) (Response ())))
  body_ <- toJSVal imageBody
  FFI.fetch url "PUT" (Just body_) headers_
    (putMVar mvar . Right)
    (putMVar mvar . Left)
    NONE
  takeMVar mvar
{-# INLINE putImage_ #-}
----------------------------------------------------------------------------
