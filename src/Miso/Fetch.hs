{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
    -- ** Types
  , Body
  ) where
----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Control.Monad
import           Data.Aeson (FromJSON, ToJSON, Result(..), fromJSON)
import           Language.Javascript.JSaddle (fromJSValUnchecked, toJSVal)
----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Effect (Effect, withSink)
import           Miso.String (MisoString, ms)
import           Miso.Util ((=:))
import           Miso.FFI
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
-- updateModel FetchGitHub =
--   getJSON "https://api.github.com" [] SetGitHub ErrorHandler
-- updateModel (SetGitHub apiInfo) =
--   info ?= apiInfo
-- updateModel (ErrorHandler msg) =
--  io_ (consoleError msg)
--
-- @
--
getJSON
  :: FromJSON result
  => MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (result -> action)
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
getJSON url headers successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing jsonHeaders
      (\e -> 
         fromJSON <$> fromJSValUnchecked e >>= \case
          Error decodeFailure ->
            sink $ errorful (ms decodeFailure)
          Success result -> do
            sink $ successful result)
      (sink . errorful)
      "json" -- dmj: expected return type
  where
    jsonHeaders = biasHeaders headers [accept =: applicationJSON]
----------------------------------------------------------------------------
postJSON
  :: ToJSON body
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
postJSON url body headers successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body
    FFI.fetch url "POST" (Just bodyVal) jsonHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    jsonHeaders =
      [contentType =: applicationJSON] <> headers
----------------------------------------------------------------------------
putJSON
  :: ToJSON body
  => MisoString
  -- ^ url
  -> body
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
putJSON url body headers successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body
    FFI.fetch url "PUT" (Just bodyVal) jsonHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    jsonHeaders = biasHeaders headers [contentType =: applicationJSON]
----------------------------------------------------------------------------
getText
  :: MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (MisoString -> action)
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
getText url headers successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing textHeaders
      (sink . successful <=< fromJSValUnchecked)
      (sink . errorful)
      "text" -- dmj: expected return type
  where
    textHeaders = biasHeaders headers [accept =: applicationText]
----------------------------------------------------------------------------
postText
  :: MisoString
  -- ^ url
  -> MisoString
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
postText url body headers successful errorful =
  withSink $ \sink -> do
    bodyVal <- FFI.jsonStringify body
    FFI.fetch url "POST" (Just bodyVal) textHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    textHeaders = biasHeaders headers [contentType =: applicationText]
----------------------------------------------------------------------------
putText
  :: MisoString
  -- ^ url
  -> MisoString
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
putText url body headers successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal body
    FFI.fetch url "PUT" (Just body_) textHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    textHeaders = biasHeaders headers [contentType =: applicationText]
----------------------------------------------------------------------------
getBlob
  :: MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (Blob -> action)
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
getBlob url headers successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing blobHeaders
      (sink . successful <=< fromJSValUnchecked)
      (sink . errorful)
      "blob" -- dmj: expected return type
  where
    blobHeaders = biasHeaders headers [accept =: octetStream]
----------------------------------------------------------------------------
postBlob
  :: MisoString
  -- ^ url
  -> Blob
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
postBlob url body headers successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body
    FFI.fetch url "POST" (Just bodyVal) blobHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    blobHeaders = biasHeaders headers [contentType =: octetStream]
----------------------------------------------------------------------------
putBlob
  :: MisoString
  -- ^ url
  -> Blob
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
putBlob url body headers successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal body
    FFI.fetch url "PUT" (Just body_) blobHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    blobHeaders = biasHeaders headers [contentType =: octetStream]
----------------------------------------------------------------------------
getArrayBuffer
  :: MisoString
  -- ^ url
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (ArrayBuffer -> action)
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
getArrayBuffer url headers successful errorful =
  withSink $ \sink ->
    FFI.fetch url "GET" Nothing arrayBufferHeaders
      (sink . successful <=< fromJSValUnchecked)
      (sink . errorful)
      "arrayBuffer" -- dmj: expected return type
  where
    arrayBufferHeaders = biasHeaders headers [accept =: octetStream]
----------------------------------------------------------------------------
postArrayBuffer
  :: MisoString
  -- ^ url
  -> ArrayBuffer
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
postArrayBuffer url body headers successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body
    FFI.fetch url "POST" (Just bodyVal) arrayBufferHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    arrayBufferHeaders = biasHeaders headers [contentType =: octetStream]
----------------------------------------------------------------------------
putArrayBuffer
  :: MisoString
  -- ^ url
  -> ArrayBuffer
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
putArrayBuffer url body headers successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal body
    FFI.fetch url "PUT" (Just body_) arrayBufferHeaders
      (const (sink successful))
      (sink . errorful)
      "none"
  where
    arrayBufferHeaders = biasHeaders headers [contentType =: octetStream]
----------------------------------------------------------------------------
postImage
  :: MisoString
  -- ^ url
  -> Image
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
postImage url body headers successful errorful =
  withSink $ \sink -> do
    bodyVal <- toJSVal body
    FFI.fetch url "POST" (Just bodyVal) headers
      (const (sink successful))
      (sink . errorful)
      "none"
----------------------------------------------------------------------------
putImage
  :: MisoString
  -- ^ url
  -> Image
  -- ^ Body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> action
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect parent model action
putImage url body headers successful errorful =
  withSink $ \sink -> do
    body_ <- toJSVal body
    FFI.fetch url "PUT" (Just body_) headers
      (const (sink successful))
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
applicationText :: MisoString
applicationText = "text/plain"
----------------------------------------------------------------------------
octetStream :: MisoString
octetStream = "application/octect-stream"
----------------------------------------------------------------------------
biasHeaders :: Ord k => [(k, a)] -> [(k, a)] -> [(k, a)]
biasHeaders userDefined contentSpecific
  = M.toList
  $ M.fromList userDefined <> M.fromList contentSpecific
----------------------------------------------------------------------------
