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
  ( -- ** Function
    fetch
    -- ** Header helpers
  , accept
  , contentType
  , applicationJSON
    -- ** Types
  , Body
  ) where
----------------------------------------------------------------------------
import           Data.Aeson (FromJSON)
import           Language.Javascript.JSaddle (JSVal)
----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Effect (Effect, withSink)
import           Miso.String (MisoString)
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
--   let headers = [ accept =: applicationJSON ]
--   fetch "https://api.github.com" "GET" Nothing headers SetGitHub ErrorHandler
-- updateModel (SetGitHub apiInfo) =
--   info ?= apiInfo
-- updateModel (ErrorHandler msg) =
--  io_ (consoleError msg)
--
-- @
--
fetch
  :: FromJSON result
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ method
  -> Maybe Body
  -- ^ body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (result -> action)
  -- ^ successful callback
  -> (MisoString -> action)
  -- ^ errorful callback
  -> Effect model action
fetch url method body headers successful errorful =
  withSink $ \sink ->
    FFI.fetch url method body headers
      (sink . successful)
      (sink . errorful)
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
