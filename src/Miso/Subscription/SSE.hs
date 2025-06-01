-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.SSE
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.SSE
 ( -- *** Subscription
   sseSub
   -- *** Types
 , SSE (..)
 ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import qualified Language.Javascript.JSaddle as JSaddle
import           Language.Javascript.JSaddle hiding (new)
-----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import qualified Miso.FFI.Internal as FFI
import           Miso.String
-----------------------------------------------------------------------------
-- | Server-sent events Subscription
sseSub
  :: FromJSON msg
  => MisoString -- ^ EventSource URL
  -> (SSE msg -> action)
  -> Sub action
sseSub url f sink = do
  es <- JSaddle.new (jsg (ms "EventSource")) [url]
  FFI.addEventListener es (ms "message") $ \v -> do
    dat <- FFI.jsonParse =<< v ! (ms "data")
    sink (f (SSEMessage dat))
  FFI.addEventListener es (ms "error") $ \_ ->
    sink (f SSEError)
  FFI.addEventListener es (ms "close") $ \_ ->
    sink (f SSEClose)
-----------------------------------------------------------------------------
-- | Server-sent events data
data SSE message
  = SSEMessage message
  | SSEClose
  | SSEError
  deriving (Show, Eq)
-----------------------------------------------------------------------------
