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
 ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Aeson as A
import qualified Language.Javascript.JSaddle as JSaddle
import           Language.Javascript.JSaddle hiding (new)
-----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import qualified Miso.FFI.Internal as FFI
import           Miso.String
import           Miso.Subscription.Util
-----------------------------------------------------------------------------
-- | Server-sent events Subscription
sseSub
  :: FromJSON msg
  => MisoString
  -- ^ EventSource URL
  -> (msg -> action)
  -- ^ Succesful callback
  -> (MisoString -> action)
  -- ^ JSON decode failure callback
  -> (JSVal -> action)
  -- ^ SSE error callback
  -> (JSVal -> action)
  -- ^ SSE close callback
  -> Sub action
sseSub url successful errorful errorCb closeCb sink = createSub acquire release sink
  where
    release (es, cb1, cb2, cb3) = do
      FFI.removeEventListener es (ms "message") cb1
      FFI.removeEventListener es (ms "error") cb2
      FFI.removeEventListener es (ms "close") cb3
    acquire = do
      es <- JSaddle.new (jsg (ms "EventSource")) [url]
      cb1 <- FFI.addEventListener es (ms "message") $ \v -> do
        maybeValue <- fromJSVal =<< v ! (ms "data")
        case maybeValue of
          Nothing ->
            sink $ errorful (ms "sseSub: Could not parse 'data' as JSON")
          Just value ->
            case fromJSON value of
              A.Success o ->
                sink (successful o)
              A.Error e ->
                sink (errorful (ms e))
      cb2 <- FFI.addEventListener es (ms "error") (sink . errorCb)
      cb3 <- FFI.addEventListener es (ms "close") (sink . closeCb)
      pure (es, cb1, cb2, cb3)
-----------------------------------------------------------------------------
