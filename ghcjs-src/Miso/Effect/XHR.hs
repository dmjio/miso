{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE CPP                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect.XHR
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect.XHR ( misoXHR ) where

import           Data.Aeson
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
-- import           GHCJS.Types
import           JavaScript.Web.XMLHttpRequest
import           Servant.API.BasicAuth
import qualified JavaScript.Web.XMLHttpRequest as J
import           Servant.API
import qualified Servant.API                   as S
import           Unsafe.Coerce
import           System.IO.Unsafe
import qualified Network.HTTP.Types.Header     as N

import           Miso.String
import           Miso.FFI
import           Miso.Lens

-- | Default request
defReq :: XHRReq
defReq = XHRReq {
    _req = Request J.GET mempty mempty mempty False NoData
  , _qs = mempty
  }

data XHRReq
  = XHRReq
  { _req :: Request
  , _qs :: [(MisoString, Maybe MisoString)]
  }

-- | Requests Lens
req :: Lens' XHRReq Request
req = makeLens _req (\r x -> r { _req = x })

-- | QueryString Lens
qs :: Lens' XHRReq Request
qs = makeLens _req (\r x -> r { _req = x })

(^.) = get
(.~) = set

-- | Class for `XHR`
class HasXHR api where
  type XHR api :: *
  xhrWithRoute
    :: Proxy api
    -> XHRReq
    -> XHR api

misoXHR :: HasXHR api => Proxy api -> XHR api
misoXHR = flip xhrWithRoute defReq

class GetMethod (m :: k) where getMethod :: Proxy m -> J.Method
instance GetMethod 'S.GET where getMethod = const J.GET
-- GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS, PATCH
instance GetMethod 'S.POST where getMethod = const J.POST
instance GetMethod 'S.PUT where getMethod = const J.PUT
instance GetMethod 'S.DELETE where getMethod = const J.DELETE
-- instance GetMethod 'S.PATCH where getMethod = const J.PATCH
-- instance GetMethod 'S.TRACE where getMethod = const J.TRACE

-- | Verb
instance {-# OVERLAPPABLE #-}
  ( MimeUnrender ct a
  , GetMethod method
  , cts' ~ (ct ': cts)
  , FromJSON a
  ) => HasXHR (Verb method status cts' a) where
  type XHR (Verb method status cts' a) = IO a
  xhrWithRoute Proxy request = do
    Response {..} :: Response MisoString <-
      xhr (get req request) { reqMethod = getMethod (Proxy :: Proxy method) }
    parse (unsafeCoerce contents)

-- | Verb NoContent
instance {-# OVERLAPPING #-}
  GetMethod method => HasXHR (Verb method status cts NoContent) where
  type XHR (Verb method status cts NoContent) = IO NoContent
  xhrWithRoute Proxy request = do
    Response {..} :: Response MisoString <-
      xhr (req ^. request) { reqMethod = getMethod (Proxy :: Proxy method) }
    pure NoContent

-- | Verb, with HEADERS
instance {-# OVERLAPPING #-}
  ( MimeUnrender ct a, FromJSON a, BuildHeadersTo ls, GetMethod method, cts' ~ (ct ': cts)
  ) => HasXHR (Verb method status cts' (Headers ls a)) where
  type XHR (Verb method status cts' (Headers ls a)) = IO (Headers ls a)
  xhrWithRoute Proxy request = do
    Response {..} :: Response MisoString <-
      xhr (req ^. request) { reqMethod = getMethod (Proxy :: Proxy method) }
    resp <- parse (unsafeCoerce contents)
    rHs <- getAllResponseHeaders
    pure Headers { getResponse = resp
                 , getHeadersHList = buildHeadersTo (toHeaders rHs)
                 }

toHeaders :: JSString -> [N.Header]
toHeaders x = undefined

-- | `Headers`, with `NoContent`
instance {-# OVERLAPPING #-}
  ( BuildHeadersTo ls, GetMethod method
  ) => HasXHR (Verb method status cts (Headers ls NoContent)) where
  type XHR (Verb method status cts' (Headers ls NoContent)) = IO (Headers ls NoContent)
  xhrWithRoute Proxy request = do
    Response {..} :: Response MisoString <-
      xhr (get req request) { reqMethod = getMethod (Proxy :: Proxy method) }
    rHs <- getAllResponseHeaders
    pure Headers { getResponse = NoContent
                 , getHeadersHList = buildHeadersTo (toHeaders rHs)
                 }

-- | Capture
instance (ToHttpApiData a, HasXHR api, KnownSymbol sym) => HasXHR (Capture sym a :> api) where
  type XHR (Capture sym a :> api) = a -> XHR api
  xhrWithRoute Proxy req' x =
    xhrWithRoute (Proxy :: Proxy api) $ (req .~ newReq) req'
      where
        r = get req req'
        newReq = r {
          reqURI = reqURI r <> "/" <> ms (toUrlPiece x)
        }

#if MIN_VERSION_servant(0,8,1)
-- | CaptureAll
-- instance (KnownSymbol capture, ToHttpApiData a, HasXHR sublayout)
--    => HasXHR (CaptureAll capture a :> sublayout) where
--   type XHR (CaptureAll capture a :> sublayout) = [a] -> XHR sublayout
--   xhrWithRoute Proxy req vals =
--     xhrWithRoute (Proxy :: Proxy sublayout)
--       (foldl' (flip appendToPath) req ps)
--     where
--       ps = Prelude.map (unpack . toUrlPiece) vals

-- appendToPath :: MisoString -> Request -> Request
-- appendToPath p req =
--   req { reqURI = reqURI req <> "/" <> toEncodedUrlPiece p }
#endif

-- | Path (done)
instance (HasXHR api, KnownSymbol sym) => HasXHR (sym :> api) where
  type XHR (sym :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) newReq
    where
      newReq = req {
        reqURI = reqURI req <> "/" <> ms (symbolVal (Proxy :: Proxy sym))
      }

-- | Raw (not supported)
-- instance HasXHR Raw where
--   type XHR Raw = XHR (Response MisoString)
--   xhrWithRoute Proxy _ = error "Raw is not supported"

-- | Alternate
instance (HasXHR left, HasXHR right) => HasXHR (left :<|> right) where
  type XHR (left :<|> right) = XHR left :<|> XHR right
  xhrWithRoute Proxy s =
    xhrWithRoute (Proxy :: Proxy left) s :<|>
      xhrWithRoute (Proxy :: Proxy right) s

-- | Header
instance (ToHttpApiData a, HasXHR api, KnownSymbol sym) => HasXHR (Header sym a :> api) where
  type XHR (Header sym a :> api) = Maybe a -> XHR api
  xhrWithRoute Proxy req header = xhrWithRoute (Proxy :: Proxy api) newReq
    where
      newReq = req {
        reqHeaders = (k,v) : reqHeaders req
      }
      k = ms $ symbolVal (Proxy :: Proxy sym)
      v = fromMaybe mempty (ms . toHeader <$> header)


-- | HttpVersion
instance HasXHR api => HasXHR (HttpVersion :> api) where
  type XHR (HttpVersion :> api) = XHR api
  xhrWithRoute Proxy = xhrWithRoute (Proxy :: Proxy api)

-- | Query param
instance (KnownSymbol sym, ToHttpApiData a, HasXHR api) => HasXHR (QueryParam sym a :> api) where
  type XHR (QueryParam sym a :> api) = Maybe a -> XHR api
  xhrWithRoute Proxy req qp = xhrWithRoute (Proxy :: Proxy api) newReq
    where
      newReq = req { reqURI = undefined }
      k = ms $ symbolVal (Proxy :: Proxy sym)
      v = fromMaybe mempty (ms . toQueryParam <$> qp)

-- | Query param(s)
instance (KnownSymbol sym, ToHttpApiData a, HasXHR api) => HasXHR (QueryParams sym a :> api) where
  type XHR (QueryParams sym a :> api) = [a] -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Query flag
instance (KnownSymbol sym, HasXHR api) => HasXHR (QueryFlag sym :> api) where
  type XHR (QueryFlag sym :> api) = Bool -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Request Body
instance (ToJSON a, MimeRender ct a, HasXHR api) => HasXHR (ReqBody (ct ': cts) a :> api) where
  type XHR (ReqBody (ct ': cts) a :> api) = a -> XHR api
  xhrWithRoute Proxy req body = do
    xhrWithRoute (Proxy :: Proxy api) req {
      reqData = StringData $ unsafePerformIO (stringify body)
    }

-- | Remote host (done)
instance HasXHR api => HasXHR (RemoteHost :> api) where
  type XHR (RemoteHost :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) req

-- | IsSecure host (done)
instance HasXHR api => HasXHR (IsSecure :> api) where
  type XHR (IsSecure :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) req

-- | WithNamedContext (done)
instance HasXHR api => HasXHR (WithNamedContext :> api) where
  type XHR (WithNamedContext :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) req

-- | Vault (done)
instance HasXHR api => HasXHR (Vault :> api) where
  type XHR (Vault :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) req

-- | BasicAuth (done)
instance HasXHR api => HasXHR (BasicAuth realm usr :> api) where
  type XHR (BasicAuth realm usr :> api) = BasicAuthData -> XHR api
  xhrWithRoute Proxy req BasicAuthData{..} =
    xhrWithRoute (Proxy :: Proxy api) req {
      reqLogin = Just (ms basicAuthUsername, ms basicAuthPassword)
    , reqWithCredentials = True
    }

-- | Can't find AuthenticateReq
-- instance HasXHR api => HasXHR (AuthProtect tag :> api) where
--   type XHR (AuthProtect tag :> api) = AuthenticateReq (AuthProtect tag) -> XHR api
--   xhrWithRoute Proxy req (AuthenticateReq (val,func)) =
--     xhrWithRoute (Proxy :: Proxy api) (func val req)
