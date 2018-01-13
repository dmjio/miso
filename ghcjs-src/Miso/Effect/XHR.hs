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
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect.XHR where

import Data.Proxy
import GHC.TypeLits
import JavaScript.Web.XMLHttpRequest
import Miso.String
import Servant.API

-- | Still a WIP, use ghcjs-base XHR for now, or other

-- | Intermediate type for accumulation
data RouteInfo
  = RouteInfo { riPath :: String
              , riMethod :: Method
              } deriving (Show, Eq)

-- | Class for `XHR`
class HasXHR api where
  type XHR api :: *
  xhrWithRoute
    :: Proxy api
    -> RouteInfo
    -> XHR api

-- | Result of using `XHR`
type Result a = IO (Either MisoString a)

-- | Verb
instance {-# OVERLAPPABLE #-}
  ( MimeUnrender ct a
  , ReflectMethod method
  , cts' ~ (ct ': cts)
  ) => HasXHR (Verb method status cts' a) where
  type XHR (Verb method status cts' a) = Result a
  xhrWithRoute Proxy _ = undefined
    -- snd <$> performRequestCT (Proxy :: Proxy ct) method req
    --   where method = reflectMethod (Proxy :: Proxy method)

-- | Verb NoContent
instance {-# OVERLAPPING #-}
  ReflectMethod method => HasXHR (Verb method status cts NoContent) where
  type XHR (Verb method status cts NoContent) = Result NoContent
  xhrWithRoute Proxy _ = undefined
    -- performRequestNoBody method req >> return NoContent
    --   where method = reflectMethod (Proxy :: Proxy method)

-- | Verb, with HEADERS
instance {-# OVERLAPPING #-}
  ( MimeUnrender ct a, BuildHeadersTo ls, ReflectMethod method, cts' ~ (ct ': cts)
  ) => HasXHR (Verb method status cts' (Headers ls a)) where
  type XHR (Verb method status cts' (Headers ls a)) = Result (Headers ls a)
  xhrWithRoute Proxy _ = undefined
    -- let method = reflectMethod (Proxy :: Proxy method)
    -- (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) method req
    -- return $ Headers { getResponse = resp
    --                  , getHeadersHList = buildHeadersTo hdrs
    --                  }


-- | `Headers`, with `NoContent`
instance {-# OVERLAPPING #-}
  ( BuildHeadersTo ls, ReflectMethod method
  ) => HasXHR (Verb method status cts (Headers ls NoContent)) where
  type XHR (Verb method status cts (Headers ls NoContent)) = Result (Headers ls NoContent)
  xhrWithRoute Proxy _ = undefined
    -- let method = reflectMethod (Proxy :: Proxy method)
    -- hdrs <- performRequestNoBody method req
    -- return $ Headers { getResponse = NoContent
    --                  , getHeadersHList = buildHeadersTo hdrs
    --                  }

-- | Capture
instance (ToHttpApiData a, HasXHR api, KnownSymbol sym) => HasXHR (Capture sym a :> api) where
  type XHR (Capture sym a :> api) = a -> XHR api
  xhrWithRoute Proxy _ (_ :: a) = undefined

#if MIN_VERSION_servant(0,8,1)
-- | CaptureAll
instance (KnownSymbol capture, ToHttpApiData a, HasXHR sublayout)
   => HasXHR (CaptureAll capture a :> sublayout) where
  type XHR (CaptureAll capture a :> sublayout) = [a] -> XHR sublayout
  xhrWithRoute Proxy _ _ = undefined
    -- xhrWithRoute (Proxy :: Proxy sublayout)
    --   (foldl' (flip appendToPath) req ps)
#endif

-- | Path (done)
instance (HasXHR api, KnownSymbol sym) => HasXHR (sym :> api) where
  type XHR (sym :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) newReq
    where
      newReq = req {
        riPath = riPath req ++ "/" ++ symbolVal (Proxy :: Proxy sym)
      }

-- | Raw (not supported)
-- instance HasXHR Raw where
--   type XHR Raw = XHR (Response MisoString)
--   xhrWithRoute Proxy _ = putStrLn "Raw is not supported"

-- | Alternate
instance (HasXHR left, HasXHR right) => HasXHR (left :<|> right) where
  type XHR (left :<|> right) = XHR left :<|> XHR right
  xhrWithRoute Proxy s =
    xhrWithRoute (Proxy :: Proxy left) s :<|>
      xhrWithRoute (Proxy :: Proxy right) s

-- | Header
instance (ToHttpApiData a, HasXHR api, KnownSymbol sym) => HasXHR (Header sym a :> api) where
  type XHR (Header sym a :> api) = Maybe a -> XHR api
  xhrWithRoute Proxy _ = undefined

-- | HttpVersion
instance HasXHR api => HasXHR (HttpVersion :> api) where
  type XHR (HttpVersion :> api) = XHR api
  xhrWithRoute Proxy = xhrWithRoute (Proxy :: Proxy api)

-- | Query param
instance (KnownSymbol sym, ToHttpApiData a, HasXHR api) => HasXHR (QueryParam sym a :> api) where
  type XHR (QueryParam sym a :> api) = Maybe a -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Query param(s)
instance (KnownSymbol sym, ToHttpApiData a, HasXHR api) => HasXHR (QueryParams sym a :> api) where
  type XHR (QueryParams sym a :> api) = [a] -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Query flag
instance (KnownSymbol sym, HasXHR api) => HasXHR (QueryFlag sym :> api) where
  type XHR (QueryFlag sym :> api) = Bool -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Request Body
instance (MimeRender ct a, HasXHR api) => HasXHR (ReqBody (ct ': cts) a :> api) where
  type XHR (ReqBody (ct ': cts) a :> api) = a -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Remote host (done)
instance HasXHR api => HasXHR (RemoteHost :> api) where
  type XHR (RemoteHost :> api) = XHR api
  xhrWithRoute Proxy req =  xhrWithRoute (Proxy :: Proxy api) req

-- | IsSecure host (done)
instance HasXHR api => HasXHR (IsSecure :> api) where
  type XHR (IsSecure :> api) = XHR api
  xhrWithRoute Proxy req =  xhrWithRoute (Proxy :: Proxy api) req

-- | WithNamedContext (done)
instance HasXHR api => HasXHR (WithNamedContext :> api) where
  type XHR (WithNamedContext :> api) = XHR api
  xhrWithRoute Proxy req =  xhrWithRoute (Proxy :: Proxy api) req

-- | Vault (done)
instance HasXHR api => HasXHR (Vault :> api) where
  type XHR (Vault :> api) = XHR api
  xhrWithRoute Proxy req = xhrWithRoute (Proxy :: Proxy api) req

-- | BasicAuth
instance HasXHR api => HasXHR (BasicAuth realm usr :> api) where
  type XHR (BasicAuth realm usr :> api) = BasicAuthData -> XHR api
  xhrWithRoute Proxy _ _ = undefined

-- | Can't find AuthenticateReq
-- instance HasXHR api => HasXHR (AuthProtect tag :> api) where
--   type XHR (AuthProtect tag :> api) = AuthenticateReq (AuthProtect tag) -> XHR api
--   xhrWithRoute Proxy req (AuthenticateReq (val,func)) =
--     xhrWithRoute (Proxy :: Proxy api) (func val req)





-- xhrWithRoute (Proxy :: Proxy api)
--                     (let ctProxy = Proxy :: Proxy ct
--                      in setReqBodyLBS (mimeRender ctProxy body)
--                                   -- We use first contentType from the Accept list
--                                   (contentType ctProxy)
--                                   req
--                     )


-- xhrJSON :: FromJSON json => Request -> IO (Response json)
-- xhrJSON req = do
--   r <- xhr' req
--   case contents r of
--     Nothing -> pure r { contents = Just Null }
--     Just jsstring -> do
--       x <- parse (unsafeCoerce jsstring)
--       pure $ r { contents = Just x }


-- import Data.JSString
-- import GHCJS.Foreign.Callback
-- import GHCJS.Nullable
-- import GHCJS.Types
-- import Prelude                hiding (lines)

-- data ReadyState
--   = UNSENT
--   -- ^ XHR has been created. open() not called yet.
--   | OPENED
--   -- ^ open() has been called.
--   | HEADERS_RECEIVED
--   -- ^ send() has been called, and headers and status are available.
--   | LOADING
--   -- ^ Downloading; responseText holds partial data.
--   | DONE
--   -- ^ The operation is complete.
--   deriving (Show, Eq, Enum)

-- data ResponseType
--   = DOMStringType
--   | ArrayBufferType
--   | BlobType
--   | DocumentType
--   | JSONType
--   | UnknownXHRType
--   deriving (Show, Eq)

-- newtype Document = Document JSVal
-- newtype XHR = XHR JSVal

-- foreign import javascript unsafe "$r = new XMLHttpRequest();"
--   newXHR :: IO XHR

-- foreign import javascript unsafe "$1.abort();"
--   abort :: XHR -> IO ()

-- foreign import javascript unsafe "$r = $1.responseURL;"
--   responseURL :: XHR -> IO JSString

-- foreign import javascript unsafe "$r = $1.readyState;"
--   readyState' :: XHR -> IO Int

-- foreign import javascript unsafe "$r = $1.responseType;"
--   responseType' :: XHR -> IO JSString

-- responseType :: XHR -> IO ResponseType
-- {-# INLINE responseType #-}
-- responseType xhr =
--   responseType' xhr >>= \case
--     "" -> pure DOMStringType
--     "blob" -> pure BlobType
--     "document" -> pure DocumentType
--     "json" -> pure JSONType
--     "arraybuffer" -> pure ArrayBufferType
--     "text" -> pure DOMStringType
--     _ -> pure UnknownXHRType

-- readyState :: XHR -> IO ReadyState
-- {-# INLINE readyState #-}
-- readyState xhr = toEnum <$> readyState' xhr

-- -- request.open("GET", "foo.txt", true);
-- foreign import javascript unsafe "$1.open($2, $3, $4);"
--   open :: XHR -> JSString -> JSString -> Bool -> IO ()

-- foreign import javascript unsafe "$1.send();"
--   send :: XHR -> IO ()

-- foreign import javascript unsafe "$1.setRequestHeader($2,$3);"
--   setRequestHeader :: XHR -> JSString -> JSString -> IO ()

-- foreign import javascript unsafe "$1.onreadystatechanged = $2;"
--   onReadyStateChanged :: XHR -> Callback (JSVal -> IO ()) -> IO ()

-- foreign import javascript unsafe "$r = $1.getAllResponseHeaders();"
--   getAllResponseHeaders' :: XHR -> IO (Nullable JSString)

-- foreign import javascript unsafe "$r = $1.getResponseHeader($2);"
--   getResponseHeader' :: XHR -> JSString -> IO (Nullable JSString)

-- getResponseHeader :: XHR -> JSString -> IO (Maybe JSString)
-- {-# INLINE getResponseHeader #-}
-- getResponseHeader xhr key =
--   nullableToMaybe <$> getResponseHeader' xhr key

-- foreign import javascript unsafe "$r = $1.response;"
--   response' :: XHR -> IO (Nullable JSVal)

-- foreign import javascript unsafe "$r = $1.status;"
--   status' :: XHR -> IO (Nullable Int)

-- foreign import javascript unsafe "$r = $1.statusText;"
--   statusText' :: XHR -> IO (Nullable JSString)

-- foreign import javascript unsafe "$1.overrideMimeType($2);"
--   overrideMimeType :: XHR -> JSString -> IO ()

-- foreign import javascript unsafe "$r = $1.timeout;"
--   timeout :: XHR -> IO Int

-- foreign import javascript unsafe "$1.withCredentials = true;"
--   withCredentials :: XHR -> IO ()

-- foreign import javascript unsafe "$1.response ? true : false"
--   hasResponse :: XHR -> IO Bool

-- getAllResponseHeaders :: XHR -> IO (Maybe [JSString])
-- {-# INLINE getAllResponseHeaders #-}
-- getAllResponseHeaders = \xhr -> do
--   result <- getAllResponseHeaders' xhr
--   pure $ lines <$> nullableToMaybe result
