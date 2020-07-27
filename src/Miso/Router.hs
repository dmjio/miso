{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Router
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Router
  ( runRoute
  , route
  , HasRouter
  , RouteT
  , RoutingError (..)
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           GHC.TypeLits
import           Network.HTTP.Types    hiding (Header)
import           Network.URI
import           Servant.API
import           Web.HttpApiData

import           Miso.Html             hiding (text)

-- | Router terminator.
-- The 'HasRouter' instance for 'View' finalizes the router.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "bookId" Int :> View

-- | 'Location' is used to split the path and query of a URI into components.
data Location = Location
  { locPath  :: [Text]
  , locQuery :: Query
  } deriving (Show, Eq, Ord)

-- | When routing, the router may fail to match a location.
data RoutingError = Fail
  deriving (Show, Eq, Ord)

-- | A 'Router' contains the information necessary to execute a handler.
data Router a where
  RChoice       :: Router a -> Router a -> Router a
  RCapture      :: FromHttpApiData x => (x -> Router a) -> Router a
  RQueryParam   :: (FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> (Maybe x -> Router a) -> Router a
  RQueryParams  :: (FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> ([x] -> Router a) -> Router a
  RQueryFlag    :: KnownSymbol sym
                   => Proxy sym -> (Bool -> Router a) -> Router a
  RPath         :: KnownSymbol sym => Proxy sym -> Router a -> Router a
  RPage         :: a -> Router a

-- | This is similar to the @HasServer@ class from @servant-server@.
-- It is the class responsible for making API combinators routable.
-- 'RouteT' is used to build up the handler types.
-- 'Router' is returned, to be interpretted by 'routeLoc'.
class HasRouter layout where
  -- | A mkRouter handler.
  type RouteT layout a :: *
  -- | Transform a mkRouter handler into a 'Router'.
  mkRouter :: Proxy layout -> Proxy a -> RouteT layout a -> Router a

-- | Alternative
instance (HasRouter x, HasRouter y) => HasRouter (x :<|> y) where
  type RouteT (x :<|> y) a = RouteT x a :<|> RouteT y a
  mkRouter _ (a :: Proxy a) ((x :: RouteT x a) :<|> (y :: RouteT y a))
    = RChoice (mkRouter (Proxy :: Proxy x) a x) (mkRouter (Proxy :: Proxy y) a y)

-- | Capture
instance (HasRouter sublayout, FromHttpApiData x) =>
  HasRouter (Capture sym x :> sublayout) where
  type RouteT (Capture sym x :> sublayout) a = x -> RouteT sublayout a
  mkRouter _ a f = RCapture (\x -> mkRouter (Proxy :: Proxy sublayout) a (f x))

-- | QueryParam
instance (HasRouter sublayout, FromHttpApiData x, KnownSymbol sym)
         => HasRouter (QueryParam sym x :> sublayout) where
  type RouteT (QueryParam sym x :> sublayout) a = Maybe x -> RouteT sublayout a
  mkRouter _ a f = RQueryParam (Proxy :: Proxy sym)
    (\x -> mkRouter (Proxy :: Proxy sublayout) a (f x))

-- | QueryParams
instance (HasRouter sublayout, FromHttpApiData x, KnownSymbol sym)
         => HasRouter (QueryParams sym x :> sublayout) where
  type RouteT (QueryParams sym x :> sublayout) a = [x] -> RouteT sublayout a
  mkRouter _ a f = RQueryParams
    (Proxy :: Proxy sym)
    (\x -> mkRouter (Proxy :: Proxy sublayout) a (f x))

-- | QueryFlag
instance (HasRouter sublayout, KnownSymbol sym)
         => HasRouter (QueryFlag sym :> sublayout) where
  type RouteT (QueryFlag sym :> sublayout) a = Bool -> RouteT sublayout a
  mkRouter _ a f = RQueryFlag
    (Proxy :: Proxy sym)
    (\x -> mkRouter (Proxy :: Proxy sublayout) a (f x))

-- | Header
instance HasRouter sublayout => HasRouter (Header sym (x :: *) :> sublayout) where
    type RouteT (Header sym x :> sublayout) a = Maybe x -> RouteT sublayout a
    mkRouter _ a f = mkRouter (Proxy :: Proxy sublayout) a (f Nothing)

-- | Path
instance (HasRouter sublayout, KnownSymbol path)
         => HasRouter (path :> sublayout) where
  type RouteT (path :> sublayout) a = RouteT sublayout a
  mkRouter _ a page = RPath
    (Proxy :: Proxy path)
    (mkRouter (Proxy :: Proxy sublayout) a page)

-- | View
instance HasRouter (View a) where
  type RouteT (View a) x = x
  mkRouter _ _ a = RPage a

-- | Verb
instance HasRouter (Verb m s c a) where
  type RouteT (Verb m s c a) x = x
  mkRouter _ _ a = RPage a

-- | Raw
instance HasRouter Raw where
  type RouteT Raw x = x
  mkRouter _ _ a = RPage a

-- | Use a handler to mkRouter a 'Location'.
-- Normally 'route' should be used instead, unless you want custom
-- handling of string failing to parse as 'URI'.
runRouteLoc :: forall layout a. HasRouter layout
            => Location -> Proxy layout -> RouteT layout a ->  Either RoutingError a
runRouteLoc loc layout page =
  let routing = mkRouter layout (Proxy :: Proxy a) page
  in routeLoc loc routing

-- | Use a handler to mkRouter a location, represented as a 'String'.
-- All handlers must, in the end, return @m a@.
-- 'routeLoc' will choose a mkRouter and return its result.
route
  :: HasRouter layout
  => Proxy layout
  -> RouteT layout a
  -> URI
  -> Either RoutingError a
route layout handler u = runRouteLoc (uriToLocation u) layout handler

-- | Executes router
runRoute
  :: HasRouter layout
  => Proxy layout
  -> RouteT layout (m -> a)
  -> (m -> URI)
  -> m
  -> Either RoutingError a
runRoute layout pages getURI model = ($ model) <$> route layout pages (getURI model)

-- | Use a computed 'Router' to mkRouter a 'Location'.
routeLoc :: Location -> Router a -> Either RoutingError a
routeLoc loc r = case r of
  RChoice a b -> do
    case routeLoc loc a of
      Left Fail -> routeLoc loc b
      Right x -> Right x
  RCapture f -> case locPath loc of
    [] -> Left Fail
    capture:paths ->
      case parseUrlPieceMaybe capture of
        Nothing -> Left Fail
        Just x -> routeLoc loc { locPath = paths } (f x)
  RQueryParam sym f -> case lookup (BS.pack $ symbolVal sym) (locQuery loc) of
    Nothing -> routeLoc loc (f Nothing)
    Just Nothing -> Left Fail
    Just (Just text) -> case parseQueryParamMaybe (decodeUtf8 text) of
      Nothing -> Left Fail
      Just x -> routeLoc loc (f (Just x))
  RQueryParams sym f -> maybe (Left Fail) (\x -> routeLoc loc (f x)) $ do
    ps <- sequence $ snd <$> Prelude.filter
      (\(k, _) -> k == BS.pack (symbolVal sym)) (locQuery loc)
    sequence $ (parseQueryParamMaybe . decodeUtf8) <$> ps
  RQueryFlag sym f -> case lookup (BS.pack $ symbolVal sym) (locQuery loc) of
    Nothing -> routeLoc loc (f False)
    Just Nothing -> routeLoc loc (f True)
    Just (Just _) -> Left Fail
  RPath sym a -> case locPath loc of
    [] -> Left Fail
    p:paths -> if p == T.pack (symbolVal sym)
      then routeLoc (loc { locPath = paths }) a
      else Left Fail
  RPage a ->
    case locPath loc of
      [] -> Right a
      [""] -> Right a
      _ -> Left Fail

-- | Convert a 'URI' to a 'Location'.
uriToLocation :: URI -> Location
uriToLocation uri = Location
  { locPath = decodePathSegments $ BS.pack (uriPath uri)
  , locQuery = parseQuery $ BS.pack (uriQuery uri)
  }
