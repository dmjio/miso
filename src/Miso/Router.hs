-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Router
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Router
  ( -- ** Classes
    Router (..)
    -- ** Types
  , Capture
    -- ** Errors
  , RoutingError (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Char
import qualified Data.Char as C
import           Data.String
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad
import           Control.Monad.State
import           GHC.Generics
import           GHC.TypeLits
-----------------------------------------------------------------------------
import           Miso.Types hiding (model)
import           Miso.Util.Parser
import qualified Miso.Util.Lexer as L
import           Miso.Util.Lexer (Lexer)
import           Miso.String
-----------------------------------------------------------------------------
data Route
  = Home 
  | About
  | Widget (Capture Int)
  deriving stock (Generic, Show)
  deriving anyclass Router
-----------------------------------------------------------------------------
route :: MisoString -> RouteParser a -> Either RoutingError a
route = undefined
-----------------------------------------------------------------------------
newtype Capture a = Capture a
  deriving stock (Generic, Show)
  deriving newtype (ToMisoString, FromMisoString)
-----------------------------------------------------------------------------
newtype Path (path :: Symbol) = Path MisoString
  deriving (Generic, Show)
  deriving newtype (ToMisoString, IsString)
-----------------------------------------------------------------------------
data Token
  = PathToken MisoString
  | CaptureToken MisoString
  | FragmentToken MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data RoutingError
  = PathNotFound MisoString
  | DecodeFailure MisoString
  | ParseError MisoString
  | RoutingError
-----------------------------------------------------------------------------
instance Semigroup RoutingError where
  l <> RoutingError = l
  RoutingError <> r = r
  _ <> r = r
-----------------------------------------------------------------------------
instance Monoid RoutingError where
  mempty = RoutingError
-----------------------------------------------------------------------------
type RouteParser route = ExceptT RoutingError (StateT [Token] []) route
-----------------------------------------------------------------------------
capture :: FromMisoString value => RouteParser value
capture = fromMisoStringEither <$> undefined >>= \case
  Left err -> throwError (DecodeFailure (ms err))
  Right r -> pure r
-----------------------------------------------------------------------------
queryParam :: FromMisoString value => RouteParser (Maybe value)
queryParam = fromMisoStringEither <$> undefined >>= \case
  Left err -> throwError (DecodeFailure (ms err))
  Right r -> pure (Just r)
-----------------------------------------------------------------------------
path :: MisoString -> RouteParser MisoString
path specified = do
  PathToken parsed <- pathToken
  when (specified /= parsed) $ throwError (PathNotFound specified)
  pure parsed
-----------------------------------------------------------------------------
class Router route where
  toRoute :: route -> MisoString
  default toRoute :: (Generic route, GRouter (Rep route)) => route -> MisoString
  toRoute = gToRoute . from
  fromRoute :: RouteParser route
  default fromRoute :: (Generic route, GRouter (Rep route)) => RouteParser route
  fromRoute = to <$> gFromRoute
-----------------------------------------------------------------------------
class GRouter f where
  gToRoute :: f route -> MisoString
  gFromRoute :: RouteParser (f route)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (D1 m next) where
  gToRoute (M1 x) = gToRoute x
  gFromRoute = M1 <$> gFromRoute
-----------------------------------------------------------------------------
instance GRouter next => GRouter (C1 m next) where
  gToRoute (M1 x) = gToRoute x
  gFromRoute = M1 <$> gFromRoute
-----------------------------------------------------------------------------
instance GRouter next => GRouter (S1 m next) where
  gToRoute (M1 x) = gToRoute x
  gFromRoute = M1 <$> gFromRoute
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} KnownSymbol path => GRouter (K1 m (Path path)) where
  gToRoute (K1 x) = ms x
  gFromRoute = K1 <$> path chunk
    where
      chunk = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} (FromMisoString a, ToMisoString a) => GRouter (K1 m (Capture a)) where
  gToRoute (K1 x) = ms x
  gFromRoute = K1 <$> capture
-----------------------------------------------------------------------------
instance (FromMisoString a, ToMisoString a) => GRouter (K1 m a) where
  gToRoute (K1 x) = ms x
  gFromRoute = K1 <$> capture
-----------------------------------------------------------------------------
instance GRouter U1 where
  gToRoute U1 
    | x : xs <- show U1 = ms (C.toLower x : xs)
    | otherwise = mempty
  gFromRoute = pure U1
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :*: right) where
  gToRoute (left :*: right) = gToRoute left <> "/" <> gToRoute right
  gFromRoute = liftA2 (:*:) gFromRoute gFromRoute
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :+: right) where
  gToRoute = \case
    L1 m1 -> gToRoute m1
    R1 m1 -> gToRoute m1
  gFromRoute = msum
    [ L1 <$> gFromRoute
    , R1 <$> gFromRoute
    ]
-----------------------------------------------------------------------------
pathToken :: RouteParser Token
pathToken = lift $ satisfy $ \case
  PathToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
captureToken :: RouteParser Token
captureToken = lift $ satisfy $ \case
  CaptureToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
urlLexer :: Lexer Token
urlLexer = msum
  [ CaptureToken <$> captureLexer
  ] where
      captureLexer =
        L.char '/' *> do
          ms <$> some (L.satisfy isAlpha)
-----------------------------------------------------------------------------
