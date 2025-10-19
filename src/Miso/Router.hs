-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Router
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module introduces a @Router@ that produces "correct-by-construction" URL
-- encoding and decoding from any Haskell algebraic data type. This @Router@ can be used
-- in conjunction with @uriSub@ or @routeSub@ to perform client-side routing. Further
-- it also supports the construction of type-safe links in any @View model action@ via
-- the @href_@ function exported from this module.
--
-- This module can be used in two ways, one is the manual construction of a @Router@
-- as seen below.
--
-- @
--
-- data Route = Widget Int
--    deriving (Show, Eq)
--
-- instance Router Route where
--   routeParser = routes [ Widget \<$\> (path "widget" *\> capture) ]
--   fromRoute (Widget value) = [ toPath "widget", toCapture value ]
--
-- main :: IO ()
-- main = print (runRouter "//widget//10" router)
--
-- > Right (Widget "widget" 10)
-- @
--
-- The second way is using the @Generic@ deriving mechanism. This should ensure that
--
-- @
--
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass     #-}
-- {-# LANGUAGE DeriveGeneric      #-}
--
-- data Route
--  = About
--  | Home
--  | Widget (Capture "thing" Int) (Path "foo") (Capture "other" MisoString) (QueryParam "bar" Int)
--  deriving stock (Generic, Show)
--  deriving anyclass Router
-- @
--
-- The @Generic@ deriving works by converting the constructor name to a path so
--
-- > test :: Either RoutingError Route
-- > test = toRoute "/widget/23/foo/okay?bar=0"
--
-- Decodes as
--
-- > Right (Widget (Capture 23) (Path "foo") (Capture "okay") (QueryParam (Just 0)))
--
-- The order of `Capture` and `Path` matters when defined on your sum type. The order of `QueryParam` and `QueryFlag` does not.
--
-- The router is "reversible" which means it can produce type-safe links using the `href_` function.
--
-- > prettyRoute $ Widget (Capture 23) (Path ("foo")) (Capture ("okay")) (QueryParam (Just 0))
-- > "/widget/23/foo/okay?bar=0"
--
-- This can be used in conjunction with the @href_@ field below to embed type safe links into 'miso' @View model action@ code.
--
-- > button_ [ Miso.Router.href_ (Widget 10) ] [ "click me" ]
--
-- Note: the `Index` constructor is name special, it used to encode the `"/"` path.
--
-- @
--
-- data Route = Index
--   deriving stock (Show, Eq)
--   deriving anyclass (Router)
--
-- main :: IO ()
-- main = print (fromRoute Index)
--
-- -- "/"
-- @
--
-- Lastly, camel-case constructors only use the first hump of the camel.
--
-- @
--
-- data Route = Index | FooBar
--   deriving anyclass Router
--   deriving stock (Show, Eq, Generic)
--
-- main :: IO ()
-- main = print (prettyRoute FooBar)
--
-- "/foo"
-- @
--
-----------------------------------------------------------------------------
module Miso.Router
  ( -- ** Classes
    Router (..)
  , RouteParser
  , GRouter (..)
    -- ** Types
  , Capture (..)
  , Path (..)
  , QueryParam (..)
  , QueryFlag (..)
  , Token (..)
  , URI (..)
    -- ** Errors
  , RoutingError (..)
    -- ** Functions
  , parseURI
  , prettyURI
  , prettyQueryString
    -- ** Manual Routing
  , runRouter
  , routes
    -- ** Construction
  , toQueryFlag
  , toQueryParam
  , toCapture
  , toPath
  , emptyURI
    -- ** Parser combinators
  , queryFlag
  , queryParam
  , capture
  , path
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Functor
import           Data.Proxy
import           Data.Char
import qualified Data.Char as C
import           Data.String
import           Control.Applicative
import           Control.Monad
import           GHC.Generics
import           GHC.TypeLits
-----------------------------------------------------------------------------
import           Miso.Types hiding (model)
import           Miso.Util
import qualified Miso.Html.Property as P
import           Miso.Util.Parser hiding (NoParses)
import qualified Miso.Util.Lexer as L
import           Miso.Util.Lexer (Lexer)
import           Miso.String (ToMisoString, FromMisoString, fromMisoStringEither)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
-- | Type used for representing capture variables
newtype Capture sym a = Capture a
  deriving stock (Generic, Eq, Show)
  deriving newtype (ToMisoString, FromMisoString)
-----------------------------------------------------------------------------
-- | Type used for representing URL paths
newtype Path (path :: Symbol) = Path MisoString
  deriving (Generic, Eq, Show)
  deriving newtype (ToMisoString, IsString)
-----------------------------------------------------------------------------
-- | Type used for representing query flags
newtype QueryFlag (path :: Symbol) = QueryFlag Bool
  deriving (Generic, Eq, Show)
-----------------------------------------------------------------------------
-- | Type used for representing query parameters
newtype QueryParam (path :: Symbol) a = QueryParam (Maybe a)
  deriving (Generic, Eq, Show)
-----------------------------------------------------------------------------
instance (ToMisoString a, KnownSymbol path) => ToMisoString (QueryParam path a) where
  toMisoString (QueryParam maybeVal) =
    maybe mempty (\param -> "?" <> param <> "=" <> val) (ms <$> maybeVal)
      where
        val = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance (FromMisoString a, KnownSymbol path) => FromMisoString (QueryParam path a) where
  fromMisoStringEither x =
    case fromMisoStringEither @a x of
      Right r -> Right $ QueryParam (Just r)
      Left v -> Left v
-----------------------------------------------------------------------------
instance KnownSymbol name => ToMisoString (QueryFlag name) where
  toMisoString = \case
    QueryFlag True ->
      "?" <> ms (symbolVal (Proxy @name))
    QueryFlag False ->
      mempty
-----------------------------------------------------------------------------
-- | A list of tokens are returned from a successful lex of a URI
data Token
  = QueryParamTokens [(MisoString, MisoString)]
  | QueryParamToken MisoString MisoString
  | QueryFlagToken MisoString
  | CaptureOrPathToken MisoString
  | FragmentToken MisoString
  | IndexToken
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Smart constructor for building a 'QueryParamToken'
toQueryParam :: ToMisoString s => MisoString -> s -> Token
toQueryParam k v = QueryParamToken k (ms v)
-----------------------------------------------------------------------------
-- | Smart constructor for building a 'QueryFlagToken'
toQueryFlag :: MisoString -> Token
toQueryFlag = QueryFlagToken
-----------------------------------------------------------------------------
-- | Smart constructor for building a 'Capture' variable 
toCapture :: ToMisoString string => string -> Token
toCapture = CaptureOrPathToken . ms
-----------------------------------------------------------------------------
-- | Smart constructor for building a path fragment
toPath :: MisoString -> Token
toPath = CaptureOrPathToken
-----------------------------------------------------------------------------
-- | Converts a list of @[Token]@ into an actual @URI@.
tokensToURI :: [Token] -> URI
tokensToURI tokens = URI
  { uriPath =
      case tokens of
        IndexToken : _ -> ""
        _ ->
          MS.intercalate "/"
          [ x
          | CaptureOrPathToken x <- filter isPathRelated tokens
          ]
  , uriQueryString =
      M.unions
        [ case queryToken of
            QueryFlagToken k ->
              M.singleton k Nothing
            QueryParamTokens queryParams_ ->
              M.fromList
              [ (k, Just v)
              | (k,v) <- queryParams_
              ]
            QueryParamToken k v ->
              M.singleton k (pure v)
            _ ->
              mempty
        | queryToken <- filter isQuery tokens
        ]
  , uriFragment =
      foldMap ms (filter isFragment tokens)
  } where
      isFragment = \case
        FragmentToken{} -> True
        _ -> False
      isQuery = \case
        QueryFlagToken{} -> True
        QueryParamToken{} -> True
        _ -> False
      isPathRelated = \case
        CaptureOrPathToken {} -> True
        IndexToken {} -> True
        _ -> False
-----------------------------------------------------------------------------
instance ToMisoString Token where
  toMisoString = \case
    CaptureOrPathToken x -> "/" <> x
    QueryFlagToken x -> "?" <> x
    FragmentToken x -> "#" <> x
    QueryParamTokens params ->
      "?" <> MS.intercalate "&"
        [ key <> "=" <> value
        | (key, value) <- params
        ]
    QueryParamToken k v ->
      "?" <> k <> "=" <> v
    IndexToken -> "/"
-----------------------------------------------------------------------------
-- | An error that can occur during lexing / parsing of a URI into a user-defined
-- data type
data RoutingError
  = ParseError MisoString [Token]
  | AmbiguousParse MisoString [Token]
  | LexError MisoString MisoString
  | LexErrorEOF MisoString
  | NoParses MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | State monad for parsing URI 
type RouteParser = ParserT URI [Token] []
-----------------------------------------------------------------------------
-- | Combinator for parsing a capture variable out of a URI
capture :: FromMisoString value => RouteParser value
capture = do
  CaptureOrPathToken capture_ <- captureOrPathToken
  case fromMisoStringEither capture_ of
    Left msg -> fail (fromMisoString (ms msg))
    Right token -> pure token
-----------------------------------------------------------------------------
-- | Combinator for parsing a path out of a URI
path :: MisoString -> RouteParser MisoString
path specified = do
  CaptureOrPathToken parsed <- captureOrPathToken
  when (specified /= parsed) (fail "path")
  pure specified
-----------------------------------------------------------------------------
index :: MisoString -> RouteParser MisoString
index specified = do
  IndexToken <- indexToken
  when (specified /= "index") (fail "index")
  pure "/"
-----------------------------------------------------------------------------
-- | URI parsing
parseURI :: MisoString -> Either MisoString URI
parseURI txt =
  case lexTokens txt of
    Left (L.LexerError err _) -> Left err
    Left (L.UnexpectedEOF eof) -> Left ("EOF: " <> ms (show eof))
    Right tokens -> Right (tokensToURI tokens)
-----------------------------------------------------------------------------
-- | Class used to facilitate routing for miso applications
class Router route where
  fromRoute :: route -> [Token]
  default fromRoute :: (Generic route, GRouter (Rep route)) => route -> [Token]
  fromRoute = gFromRoute . from

  -- | Convert a 'Router route => route' into a 'URI'
  toURI :: route -> URI
  toURI = tokensToURI . fromRoute

  -- | Map a URI back to a route
  route :: URI -> Either RoutingError route
  route = toRoute . prettyURI

  -- | Convenience for specifying a URL as a hyperlink reference in 'View'
  href_ :: route -> Attribute action
  href_ = P.href_ . prettyRoute

  -- | Route pretty printing
  prettyRoute :: route -> MisoString
  prettyRoute = prettyURI . tokensToURI . fromRoute

  -- | Route debugging
  dumpURI :: route -> MisoString
  dumpURI = ms . show . tokensToURI . fromRoute

  -- | Route parsing from a 'MisoString'
  toRoute :: MisoString -> Either RoutingError route
  toRoute input = parseRoute input routeParser

  routeParser :: RouteParser route
  default routeParser :: (Generic route, GRouter (Rep route)) => RouteParser route
  routeParser = to <$> gRouteParser
-----------------------------------------------------------------------------
-- | Smart constructor for building a @RouteParser@
--
-- @
--
-- data Route = Widget MisoString Int
--
-- instance Router Route where
--   routeParser = routes [ Widget \<$\> path "widget" \<*\> capture ]
--   fromRoute (Widget path value) = [ toPath path, toCapture value ]
--
-- router :: Router router => RouteParser router
-- router = routes [ Widget \<$\> path "widget" \<*\> capture ]
--
-- > Right (Widget "widget" 10)
-- @
--
-----------------------------------------------------------------------------
runRouter :: MisoString -> RouteParser route -> Either RoutingError route
runRouter = parseRoute
-----------------------------------------------------------------------------
-- | Convenience for specifying multiple routes
routes :: [ RouteParser route ] -> RouteParser route
routes = foldr (<|>) empty
-----------------------------------------------------------------------------
-- | Generic deriving for 'Router'
class GRouter f where
  gFromRoute :: f route -> [Token]
  gRouteParser :: RouteParser (f route)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (D1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance (KnownSymbol name, GRouter next) => GRouter (C1 (MetaCons name x y) next) where
  gFromRoute (M1 x) =
    case name of
      "index" -> [IndexToken]
      _ -> CaptureOrPathToken name : gFromRoute x
      where
        name = lowercaseStrip $ symbolVal (Proxy @name)
  gRouteParser = do
    case name of
      "index" -> do
        void (index name)
        M1 <$> gRouteParser
      _ -> do
        void (path name)
        M1 <$> gRouteParser
      where
        name = lowercaseStrip $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (S1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall path m . KnownSymbol path => GRouter (K1 m (Path path)) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser = K1 (Path chunk) <$ path chunk
    where
      chunk = ms $ symbolVal (Proxy :: Proxy path)
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} (FromMisoString a, ToMisoString a) => GRouter (K1 m (Capture sym a)) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser = K1 <$> capture
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall param m a . (ToMisoString a, FromMisoString a, KnownSymbol param) =>
  GRouter (K1 m (QueryParam param a)) where
    gFromRoute (K1 (QueryParam maybeParam)) =
      case maybeParam of
        Nothing -> []
        Just v -> pure $ QueryParamToken (ms (symbolVal (Proxy @param))) (ms v)
    gRouteParser = K1 <$> queryParam
-----------------------------------------------------------------------------
-- | Query parameter parser from a route
queryParam
  :: forall param a . (FromMisoString a, KnownSymbol param)
  => RouteParser (QueryParam param a)
queryParam = do
  URI {..} <- askParser
  QueryParam <$> do
    case M.lookup (ms (symbolVal (Proxy @param))) uriQueryString of
      Just (Just value) ->
        case fromMisoStringEither value of
          Left _ -> pure Nothing
          Right parsed -> pure (Just parsed)
      _ -> pure Nothing
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall flag m . KnownSymbol flag => GRouter (K1 m (QueryFlag flag)) where
  gFromRoute (K1 (QueryFlag specified))
    | specified = [ QueryFlagToken flag ]
    | otherwise = []
        where
          flag = ms (symbolVal (Proxy @flag))
  gRouteParser = K1 <$> queryFlag
-----------------------------------------------------------------------------
-- | Query flag parser from a route
queryFlag :: forall flag . KnownSymbol flag => RouteParser (QueryFlag flag)
queryFlag = do
  URI {..} <- askParser
  pure $ QueryFlag $ isJust (M.lookup flag uriQueryString)
    where
      flag = ms $ symbolVal (Proxy @flag)
-----------------------------------------------------------------------------
instance Router a => GRouter (K1 m a) where
  gFromRoute (K1 x) = fromRoute x
  gRouteParser = K1 <$> routeParser
-----------------------------------------------------------------------------
instance GRouter U1 where
  gFromRoute U1 = []
  gRouteParser = pure U1
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :*: right) where
  gFromRoute (left :*: right) = gFromRoute left <> gFromRoute right
  gRouteParser = liftA2 (:*:) gRouteParser gRouteParser
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :+: right) where
  gFromRoute = \case
    L1 m1 -> gFromRoute m1
    R1 m1 -> gFromRoute m1
  gRouteParser = foldr (<|>) empty
    [ L1 <$> gRouteParser
    , R1 <$> gRouteParser
    ]
-----------------------------------------------------------------------------
captureOrPathToken :: RouteParser Token
captureOrPathToken = satisfy $ \case
  CaptureOrPathToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
indexToken :: RouteParser Token
indexToken = satisfy $ \case
  IndexToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
-- | Lexing for a URI
uriLexer :: Lexer [Token]
uriLexer = do
  tokens <- some lexer
  void $ optional (L.char '/')
  pure (postProcess tokens)
    where
      postProcess :: [Token] -> [Token]
      postProcess = concatMap $ \case
        QueryParamTokens queryParams_ ->
          [ QueryParamToken k v
          | (k,v) <- queryParams_
          ]
        x -> pure x
      lexer = msum
        [ queryFlagLexer
        , captureOrPathLexer
        , queryParamLexer
        , fragmentLexer
        , indexLexer
        ] where
            indexLexer =
              IndexToken <$ L.char '/'
            captureOrPathLexer = do
              void (L.char '/')
              CaptureOrPathToken <$> chars
            fragmentLexer = do
              void (L.char '#')
              FragmentToken <$> fragment
            queryFlagLexer = do
              void (L.char '?')
              QueryFlagToken <$> query
            queryParamLexer = QueryParamTokens <$> do
              void (L.char '?')
              sepBy (L.char '&') $ do
                key <- query
                void (L.char '=')
                value <- query
                pure (key, value)
-----------------------------------------------------------------------------
chars :: Lexer MisoString
chars = MS.concat <$> some pchar
-----------------------------------------------------------------------------
pchar :: Lexer MisoString
pchar = unreserved <|> pctEncoded <|> subDelims <|> L.string ":" <|> L.string "@"
-----------------------------------------------------------------------------
fragment :: Lexer MisoString
fragment = query
-----------------------------------------------------------------------------
query :: Lexer MisoString
query = foldr (<|>) empty
  [ MS.concat <$> some pchar
  , L.string "/"
  , L.string "?"
  ]
-----------------------------------------------------------------------------
subDelims :: Lexer MisoString
subDelims = fmap ms <$> L.satisfy $ \x -> x `elem` ("!$&'()*+,;=" :: String)
-----------------------------------------------------------------------------
unreserved :: Lexer MisoString
unreserved = ms <$> do
  L.satisfy $ \x -> or
    [ isAlphaNum x
    , x == '-'
    , x == '.'
    , x == '_'
    , x == '~'
    ]
-----------------------------------------------------------------------------
pctEncoded :: Lexer MisoString
pctEncoded = do
  pct <- L.char '%'
  d1 <- hexDig
  d2 <- hexDig
  pure (ms pct <> ms d1 <> ms d2)
-----------------------------------------------------------------------------
hexDig :: Lexer Char
hexDig = L.satisfy $ \x -> or
  [ x `elem` [ '0' .. '9' ]
  , x `elem` [ 'a' .. 'f' ]
  , x `elem` [ 'A' .. 'F' ]
  ]
-----------------------------------------------------------------------------
lexTokens :: MisoString -> Either L.LexerError [Token]
lexTokens input =
  case L.runLexer uriLexer (L.mkStream input) of
    Right (tokens, _) -> Right tokens
    Left x -> Left x
-----------------------------------------------------------------------------
parseRoute :: MisoString -> RouteParser a -> Either RoutingError a
parseRoute input parser =
  case L.runLexer uriLexer (L.mkStream input) of
    Left (L.LexerError lexErrorMessage _) ->
      Left (LexError input lexErrorMessage)
    Left (L.UnexpectedEOF _) ->
      Left (LexErrorEOF input)
    Right (tokens, _) -> do
      let
        uri = tokensToURI tokens
        isCapturePathOrIndex = \case
          CaptureOrPathToken{} -> True
          IndexToken{} -> True
          _ -> False
      case runParserT parser uri (filter isCapturePathOrIndex tokens) of
        [(x, [])]  ->
          Right x
        [(_, leftovers)]  ->
          Left $ ParseError input leftovers
        []  ->
          Left $ NoParses input
        (_, leftovers) : _  ->
          Left $ AmbiguousParse input leftovers
-----------------------------------------------------------------------------
lowercaseStrip :: String -> MisoString
lowercaseStrip (x:xs) = ms (C.toLower x : takeWhile C.isLower xs)
lowercaseStrip x = ms x
-----------------------------------------------------------------------------
