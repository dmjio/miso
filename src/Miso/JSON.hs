-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
#if __GLASGOW_HASKELL__ <= 865
{-# LANGUAGE UndecidableInstances       #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A JSON library specialized to MisoString for performance. Largely based
-- on [microaeson](https://hackage-content.haskell.org/package/microaeson).
--
-- Uses JS runtime primitives `JSON.stringify()` and `JSON.parse()`. 
--
----------------------------------------------------------------------------
module Miso.JSON
  ( -- * JSON
    -- ** Core JSON types
    Value(..)
  , Object
  , Pair
  , Result (..)
    -- ** Constructors
  , (.=)
  , object
  , emptyArray
  , emptyObject
    -- ** Accessors
  , (.:)
  , (.:?)
  , (.:!)
  , (.!=)
    -- * Encoding and decoding
  , encode
  , decode
    -- * Prism-style parsers
  , withObject
  , withText
  , withArray
  , withNumber
  , withBool
    -- * Type conversion
  , FromJSON(parseJSON)
  , Parser (..)
  , parseMaybe
  , ToJSON(toJSON)
  -- * Misc.
  , fromJSON
  , parseEither
  , eitherDecode
  , typeMismatch
  -- * Pretty
  , encodePretty
  , encodePretty'
  , defConfig
  , Config (..)
  -- * FFI
  , fromJSVal_Value
  , toJSVal_Value
  , jsonStringify
  , jsonParse
  -- * Options
  , Options (..)
  , defaultOptions
  -- * Generics
  , GToJSON (..)
  , genericToJSON
  , GFromJSON (..)
  , genericParseJSON
  -- * Modifiers
  , camelTo2
  ) where
----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
import qualified GHCJS.Marshal as Marshal
#endif
----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
#if __GLASGOW_HASKELL__ <= 865
import           Control.Monad.Fail
import           GHC.Natural (Natural)
#endif
import           Data.Char
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Int
import           GHC.Natural (naturalToInteger, naturalFromInteger )
import           GHC.TypeLits
import           Data.Kind
import           Data.Word
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
----------------------------------------------------------------------------
import           Miso.DSL.FFI
import           Miso.String (FromMisoString, ToMisoString, MisoString, ms, singleton, pack)
import qualified Miso.String as MS
import           Miso.JSON.Types
import qualified Miso.JSON.Parser as Parser
----------------------------------------------------------------------------
#ifndef VANILLA
import           Control.Monad.Trans.Maybe
#endif
----------------------------------------------------------------------------
infixr 8 .=
(.=) :: ToJSON v => MisoString -> v -> Pair
k .= v  = (k, toJSON v)
----------------------------------------------------------------------------
-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object . M.fromList
----------------------------------------------------------------------------
-- | The empty JSON 'Object' (i.e. @{}@).
emptyObject :: Value
emptyObject = Object mempty
----------------------------------------------------------------------------
-- | The empty JSON 'Array' (i.e. @[]@).
emptyArray :: Value
emptyArray = Array mempty
----------------------------------------------------------------------------
(.:) :: FromJSON a => Object -> MisoString -> Parser a
m .: k = maybe (pfail "key not found") parseJSON (M.lookup k m)
----------------------------------------------------------------------------
(.:?) :: FromJSON a => Object -> MisoString -> Parser (Maybe a)
m .:? k = maybe (pure Nothing) parseJSON (M.lookup k m)
----------------------------------------------------------------------------
(.:!) :: FromJSON a => Object -> MisoString -> Parser (Maybe a)
m .:! k = maybe (pure Nothing) (fmap Just . parseJSON) (M.lookup k m)
----------------------------------------------------------------------------
(.!=) :: Parser (Maybe a) -> a -> Parser a
mv .!= def = fmap (maybe def id) mv
----------------------------------------------------------------------------
class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
  toJSON = genericToJSON defaultOptions
----------------------------------------------------------------------------
genericToJSON :: (Generic a, GToJSON (Rep a)) => Options -> a -> Value
genericToJSON opts = object . gToJSON opts [] . from
----------------------------------------------------------------------------
data Options
  = Options
  { fieldLabelModifier :: String -> String
  }
----------------------------------------------------------------------------
defaultOptions :: Options
defaultOptions = Options { fieldLabelModifier = \x -> x }
----------------------------------------------------------------------------
camelTo2 :: Char -> String -> String
camelTo2 c = Prelude.map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs
----------------------------------------------------------------------------
class GToJSON (f :: Type -> Type) where
  gToJSON :: Options -> [Pair] -> f a -> [Pair]
----------------------------------------------------------------------------
instance GToJSON a => GToJSON (D1 i a) where
  gToJSON opts acc (M1 x) = gToJSON opts acc x
----------------------------------------------------------------------------
instance GToJSON a => GToJSON (C1 i a) where
  gToJSON opts acc (M1 x) = gToJSON opts acc x
----------------------------------------------------------------------------
instance (GToJSON a, GToJSON b) => GToJSON (a :*: b) where
  gToJSON opts acc (x :*: y) = gToJSON opts acc x <> gToJSON opts acc y
----------------------------------------------------------------------------
instance (TypeError ('Text "Sum types unsupported"), GToJSON a, GToJSON b) => GToJSON (a :+: b) where
  gToJSON opts acc = \case
    L1 x -> gToJSON opts acc x
    R1 x -> gToJSON opts acc x
----------------------------------------------------------------------------
instance GToJSON U1 where
  gToJSON _ acc U1 = acc
----------------------------------------------------------------------------
instance GToJSON V1 where
  gToJSON _ acc _ = acc
----------------------------------------------------------------------------
instance (Selector s, ToJSON a) => GToJSON (S1 s (K1 i a)) where
  gToJSON opts acc (M1 (K1 x)) = ms field .= toJSON x : acc
    where
      field :: String
      field = fieldLabelModifier opts $ selName (undefined :: S1 s (K1 i a) ())
----------------------------------------------------------------------------
instance ToJSON () where
  toJSON () = Array []
----------------------------------------------------------------------------
instance ToJSON Value where
  toJSON = id
----------------------------------------------------------------------------
instance ToJSON Char where
  toJSON c = String (singleton c)
----------------------------------------------------------------------------
instance ToJSON Bool where
  toJSON = Bool
----------------------------------------------------------------------------
instance ToJSON a => ToJSON [a] where
  toJSON = Array . Prelude.map toJSON
----------------------------------------------------------------------------
instance ToJSON v => ToJSON (M.Map MisoString v) where
  toJSON = Object . M.map toJSON
----------------------------------------------------------------------------
instance ToJSON a => ToJSON (Maybe a) where
  toJSON = \case
    Nothing -> Null
    Just a -> toJSON a
----------------------------------------------------------------------------
instance (ToJSON a,ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = Array [toJSON a, toJSON b]
----------------------------------------------------------------------------
instance (ToJSON a,ToJSON b,ToJSON c) => ToJSON (a,b,c) where
  toJSON (a,b,c) = Array [toJSON a, toJSON b, toJSON c]
----------------------------------------------------------------------------
instance (ToJSON a,ToJSON b,ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
  toJSON (a,b,c,d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]
----------------------------------------------------------------------------
instance ToJSON MisoString where
  toJSON = String
----------------------------------------------------------------------------
instance ToJSON Float where
  toJSON = Number . realToFrac
----------------------------------------------------------------------------
instance ToJSON Double where
  toJSON = Number
----------------------------------------------------------------------------
instance ToJSON Int    where  toJSON = Number . realToFrac
instance ToJSON Int8   where  toJSON = Number . realToFrac
instance ToJSON Int16  where  toJSON = Number . realToFrac
instance ToJSON Int32  where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
instance ToJSON Word   where  toJSON = Number . realToFrac
instance ToJSON Word8  where  toJSON = Number . realToFrac
instance ToJSON Word16 where  toJSON = Number . realToFrac
instance ToJSON Word32 where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Int64  where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Word64 where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Integer where toJSON = Number . fromInteger
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Natural where toJSON = Number . fromInteger . naturalToInteger
----------------------------------------------------------------------------
newtype Parser a = Parser { unParser :: Either MisoString a }
  deriving (Functor, Applicative, Monad)
----------------------------------------------------------------------------
instance MonadFail Parser where
  fail = pfail . pack
----------------------------------------------------------------------------
instance Alternative Parser where
  empty = Parser (Left mempty)
  Parser (Left _) <|> r = r
  l <|> _ = l
----------------------------------------------------------------------------
instance MonadPlus Parser
----------------------------------------------------------------------------
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v =
  case parseEither m v of
    Left _ -> Nothing
    Right r -> Just r 
----------------------------------------------------------------------------
parseEither :: (a -> Parser b) -> a -> Either MisoString b
parseEither m v = unParser (m v)
----------------------------------------------------------------------------
pfail :: MisoString -> Parser a
pfail message = Parser (Left message)
----------------------------------------------------------------------------
class FromJSON a where
  parseJSON :: Value -> Parser a
  default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
  parseJSON = genericParseJSON defaultOptions
----------------------------------------------------------------------------
class GFromJSON (f :: Type -> Type) where
  gParseJSON :: Options -> Value -> Parser (f a)
----------------------------------------------------------------------------
genericParseJSON :: (Generic a, GFromJSON (Rep a)) => Options -> Value -> Parser a
genericParseJSON opts value = to <$> gParseJSON opts value
----------------------------------------------------------------------------
instance GFromJSON a => GFromJSON (D1 i a) where
  gParseJSON opts x = M1 <$> gParseJSON opts x
----------------------------------------------------------------------------
instance GFromJSON a => GFromJSON (C1 i a) where
  gParseJSON opts x = M1 <$> gParseJSON opts x
----------------------------------------------------------------------------
instance (GFromJSON a, GFromJSON b) => GFromJSON (a :*: b) where
  gParseJSON opts x = (:*:) <$> gParseJSON opts x <*> gParseJSON opts x
----------------------------------------------------------------------------
instance (GFromJSON a, GFromJSON b) => GFromJSON (a :+: b) where
  gParseJSON opts x = (L1 <$> gParseJSON opts x) <|> (R1 <$> gParseJSON opts x)
----------------------------------------------------------------------------
instance GFromJSON U1 where
  gParseJSON _ _ = pure U1
----------------------------------------------------------------------------
instance (Selector s, FromJSON a) => GFromJSON (S1 s (K1 i a)) where
  gParseJSON opts = \case
    Object o ->
      M1 . K1 <$> o .: ms field
    v ->
      M1 . K1 <$> parseJSON v
    where
      field = fieldLabelModifier opts $ selName (undefined :: S1 s (K1 i a) ())
----------------------------------------------------------------------------
instance FromJSON Value where
  parseJSON = pure
----------------------------------------------------------------------------
instance FromJSON Bool where
  parseJSON = withBool "Bool" pure
----------------------------------------------------------------------------
instance FromJSON MisoString where
  parseJSON = withText "MisoString" pure
----------------------------------------------------------------------------
instance FromJSON a => FromJSON [a] where
  parseJSON = withArray "[a]" (mapM parseJSON)
----------------------------------------------------------------------------
instance FromJSON Double where
  parseJSON Null = pure (0/0)
  parseJSON j    = withNumber "Double" pure j
----------------------------------------------------------------------------
instance FromJSON Float where
  parseJSON Null = pure (0/0)
  parseJSON j    = withNumber "Float" (pure . realToFrac) j
----------------------------------------------------------------------------
instance FromJSON Integer where
  parseJSON = withNumber "Integer" (pure . round)
----------------------------------------------------------------------------
instance FromJSON Natural where
  parseJSON = withNumber "Natural" parseNumber
    where parseNumber d | d < 0 = pfail "cannot parse negative number as Natural"
                        | isNaN d = pfail "cannot parse NaN as Natural"
                        | otherwise  = pure $ naturalFromInteger $ fromInteger $ round d 
----------------------------------------------------------------------------
instance FromJSON Int where
  parseJSON = withNumber "Int" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int8 where
  parseJSON = withNumber "Int8" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int16 where
  parseJSON = withNumber "Int16" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int32 where
  parseJSON = withNumber "Int32" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int64 where
  parseJSON = withNumber "Int64" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word where
  parseJSON = withNumber "Word" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word8 where
  parseJSON = withNumber "Word8" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word16 where
  parseJSON = withNumber "Word16" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word32 where
  parseJSON = withNumber "Word32" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word64 where
  parseJSON = withNumber "Word64" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON () where
  parseJSON = withArray "()" $ \lst ->
    case lst of
      [] -> pure ()
      _  -> pfail "expected ()"
----------------------------------------------------------------------------
instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
  parseJSON = withArray "(a,b)" $ \lst ->
    case lst of
      [a,b] -> liftM2 (,) (parseJSON a) (parseJSON b)
      _     -> pfail "expected (a,b)"
----------------------------------------------------------------------------
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a,b,c) where
  parseJSON = withArray "(a,b,c)" $ \lst ->
    case lst of
      [a,b,c] -> liftM3 (,,) (parseJSON a) (parseJSON b) (parseJSON c)
      _       -> pfail "expected (a,b,c)"
----------------------------------------------------------------------------
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a,b,c,d) where
  parseJSON = withArray "(a,b,c,d)" $ \lst ->
    case lst of
      [a,b,c,d] -> liftM4 (,,,) (parseJSON a) (parseJSON b) (parseJSON c) (parseJSON d)
      _         -> pfail "expected (a,b,c,d)"
----------------------------------------------------------------------------
instance FromJSON a => FromJSON (Maybe a) where
  parseJSON Null = pure Nothing
  parseJSON j    = Just <$> parseJSON j
----------------------------------------------------------------------------
instance FromJSON Ordering where
  parseJSON = withText "{'LT','EQ','GT'}" $ \s ->
    case s of
      "LT" -> pure LT
      "EQ" -> pure EQ
      "GT" -> pure GT
      _    -> pfail "expected {'LT','EQ','GT'}"
----------------------------------------------------------------------------
instance FromJSON v => FromJSON (Map MisoString v) where
  parseJSON = withObject "Map MisoString v" $ mapM parseJSON
----------------------------------------------------------------------------
withBool :: MisoString -> (Bool -> Parser a) -> Value -> Parser a
withBool _        f (Bool arr) = f arr
withBool expected _ v          = typeMismatch expected v
----------------------------------------------------------------------------
withText :: MisoString -> (MisoString -> Parser a) -> Value -> Parser a
withText _        f (String txt) = f txt
withText expected _ v            = typeMismatch expected v
----------------------------------------------------------------------------
withArray :: MisoString -> ([Value] -> Parser a) -> Value -> Parser a
withArray _        f (Array lst) = f lst
withArray expected _ v           = typeMismatch expected v
----------------------------------------------------------------------------
withObject :: MisoString -> (Object -> Parser a) -> Value -> Parser a
withObject _        f (Object obj) = f obj
withObject expected _ v            = typeMismatch expected v
----------------------------------------------------------------------------
withNumber :: MisoString -> (Double -> Parser a) -> Value -> Parser a
withNumber _        f (Number n) = f n
withNumber expected _ v          = typeMismatch expected v
----------------------------------------------------------------------------
typeMismatch :: MisoString -> Value -> Parser a
typeMismatch expected _ = pfail ("expected " <> expected)
----------------------------------------------------------------------------
#ifdef VANILLA
encode :: ToJSON a => a -> MisoString
encode = ms . toJSON
#else
encode :: ToJSON a => a -> MisoString
encode x = unsafePerformIO $ jsonStringify =<< toJSVal_Value (toJSON x)
#endif
----------------------------------------------------------------------------
instance FromMisoString Value where
  fromMisoStringEither = Parser.decodePure
----------------------------------------------------------------------------
instance ToMisoString Value where
  toMisoString = \case
    String s ->
      "\"" <> s <> "\""
    Number n ->
      ms n
    Null ->
      "null"
    Array xs ->
      "[" <> MS.intercalate "," (fmap ms xs) <> "]"
    Bool True ->
      "true"
    Bool False ->
      "false"
    Object o ->
      "{" <>
        MS.intercalate "," [ "\"" <> k <> "\"" <> ":" <> ms v | (k,v) <- M.toList o ]
      <> "}"
----------------------------------------------------------------------------
#ifdef VANILLA
decode :: FromJSON a => MisoString -> Maybe a
decode s
  | Right x <- Parser.decodePure s
  , Success v <- fromJSON x = v
  | otherwise = Nothing
#else
decode :: FromJSON a => MisoString -> Maybe a
decode s
  | Right x <- eitherDecode s = Just x
  | otherwise = Nothing
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = JSON.stringify($1, null, $2)"
  encodePretty_ffi :: JSVal -> Int -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return JSON.stringify($1, null, $2); })"
  encodePretty_ffi :: JSVal -> Int -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
  "return JSON.stringify($1, null, $2);"
  encodePretty_ffi :: JSVal -> Int -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
encodePretty' :: ToJSON a => Config -> a -> MisoString
encodePretty' = error "encodePretty': not implemented"
-----------------------------------------------------------------------------
encodePretty :: ToJSON a => a -> MisoString
encodePretty _ = error "encodePretty: not implemented"
-----------------------------------------------------------------------------
#else
-----------------------------------------------------------------------------
encodePretty' :: ToJSON a => Config -> a -> MisoString
encodePretty' (Config s) x = unsafePerformIO (flip encodePretty_ffi s =<< toJSVal_Value (toJSON x))
-----------------------------------------------------------------------------
encodePretty :: ToJSON a => a -> MisoString
encodePretty = encodePretty' defConfig
#endif
-----------------------------------------------------------------------------
newtype Config
  = Config
  { spaces :: Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
defConfig :: Config
defConfig = Config 4
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = JSON.stringify($1)"
  jsonStringify :: JSVal -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return JSON.stringify($1); })"
  jsonStringify :: JSVal -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
  "return JSON.stringify($1);"
  jsonStringify :: JSVal -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
jsonStringify :: JSVal -> IO MisoString
jsonStringify _ = error "jsonStringify: not implemented"
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = JSON.parse($1)"
  jsonParse :: MisoString -> IO JSVal
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return JSON.parse($1); })"
  jsonParse :: MisoString -> IO JSVal
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
  "return JSON.parse($1);"
  jsonParse :: MisoString -> IO JSVal
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
jsonParse :: MisoString -> IO JSVal
jsonParse _ = error "jsonParse: not implemented"
#endif
-----------------------------------------------------------------------------
eitherDecode :: FromJSON a => MisoString -> Either MisoString a
eitherDecode string = unsafePerformIO $ do
  (jsonParse string >>= fromJSVal_Value) >>= \case
    Nothing ->
      pure $ Left ("eitherDecode: " <> string)
    Just result ->
      pure (case fromJSON result of
        Success x -> Right x
        Error err -> Left err)
----------------------------------------------------------------------------
fromJSON :: FromJSON a => Value -> Result a
fromJSON value =
  case parseEither parseJSON value of
    Left s -> Error s
    Right x -> Success x
----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = \case
  Null ->
    pure jsNull
  Bool bool_ ->
    Marshal.toJSVal bool_
  String string ->
    Marshal.toJSVal string
  Number double ->
    Marshal.toJSVal double
  Array arr ->
    toJSVal_List =<< mapM toJSVal_Value arr
  Object hms -> do
    o <- create_ffi
    forM_ (M.toList hms) $ \(k,v) -> do
      v' <- toJSVal_Value v
      setProp_ffi k v' o
    pure o
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value jsval_ = do
  typeof jsval_ >>= \case
    0 -> return (Just Null)
    1 -> Just . Number <$> Marshal.fromJSValUnchecked jsval_
    2 -> Just . String <$> Marshal.fromJSValUnchecked jsval_
    3 -> fromJSValUnchecked_Int jsval_ >>= \case
      0 -> pure $ Just (Bool False)
      1 -> pure $ Just (Bool True)
      _ -> pure Nothing
    4 -> do xs <- Marshal.fromJSValUnchecked jsval_
            values <- forM xs fromJSVal_Value
            pure (Array <$> sequence values)
    5 -> do keys <- Marshal.fromJSValUnchecked =<< listProps_ffi jsval_
            result <-
              runMaybeT $ forM keys $ \k -> do
                key <- MaybeT (Marshal.fromJSVal k)
                raw <- MaybeT $ Just <$> getProp_ffi key jsval_
                value <- MaybeT (fromJSVal_Value raw)
                pure (key, value)
            pure (toObject <$> result)
    _ -> error "fromJSVal_Value: Unknown JSON type"
  where
    toObject = Object . M.fromList
#endif
-----------------------------------------------------------------------------
#ifdef WASM
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value jsval = do
  typeof jsval >>= \case
    0 -> return (Just Null)
    1 -> Just . Number <$> fromJSValUnchecked_Double jsval
    2 -> pure $ Just $ String $ (JSString jsval)
    3 -> fromJSValUnchecked_Int jsval >>= \case
      0 -> pure $ Just (Bool False)
      1 -> pure $ Just (Bool True)
      _ -> pure Nothing
    4 -> do xs <- fromJSValUnchecked_List jsval
            values <- forM xs fromJSVal_Value
            pure (Array <$> sequence values)
    5 -> do keys <- fromJSValUnchecked_List =<< listProps_ffi jsval
            result <-
              runMaybeT $ forM keys $ \k -> do
                let key = JSString k
                raw <- MaybeT $ Just <$> getProp_ffi key jsval
                value <- MaybeT (fromJSVal_Value raw)
                pure (key, value)
            pure (toObject <$> result)
    _ -> error "fromJSVal_Value: Unknown JSON type"
  where
    toObject = Object . M.fromList
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
-----------------------------------------------------------------------------
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value = error "fromJSVal_Value: not implemented"
-----------------------------------------------------------------------------
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = error "toJSVal_Value: not implemented"
-----------------------------------------------------------------------------
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return globalThis.miso.typeOf($1); })"
  typeof :: JSVal -> IO Int
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
 "return globalThis.miso.typeOf($1);"
  typeof :: JSVal -> IO Int
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = globalThis.miso.typeOf($1);"
  typeof :: JSVal -> IO Int
#endif
-----------------------------------------------------------------------------
#ifdef WASM
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = \case
  Null ->
    pure jsNull
  Bool bool_ ->
    toJSVal_Bool bool_
  String string ->
    toJSVal_JSString string
  Number double ->
    toJSVal_Double double
  Array arr ->
    toJSVal_List =<< mapM toJSVal_Value arr
  Object hms -> do
    o <- create_ffi
    forM_ (M.toList hms) $ \(k,v) -> do
      v' <- toJSVal_Value v
      setProp_ffi k v' o
    pure o
#endif
-----------------------------------------------------------------------------
