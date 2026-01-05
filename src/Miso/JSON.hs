-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , Parser, parseMaybe
  , ToJSON(toJSON)
  -- * Misc.
  , fromJSON
  , parseEither
  , eitherDecode
  -- * FFI
  , fromJSVal_Value
  , toJSVal_Value
  , jsonStringify
  , jsonParse
  ) where
----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
import qualified GHCJS.Marshal as Marshal
#endif
----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Int
import           Data.Word
import           Data.String
import           System.IO.Unsafe (unsafePerformIO)
----------------------------------------------------------------------------
import           Miso.DSL.FFI
----------------------------------------------------------------------------
#ifdef VANILLA
import Data.Text
type MisoString = Text
#else
import Control.Monad.Trans.Maybe
type MisoString = JSString
#endif
----------------------------------------------------------------------------
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
----------------------------------------------------------------------------
instance ToJSON () where
  toJSON () = Array []
----------------------------------------------------------------------------
instance ToJSON Value where
  toJSON = id
----------------------------------------------------------------------------
instance ToJSON Char where
  toJSON _ = undefined
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
newtype Parser a = P { unP :: Either MisoString a }
  deriving (Functor, Applicative, Monad)
----------------------------------------------------------------------------
instance Alternative Parser where
  empty = P (Left mempty)
  P (Left _) <|> r = r
  l <|> _ = l
----------------------------------------------------------------------------
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v =
  case parseEither m v of
    Left _ -> Nothing
    Right r -> Just r 
----------------------------------------------------------------------------
parseEither :: (a -> Parser b) -> a -> Either MisoString b
parseEither m v = unP (m v)
----------------------------------------------------------------------------
pfail :: MisoString -> Parser a
pfail message = P (Left message)
----------------------------------------------------------------------------
class FromJSON a where
  parseJSON :: Value -> Parser a
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
encode :: ToJSON a => a -> MisoString
encode x = unsafePerformIO $ jsonStringify =<< toJSVal_Value (toJSON x)
----------------------------------------------------------------------------
decode :: FromJSON a => MisoString -> Maybe a
decode s
  | Right x <- eitherDecode s = Just x
  | otherwise = Nothing
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
jsonStringify _ = undefined
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
jsonParse _ = undefined
#endif
-----------------------------------------------------------------------------
eitherDecode :: FromJSON a => MisoString -> Either MisoString a
eitherDecode string = unsafePerformIO $ do
  (jsonParse string >>= fromJSVal_Value) >>= \case
    Nothing ->
      pure $ Left ("eitherDecode: " <> string)
    Just result ->
      case fromJSON result of
        Success x -> pure (Right x)
        Error err -> pure (Left err)
----------------------------------------------------------------------------
data Value
  = Number Double
  | Bool Bool
  | String MisoString
  | Array [Value]
  | Object (Map MisoString Value)
  | Null
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance IsString Value where
  fromString = String . fromString
----------------------------------------------------------------------------
type Pair = (MisoString, Value)
----------------------------------------------------------------------------
type Object = Map MisoString Value
----------------------------------------------------------------------------
data Result a
  = Success a
  | Error MisoString
  deriving (Show, Eq)  
----------------------------------------------------------------------------
fromJSON :: FromJSON a => Value -> Result a
fromJSON value =
  case parseMaybe parseJSON value of
    Nothing -> Error ("No parse for: " <> encode value)
    Just x -> Success x
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
fromJSVal_Value = undefined
-----------------------------------------------------------------------------
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = undefined
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
