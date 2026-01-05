-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE MultilineStrings         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
module Miso.DSL.FFI
  ( -- ** Types
    JSVal
  , JSString (..)
    -- ** Serialization FFI
    -- *** ToJSVal
  , toJSVal_Char
  , toJSVal_Bool
  , toJSVal_Double
  , toJSVal_Float
  , toJSVal_Int
  , toJSVal_List
  , toJSVal_JSString
  , toJSVal_Text
    -- *** FromJSVal
  , fromJSVal_Text
  , fromJSValUnchecked_Text
  , fromJSVal_Char
  , fromJSValUnchecked_Char
  , fromJSVal_Bool
  , fromJSValUnchecked_Bool
  , fromJSVal_Double
  , fromJSValUnchecked_Double
  , fromJSVal_Float
  , fromJSValUnchecked_Float
  , fromJSVal_Int
  , fromJSValUnchecked_Int
  , fromJSVal_List
  , fromJSValUnchecked_List
  , fromJSVal_JSString
  , fromJSVal_Maybe
  , fromJSValUnchecked_Maybe
  -- * Callback FFI
  , asyncCallback
  , asyncCallback1
  , asyncCallback2
  , asyncCallback3
  , syncCallback
  , syncCallback1
  , syncCallback2
  , syncCallback3
  , syncCallback'
  , syncCallback1'
  , syncCallback2'
  , syncCallback3'
  -- * DSL FFI
  , invokeFunction
  , setProp_ffi
  , new_ffi
  , getProp_ffi
  , eval_ffi
  , setPropIndex_ffi
  , getPropIndex_ffi
  , create_ffi
    -- *** Misc. FFI
  , global
  , isUndefined_ffi
  , isNull_ffi
  , jsNull
  , freeFunction_ffi
  , requestAnimationFrame
  , cancelAnimationFrame
  , listProps_ffi
  -- *** String FFI
  , parseInt
  , parseDouble
  , parseWord
  , parseFloat
  ) where
-----------------------------------------------------------------------------
import           Data.Text (Text)
import           Control.Monad.Trans.Maybe
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.JSString (textFromJSString, textToJSString)
import           Prelude hiding (length, head, tail, unlines, concat, null, drop, replicate, concatMap)
-----------------------------------------------------------------------------
import           GHC.Wasm.Prim
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 === $2;
  """ eq :: JSVal -> JSVal -> Bool
-----------------------------------------------------------------------------
instance Eq JSVal where (==) = eq
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 === 0.0) return false;
  return true;
  """ toJSVal_Bool :: Bool -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ toJSVal_Double :: Double -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ toJSVal_Int :: Int -> IO JSVal
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List js = do
  arr <- newArray
  forM_ js (pushArray arr)
  pure arr
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return [];
  """ newArray :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  $1.push($2)
  """ pushArray :: JSVal -> JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """
  toJSVal_Char :: Char -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """
  toJSVal_Float :: Float -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Float :: JSVal -> IO Float
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Char :: JSVal -> IO Char
-----------------------------------------------------------------------------
fromJSVal_Char :: JSVal -> IO (Maybe Char)
fromJSVal_Char x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Char x
-----------------------------------------------------------------------------
toJSVal_JSString :: JSString -> IO JSVal
toJSVal_JSString (JSString jsval) = pure jsval
-----------------------------------------------------------------------------
fromJSVal_Text :: JSVal -> IO (Maybe Text)
fromJSVal_Text x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Text x
-----------------------------------------------------------------------------
fromJSValUnchecked_Text :: JSVal -> IO Text
fromJSValUnchecked_Text t =
  pure $ textFromJSString (JSString t)
-----------------------------------------------------------------------------
toJSVal_Text :: Text -> IO JSVal
toJSVal_Text t =
  case textToJSString t of
    JSString jsval -> pure jsval
-----------------------------------------------------------------------------
fromJSVal_Float :: JSVal -> IO (Maybe Float)
fromJSVal_Float x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Float x
-----------------------------------------------------------------------------
fromJSVal_Bool :: JSVal -> IO (Maybe Bool)
fromJSVal_Bool x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Bool x
-----------------------------------------------------------------------------
fromJSVal_Int :: JSVal -> IO (Maybe Int)
fromJSVal_Int x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Int x
-----------------------------------------------------------------------------
fromJSVal_Double :: JSVal -> IO (Maybe Double)
fromJSVal_Double x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Double x
-----------------------------------------------------------------------------
fromJSVal_List :: JSVal -> IO (Maybe [JSVal])
fromJSVal_List x = do
  if isNullOrUndefined x
    then pure Nothing
    else do
      arrayLike <- isArray x
      if not arrayLike
        then pure Nothing
        else Just <$> fromJSValUnchecked_List x
-----------------------------------------------------------------------------
fromJSValUnchecked_List :: JSVal -> IO [JSVal]
fromJSValUnchecked_List x = do
   len <- length x
   forM [ 0 .. len - 1 ] (flip getPropIndex_ffi x)
-----------------------------------------------------------------------------
fromJSVal_JSString :: JSVal -> IO (Maybe JSString)
fromJSVal_JSString x = do
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> jsstringFromJSVal x
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $1" jsstringFromJSVal :: JSVal -> IO JSString
-----------------------------------------------------------------------------
isNullOrUndefined :: JSVal -> Bool
isNullOrUndefined x = isNull_ffi x || isUndefined_ffi x
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 === undefined;
  """ isUndefined_ffi :: JSVal -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 === null;
  """ isNull_ffi :: JSVal -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return null;
  """ jsNull :: JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "return globalThis" global :: JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback
    :: IO ()
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback1
    :: (JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback2
    :: (JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback3
    :: (JSVal -> JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback
    :: IO ()
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback1
    :: (JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback2
    :: (JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback3
    :: (JSVal -> JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback'
    :: IO JSVal
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback1'
    :: (JSVal -> IO JSVal)
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback2'
    :: (JSVal -> JSVal -> IO JSVal)
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback3'
    :: (JSVal -> JSVal -> JSVal -> IO JSVal)
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return Object.keys($1);
  """
  listProps_ffi :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.apply($2, $3);
  """
  invokeFunction
    :: JSVal
    -- ^ Func
    -> JSVal
    -- ^ Obj
    -> JSVal
    -- ^ Args
    -> IO JSVal
    -- ^ Return value
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "$3[$1]=$2"
  setPropIndex_ffi
    :: Int
    -- ^ Index
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "$3[$1]=$2"
  setProp_ffi
    :: JSString
    -- ^ Field
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
-- | Regular FFIs
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "return new $1(...$2)"
  new_ffi
    :: JSVal
    -- ^ Constructor
    -> JSVal
    -- ^ Args
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
foreign import javascript unsafe "return {}" create_ffi :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $2[$1]"
  getProp_ffi
    :: JSString
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
-- | Unsafe JS eval, use at your own risk! You have been warned
foreign import javascript unsafe
  """
  return eval($1);
  """ eval_ffi :: JSString -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Int :: JSVal -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Double :: JSVal -> IO Double
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Bool :: JSVal -> IO Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $2[$1]"
  getPropIndex_ffi
    :: Int
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
freeFunction_ffi :: JSVal -> IO ()
freeFunction_ffi = freeJSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return requestAnimationFrame($1);
  """ requestAnimationFrame :: JSVal -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return cancelAnimationFrame($1);
  """ cancelAnimationFrame :: Int -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return Array.isArray($1);
  """ isArray :: JSVal -> IO Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length
  """ length :: JSVal -> IO Int
-----------------------------------------------------------------------------
fromJSVal_Maybe :: JSVal -> IO (Maybe (Maybe JSVal))
fromJSVal_Maybe jsval = do
  if isNullOrUndefined jsval
    then pure (Just Nothing)
    else pure $ Just (Just jsval)
-----------------------------------------------------------------------------
fromJSValUnchecked_Maybe :: JSVal -> IO (Maybe JSVal)
fromJSValUnchecked_Maybe jsval = do
  if isNullOrUndefined jsval
    then pure Nothing
    else pure (Just jsval)
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return parseInt($1);
  """
  parseInt_Unchecked :: JSString -> Double
-----------------------------------------------------------------------------
parseWord :: JSString -> Maybe Word
parseWord string = fromIntegral <$> parseInt string
-----------------------------------------------------------------------------
parseInt :: JSString -> Maybe Int
parseInt string = do
  case parseInt_Unchecked string of
    double | isNaN double -> Nothing
           | otherwise -> Just (round double)
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return parseFloat($1);
  """
  parseDouble_Unchecked :: JSString -> Double
-----------------------------------------------------------------------------
parseDouble :: JSString -> Maybe Double
parseDouble string = do
  case parseDouble_Unchecked string of
    double | isNaN double -> Nothing
           | otherwise -> Just double
-----------------------------------------------------------------------------
parseFloat :: JSString -> Maybe Float
parseFloat string = realToFrac <$> parseDouble string
-----------------------------------------------------------------------------
