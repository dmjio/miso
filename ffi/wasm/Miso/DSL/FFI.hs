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
  , toJSVal_Bool
  , toJSVal_Double
  , toJSVal_Int
  , toJSVal_List
  , toJSVal_Value
  , toJSVal_JSString
    -- *** FromJSVal
  , fromJSVal_Bool
  , fromJSValUnchecked_Bool
  , fromJSVal_Double
  , fromJSValUnchecked_Double
  , fromJSVal_Int
  , fromJSValUnchecked_Int
  , fromJSVal_List
  , fromJSVal_Value
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
  , setField_ffi
  , setPropIndex_ffi
  , getPropIndex_ffi
  , create_ffi
    -- *** Misc. FFI
  , global
  , isUndefined_ffi
  , isNull_ffi
  , jsNull
  , jsUndefined
  , freeFunction_ffi
  , waitForAnimationFrame_ffi
  , listProps_ffi
  ) where
-----------------------------------------------------------------------------
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import           Data.Scientific
import           Control.Monad.Trans.Maybe
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.JSString (textFromJSString)
import           Prelude
  hiding (length, head, tail, unlines, concat, null, drop, replicate, concatMap)
import qualified Data.Vector as V
-----------------------------------------------------------------------------
import           GHC.Wasm.Prim
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
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
toJSVal_JSString :: JSString -> IO JSVal
toJSVal_JSString (JSString jsval) = pure jsval
-----------------------------------------------------------------------------
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = error "tojsval value"
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
foreign import javascript unsafe
  """
  return undefined;
  """ jsUndefined :: JSVal
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
foreign import javascript
  """
  return Object.keys($1);
  """
  listProps_ffi :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript
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
foreign import javascript "$2[$1]=$3"
  setPropIndex_ffi
    :: Int
    -- ^ Index
    -> JSVal
    -- ^ Object
    -> JSVal
    -- ^ Value
    -> IO ()
-----------------------------------------------------------------------------
foreign import javascript "$3[$1]=$2"
  setProp_ffi
    :: JSString
    -- ^ Field
    -> JSVal
    -- ^ Object
    -> JSVal
    -- ^ Value
    -> IO ()
-----------------------------------------------------------------------------
-- | Regular FFIs
-----------------------------------------------------------------------------
foreign import javascript unsafe "return new $1(...$2)"
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
  $1[$2] = $3;
  """ setField_ffi
      :: JSVal
      -- ^ Object to set
      -> JSString
      -- ^ Field name
      -> JSVal
      -- ^ Value to set
      -> IO ()
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
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value jsval = do
  typeof jsval >>= \case
    0 -> return (Just Null)
    1 -> Just . Number . fromFloatDigits <$> fromJSValUnchecked_Double jsval
    2 -> pure $ Just $ String $ textFromJSString (JSString jsval)
    3 -> Just . Bool <$> fromJSValUnchecked_Bool jsval
    4 -> do xs <- fromJSValUnchecked_List jsval
            values <- forM xs fromJSVal_Value
            pure (Array . V.fromList <$> sequence values)
    5 -> do keys <- fromJSValUnchecked_List =<< listProps_ffi jsval
            result <-
              runMaybeT $ forM keys $ \key -> do
                raw <- MaybeT $ Just <$> getProp_ffi (JSString key) jsval
                value <- MaybeT (fromJSVal_Value raw)
                let txt = textFromJSString (JSString key)
                pure (K.fromText txt, value)
            pure (toObject <$> result)
    _ -> error "fromJSVal_Value: Unknown JSON type"
  where
    toObject = Object . KM.fromList
-----------------------------------------------------------------------------
-- | Determine type for FromJSVal Value instance
--
-- 0. null
-- 1. number
-- 2. string
-- 3. bool
-- 4. array
-- 5. object
--
foreign import javascript unsafe
  """
  if ($1 === null || $1 === undefined) return 0;
  if (typeof($1) === 'number') return 1;
  if (typeof($1) === 'string') return 2;
  if (typeof($1) === 'boolean') return 3;
  if (Array.isArray($1)) return 4;
  return 5;
  """ typeof :: JSVal -> IO Int
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
-- | String FFIs
-----------------------------------------------------------------------------
-- instance IsString JSString where
--   fromString = toJSString
-- -----------------------------------------------------------------------------
-- instance Show JSString where
--   show = fromJSString
-----------------------------------------------------------------------------
freeFunction_ffi :: JSVal -> IO ()
freeFunction_ffi = freeJSVal
-----------------------------------------------------------------------------
waitForAnimationFrame_ffi :: IO Double
waitForAnimationFrame_ffi = do
  h <- makeHandle
  waitForFrame h `onException` cancelFrame h
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return { handle: null, callback: null };
  """ makeHandle :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  "(($1,$2) => { return $1.handle = requestAnimationFrame($2); })"
  """ waitForFrame :: JSVal -> IO Double
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
   if ($1.handle) cancelAnimationFrame($1.handle);
   if ($1.callback) { $1.callback = null; }
  """ cancelFrame :: JSVal -> IO ()
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
