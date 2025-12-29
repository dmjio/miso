-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE MultilineStrings         #-}
{-# LANGUAGE ImportQualifiedPost      #-}
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
  , fromJSVal_List
  , fromJSVal_JSString
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
  , setPropIndex_ffi
  , setProp_ffi
  , new_ffi
  , getProp_ffi
  , eval_ffi
  , setField_ffi
  , fromJSVal_Int
  , fromJSVal_Double
  , getPropIndex_ffi
  , create_ffi
    -- *** Misc. FFI
  , global
  , isUndefined_ffi
  , isNull_ffi
  , jsNull
  , freeFunction_ffi
  , waitForAnimationFrame_ffi
  , listProps_ffi
  ) where
-----------------------------------------------------------------------------
import Control.Exception
import Control.Monad
import Data.Aeson
import Prelude
  hiding (length, head, tail, unlines, concat, null, drop, replicate, concatMap)
-----------------------------------------------------------------------------
import GHC.Wasm.Prim
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
    else Just <$> boolFromJSVal x
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $1" boolFromJSVal :: JSVal -> IO Bool
-----------------------------------------------------------------------------
fromJSVal_List :: JSVal -> IO (Maybe [JSVal])
fromJSVal_List arr = do
  arrayLike <- isArray arr
  if arrayLike
    then pure Nothing
    else do
      len <- length arr
      if len == 0
        then pure (Just [])
        else Just <$> do
          forM [ 0 .. len - 1 ] $ \idx ->
            getPropIndex_ffi idx arr
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
  """ fromJSVal_Int :: JSVal -> IO Int
-----------------------------------------------------------------------------
fromJSVal_Double :: JSVal -> IO (Maybe Double)
fromJSVal_Double = error "fromjsval double"
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
  console.log('making animation frame handle');
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
foreign import javascript unsafe
  """
  return typeof($1);
  """ typeof :: JSVal -> IO JSString
-----------------------------------------------------------------------------
