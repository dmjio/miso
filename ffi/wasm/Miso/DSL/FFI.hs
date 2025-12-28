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
  , toJSVal_null
  , toJSVal_List
  , toJSVal_Value
    -- *** FromJSVal
  , fromJSVal_Bool
  , fromJSVal_Value
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
  ) where
-----------------------------------------------------------------------------
import Data.Aeson
import System.IO.Unsafe
import Prelude
  hiding (length, head, tail, unlines, concat, null, drop, replicate, concatMap)
-----------------------------------------------------------------------------
import GHC.Wasm.Prim
-----------------------------------------------------------------------------
toJSVal_Bool :: Bool -> IO JSVal
toJSVal_Bool = undefined
-----------------------------------------------------------------------------
toJSVal_Double :: Double -> IO JSVal
toJSVal_Double = undefined
-----------------------------------------------------------------------------
toJSVal_Int :: Int -> IO JSVal
toJSVal_Int = undefined
-----------------------------------------------------------------------------
toJSVal_null :: IO JSVal
toJSVal_null = undefined
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List = undefined
-----------------------------------------------------------------------------
isUndefined_ffi :: JSVal -> IO Bool
isUndefined_ffi = undefined
-----------------------------------------------------------------------------
isNull_ffi :: JSVal -> IO Bool
isNull_ffi = undefined
-----------------------------------------------------------------------------
jsNull :: JSVal
jsNull = undefined
-----------------------------------------------------------------------------
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = undefined
-----------------------------------------------------------------------------
fromJSVal_Bool :: JSVal -> IO (Maybe Bool)
fromJSVal_Bool = undefined
-----------------------------------------------------------------------------
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value = undefined
-----------------------------------------------------------------------------
foreign import javascript unsafe "return globalThis" globalThis :: IO JSVal 
-----------------------------------------------------------------------------
global :: JSVal
global = unsafePerformIO globalThis
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
foreign import javascript "return $1.apply($2, $3);"
  invokeFunction
    :: JSVal
    -- ^ Object
    -> JSString
    -- ^ Field name
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
foreign import javascript "$2[$1]=$3"
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
eval_ffi :: a
eval_ffi = undefined
-----------------------------------------------------------------------------
setField_ffi :: a
setField_ffi = undefined
-----------------------------------------------------------------------------
fromJSVal_Int :: a
fromJSVal_Int = undefined
-----------------------------------------------------------------------------
fromJSVal_Double :: a
fromJSVal_Double = undefined
-----------------------------------------------------------------------------
getPropIndex_ffi :: a
getPropIndex_ffi = undefined
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
