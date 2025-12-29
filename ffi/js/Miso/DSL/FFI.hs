-----------------------------------------------------------------------------
module Miso.DSL.FFI where
-----------------------------------------------------------------------------
import Data.Aeson
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
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List = undefined
-----------------------------------------------------------------------------
toJSVal_isUndefined :: JSVal -> IO Bool
toJSVal_isUndefined = undefined
-----------------------------------------------------------------------------
toJSVal_isNull :: JSVal -> IO Bool
toJSVal_isNull = undefined
-----------------------------------------------------------------------------
jsNull :: IO JSVal
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
new_ffi           = undefined
eval_ffi          = undefined
create_ffi        = undefined
getProp_ffi       = undefined
setProp_ffi       = undefined
setField_ffi      = undefined
function_ffi      = undefined
fromJSVal_Int     = undefined
fromJSVal_Double  = undefined
getPropIndex_ffi  = undefined
asyncFunction_ffi = undefined
-----------------------------------------------------------------------------
