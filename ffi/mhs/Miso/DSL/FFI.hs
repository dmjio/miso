-----------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI         #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.DSL.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- FFI layer for MicroHs (mhs) compiled with the emscripten target.
--
-- This mirrors the WASM FFI layer (ffi/wasm), using the 'GHC.Wasm.Prim'
-- module of MicroHs, which provides 'JSVal', 'JSString', 'freeJSVal' and
-- the callback functions.  'MisoString' is 'Text' on this backend
-- (i.e., MISO_TEXT is always defined).
-----------------------------------------------------------------------------
module Miso.DSL.FFI
  ( -- ** Types
    JSVal
  , JSString (..)
  , now_ffi
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
  , await
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
  , freeJSVal_ffi
  , requestAnimationFrame
  , cancelAnimationFrame
  , listProps_ffi
  -- *** String FFI
  , parseInt
  , parseDouble
  , parseWord
  , parseFloat
  , toString_Int
  , toString_Double
  , toString_Float
  , toString_Word
  , textFromJSString
  , textToJSString
  , JSException
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
import           Data.Char (isSpace)
import qualified Data.List as L
import           Data.List (dropWhileEnd)
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric (readHex)
import           Prelude hiding (length)
import           Text.Read (readMaybe)
-----------------------------------------------------------------------------
import           GHC.Wasm.Prim
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1 === $2"
  eq :: JSVal -> JSVal -> Bool
-----------------------------------------------------------------------------
instance Eq JSVal where
  (==) = eq
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  toJSVal_Bool :: Bool -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  toJSVal_Double :: Double -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  toJSVal_Int :: Int -> IO JSVal
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List js = do
  arr <- newArray
  forM_ js (pushArray arr)
  pure arr
-----------------------------------------------------------------------------
foreign import javascript unsafe "[]"
  newArray :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.push($2)"
  pushArray :: JSVal -> JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  toJSVal_Char :: Char -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  toJSVal_Float :: Float -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  fromJSValUnchecked_Float :: JSVal -> IO Float
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  fromJSValUnchecked_Char :: JSVal -> IO Char
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
foreign import javascript unsafe "$1"
  jsstringFromJSVal :: JSVal -> IO JSString
-----------------------------------------------------------------------------
isNullOrUndefined :: JSVal -> Bool
isNullOrUndefined x = isNull_ffi x || isUndefined_ffi x
-----------------------------------------------------------------------------
isUndefined_ffi :: JSVal -> Bool
isUndefined_ffi = isUndefined
-----------------------------------------------------------------------------
isNull_ffi :: JSVal -> Bool
isNull_ffi = isNull
-----------------------------------------------------------------------------
foreign import javascript unsafe "globalThis"
  global :: JSVal
-----------------------------------------------------------------------------
-- | Awaits a JS Promise. If the promise rejects, it throws a t'JSException'.
foreign import javascript interruptible "await $1"
  await :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
-- The callbacks (asyncCallback, syncCallback, ...) come from GHC.Wasm.Prim.
-----------------------------------------------------------------------------
foreign import javascript unsafe "Object.keys($1)"
  listProps_ffi :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.apply($2, $3)"
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
foreign import javascript unsafe "$3[$1]=$2"
  setPropIndex_ffi
    :: Int
    -- ^ Index
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$3[$1]=$2"
  setProp_ffi
    :: JSString
    -- ^ Field
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "new $1(...$2)"
  new_ffi
    :: JSVal
    -- ^ Constructor
    -> JSVal
    -- ^ Args
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
foreign import javascript unsafe "({})"
  create_ffi :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$2[$1]"
  getProp_ffi
    :: JSString
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
-- | Unsafe JS eval, use at your own risk! You have been warned
foreign import javascript unsafe "eval($1)"
  eval_ffi :: JSString -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  fromJSValUnchecked_Int :: JSVal -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  fromJSValUnchecked_Double :: JSVal -> IO Double
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1"
  fromJSValUnchecked_Bool :: JSVal -> IO Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe "$2[$1]"
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
-- | Eagerly release a 'JSVal' handle. See 'Miso.DSL.freeJSVal'.
freeJSVal_ffi :: JSVal -> IO ()
freeJSVal_ffi = freeJSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "requestAnimationFrame($1)"
  requestAnimationFrame :: JSVal -> IO Int
-----------------------------------------------------------------------------
-- | High-resolution timestamp where one exists, wall clock where it does not.
foreign import javascript unsafe
  "(typeof performance !== 'undefined' && performance && typeof performance.now === 'function') ? performance.now() : Date.now()"
  now_ffi :: IO Double
-----------------------------------------------------------------------------
foreign import javascript unsafe "cancelAnimationFrame($1)"
  cancelAnimationFrame :: Int -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "Array.isArray($1)"
  isArray :: JSVal -> IO Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.length"
  length :: JSVal -> IO Int
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
-- | Parses like JS's @parseInt@: leading\/trailing whitespace and
-- trailing garbage are ignored, a leading @+\/-@ is allowed, and a
-- @0x@\/@0X@ prefix is read as hexadecimal.
parseInt :: Text -> Maybe Int
parseInt input = applySign <$> digits unsigned
  where
    stripped = dropWhileEnd isSpace (dropWhile isSpace (T.unpack input))
    (isNegative, unsigned) = case stripped of
      ('-' : rest) -> (True, rest)
      ('+' : rest) -> (False, rest)
      _            -> (False, stripped)
    applySign = if isNegative then negate else id
    digits ('0' : x : hex) | x == 'x' || x == 'X' =
      case readHex hex of
        ((n, _) : _) -> Just n
        _            -> Nothing
    digits ds =
      case span (`elem` ['0'..'9']) ds of
        ("", _) -> Nothing
        (ns, _) -> Just (read ns)
-----------------------------------------------------------------------------
parseWord :: Text -> Maybe Word
parseWord string = fromIntegral <$> parseInt string
-----------------------------------------------------------------------------
-- | Parses like JS's @parseFloat@: leading\/trailing whitespace and
-- trailing garbage are ignored, and a leading @+\/-@ is allowed.
parseDouble :: Text -> Maybe Double
parseDouble input =
  case stripped of
    ('+' : rest) -> go rest
    _            -> go stripped
  where
    stripped = dropWhile isSpace (T.unpack input)
    -- Try successively shorter prefixes, so trailing garbage is ignored.
    go s = firstJust [ readMaybe p | p <- reverse (prefixes s) ]
    prefixes s = [ take n s | n <- [1 .. L.length s] ]
    firstJust (Just x : _) = Just x
    firstJust (_ : xs) = firstJust xs
    firstJust [] = Nothing
-----------------------------------------------------------------------------
parseFloat :: Text -> Maybe Float
parseFloat string = realToFrac <$> parseDouble string
-----------------------------------------------------------------------------
toString_Int :: Int -> Text
toString_Int = T.pack . show
-----------------------------------------------------------------------------
toString_Double :: Double -> Text
toString_Double = T.pack . show
-----------------------------------------------------------------------------
toString_Float :: Float -> Text
toString_Float = T.pack . show
-----------------------------------------------------------------------------
toString_Word :: Word -> Text
toString_Word = T.pack . show
-----------------------------------------------------------------------------
