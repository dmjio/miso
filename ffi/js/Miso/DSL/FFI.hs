-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InterruptibleFFI  #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
module Miso.DSL.FFI
  ( -- ** Types
    JSVal
  , JSString
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
  , fromJSVal_JSString
  , fromJSVal_Maybe
  , fromJSValUnchecked_Maybe
  -- * Callback FFI
  , awaitPromise_ffi
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
  , listProps_ffi
  , requestAnimationFrame
  , cancelAnimationFrame
  -- *** String FFI
  , parseInt
  , parseDouble
  , parseWord
  , parseFloat
  , toString_Double
  , toString_Float
  , toString_Word
  , toString_Int
  , JSException
  ) where
-----------------------------------------------------------------------------
import           Data.JSString
import           Data.Text
import           Control.Exception (throwIO)
-----------------------------------------------------------------------------
import qualified GHCJS.Marshal as Marshal
import           GHCJS.Types
#ifdef GHCJS_NEW
import           GHC.JS.Prim
import qualified GHC.JS.Foreign.Callback as Callback
#elif GHCJS_OLD
import           GHCJS.Prim
import qualified GHCJS.Foreign.Callback as Callback
#endif
-----------------------------------------------------------------------------
foreign import javascript safe
#ifdef GHCJS_NEW
  "(($1,$2) => { return $1 === $2; })"
#else
  "$r = $1 === $2;"
#endif
  eq :: JSVal -> JSVal -> Bool
-----------------------------------------------------------------------------
instance Eq JSVal where
  (==) = eq
  {-# INLINE (==) #-}
-----------------------------------------------------------------------------
toJSVal_Bool :: Bool -> IO JSVal
toJSVal_Bool = Marshal.toJSVal
{-# INLINE toJSVal_Bool #-}
-----------------------------------------------------------------------------
toJSVal_Double :: Double -> IO JSVal
toJSVal_Double = Marshal.toJSVal
{-# INLINE toJSVal_Double #-}
-----------------------------------------------------------------------------
toJSVal_Int :: Int -> IO JSVal
toJSVal_Int = Marshal.toJSVal
{-# INLINE toJSVal_Int #-}
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List = Marshal.toJSVal
{-# INLINE toJSVal_List #-}
-----------------------------------------------------------------------------
fromJSVal_Bool :: JSVal -> IO (Maybe Bool)
fromJSVal_Bool = Marshal.fromJSVal
{-# INLINE fromJSVal_Bool #-}
-----------------------------------------------------------------------------
foreign import javascript safe
#ifdef GHCJS_NEW
  "(($1,$2) => { return new $1(...$2) })"
#else
  "$r = Reflect.construct($1, $2);"
#endif
  new_ffi :: JSVal -> JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return eval($1); })"
#else
  "$r = eval($1);"
#endif
  eval_ffi :: JSString -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(() => { return {}; })"
#else
  "$r = {};"
#endif
  create_ffi :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1,$2) => { return $2[$1]; })"
#else
  "$r=$2[$1]"
#endif
  getProp_ffi :: JSString -> JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1,$2,$3) => { return $3[$1]=$2; })"
#else
  "$3[$1]=$2"
#endif
  setProp_ffi
    :: JSString
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
fromJSVal_Int :: JSVal -> IO (Maybe Int)
fromJSVal_Int = Marshal.fromJSVal
{-# INLINE fromJSVal_Int #-}
-----------------------------------------------------------------------------
fromJSVal_Double :: JSVal -> IO (Maybe Double)
fromJSVal_Double  = Marshal.fromJSVal
{-# INLINE fromJSVal_Double #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1,$2) => { return $2[$1]; })"
#else
  "$r=$2[$1]"
#endif
  getPropIndex_ffi :: Int -> JSVal -> IO JSVal
-----------------------------------------------------------------------------
isNull_ffi :: JSVal -> Bool
isNull_ffi = isNull
{-# INLINE isNull_ffi #-}
-----------------------------------------------------------------------------
isUndefined_ffi :: JSVal -> Bool
isUndefined_ffi = isUndefined
{-# INLINE isUndefined_ffi #-}
-----------------------------------------------------------------------------
freeFunction_ffi :: JSVal -> IO ()
freeFunction_ffi _ = pure ()
{-# INLINE freeFunction_ffi #-}
-----------------------------------------------------------------------------
-- | No-op on GHCJS: 'JSVal's are ordinary JS references collected by the JS GC.
freeJSVal_ffi :: JSVal -> IO ()
freeJSVal_ffi _ = pure ()
{-# INLINE freeJSVal_ffi #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return requestAnimationFrame($1); })"
#else
  "$r = requestAnimationFrame($1);"
#endif
  requestAnimationFrame :: JSVal -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return cancelAnimationFrame($1); })"
#else
  "cancelAnimationFrame($1);"
#endif
  cancelAnimationFrame :: Int -> IO ()
-----------------------------------------------------------------------------
toJSVal_JSString :: JSString -> IO JSVal
toJSVal_JSString = Marshal.toJSVal
{-# INLINE toJSVal_JSString #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Maybe :: JSVal -> IO (Maybe JSVal)
fromJSValUnchecked_Maybe = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Maybe #-}
-----------------------------------------------------------------------------
fromJSVal_Maybe :: JSVal -> IO (Maybe (Maybe JSVal))
fromJSVal_Maybe = Marshal.fromJSVal
{-# INLINE fromJSVal_Maybe #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Bool :: JSVal -> IO Bool
fromJSValUnchecked_Bool = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Bool #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1,$2,$3) => { return $1.apply($2, $3); })"
#else
  "$r = $1.apply($2, $3);"
#endif
  invokeFunction :: JSVal -> JSVal -> JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return Object.keys($1); })"
#else
  "$r = Object.keys($1);"
#endif
  listProps_ffi :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1,$2,$3) => { return $3[$1]=$2; })"
#else
  "$3[$1]=$2"
#endif
  setPropIndex_ffi
    :: Int
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(() => { return globalThis; })"
#else
  "$r = globalThis"
#endif
  global :: JSVal
-----------------------------------------------------------------------------
fromJSVal_List :: JSVal -> IO (Maybe [JSVal])
fromJSVal_List = Marshal.fromJSVal
{-# INLINE fromJSVal_List #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Int :: JSVal -> IO Int
fromJSValUnchecked_Int = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Int #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Double :: JSVal -> IO Double
fromJSValUnchecked_Double = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Double #-}
-----------------------------------------------------------------------------
fromJSVal_JSString :: JSVal -> IO (Maybe JSString)
fromJSVal_JSString = Marshal.fromJSVal
{-# INLINE fromJSVal_JSString #-}
-----------------------------------------------------------------------------
toJSVal_Char :: Char -> IO JSVal
toJSVal_Char = Marshal.toJSVal
{-# INLINE toJSVal_Char #-}
-----------------------------------------------------------------------------
toJSVal_Float :: Float -> IO JSVal
toJSVal_Float = Marshal.toJSVal
{-# INLINE toJSVal_Float #-}
-----------------------------------------------------------------------------
toJSVal_Text :: Text -> IO JSVal
toJSVal_Text = Marshal.toJSVal
{-# INLINE toJSVal_Text #-}
-----------------------------------------------------------------------------
fromJSVal_Text :: JSVal -> IO (Maybe Text)
fromJSVal_Text = Marshal.fromJSVal
{-# INLINE fromJSVal_Text #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Text :: JSVal -> IO Text
fromJSValUnchecked_Text = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Text #-}
-----------------------------------------------------------------------------
fromJSVal_Char :: JSVal -> IO (Maybe Char)
fromJSVal_Char = Marshal.fromJSVal
{-# INLINE fromJSVal_Char #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Char :: JSVal -> IO Char
fromJSValUnchecked_Char = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Char #-}
-----------------------------------------------------------------------------
fromJSVal_Float :: JSVal -> IO (Maybe Float)
fromJSVal_Float = Marshal.fromJSVal
{-# INLINE fromJSVal_Float #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Float :: JSVal -> IO Float
fromJSValUnchecked_Float = Marshal.fromJSValUnchecked
{-# INLINE fromJSValUnchecked_Float #-}
-----------------------------------------------------------------------------
-- | Suspends the current Haskell thread and yields to the JS event loop
-- until the given JavaScript Promise resolves or rejects.
foreign import javascript interruptible
#if GHCJS_NEW
  "((promise, $c) => { promise.then(s => $c(null, s), e => $c(e, null)); })"
#else
  "$1.then(function(s) { $c(null, s); }, function(e) { $c(e, null); });"
#endif
  awaitPromise_ffi :: JSVal -> IO (JSVal, JSVal)

-- | Awaits a JS Promise. If the promise rejects, it throws a t'JSException',
-- mirroring the exact behavior of the GHC WASM backend's 'safe' imports.
--
-- @since 1.13.0.0
await :: JSVal -> IO JSVal
await promise = do
  (err, val) <- awaitPromise_ffi promise
  -- If the error argument is null/undefined, the promise resolved successfully
  if isNull_ffi err || isUndefined_ffi err
    then pure val
    else throwIO =<< mkJSException err
-----------------------------------------------------------------------------
asyncCallback :: IO () -> IO JSVal
asyncCallback x = jsval <$> Callback.asyncCallback x
{-# INLINE asyncCallback #-}
asyncCallback1 :: (JSVal -> IO ()) -> IO JSVal
asyncCallback1 x = jsval <$> Callback.asyncCallback1 x
{-# INLINE asyncCallback1 #-}
asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
asyncCallback2 x = jsval <$> Callback.asyncCallback2 x
{-# INLINE asyncCallback2 #-}
asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
asyncCallback3 x = jsval <$> Callback.asyncCallback3 x
{-# INLINE asyncCallback3 #-}
-----------------------------------------------------------------------------
syncCallback :: IO () -> IO JSVal
syncCallback x = jsval <$> Callback.syncCallback Callback.ThrowWouldBlock x
{-# INLINE syncCallback #-}
syncCallback1 :: (JSVal -> IO ()) -> IO JSVal
syncCallback1 x = jsval <$> Callback.syncCallback1 Callback.ThrowWouldBlock x
{-# INLINE syncCallback1 #-}
syncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
syncCallback2 x = jsval <$> Callback.syncCallback2 Callback.ThrowWouldBlock x
{-# INLINE syncCallback2 #-}
syncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
syncCallback3 x = jsval <$> Callback.syncCallback3 Callback.ThrowWouldBlock x
{-# INLINE syncCallback3 #-}
-----------------------------------------------------------------------------
syncCallback' :: IO JSVal -> IO JSVal
syncCallback' x = jsval <$> Callback.syncCallback' x
{-# INLINE syncCallback' #-}
syncCallback1' :: (JSVal -> IO JSVal) -> IO JSVal
syncCallback1' x = jsval <$> Callback.syncCallback1' x
{-# INLINE syncCallback1' #-}
syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal
syncCallback2' x = jsval <$> Callback.syncCallback2' x
{-# INLINE syncCallback2' #-}
syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO JSVal
syncCallback3' x = jsval <$> Callback.syncCallback3' x
{-# INLINE syncCallback3' #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return parseInt($1); })"
#else
  "$r = parseInt($1)"
#endif
  parseInt_Unchecked :: JSString -> Double
-----------------------------------------------------------------------------
parseWord :: JSString -> Maybe Word
parseWord string = fromIntegral <$> parseInt string
{-# INLINE parseWord #-}
-----------------------------------------------------------------------------
parseInt :: JSString -> Maybe Int
parseInt string =
  case parseInt_Unchecked string of
    double | isNaN double -> Nothing
           | otherwise -> Just (round double)
{-# INLINE parseInt #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return parseFloat($1); })"
#else
  "$r = parseFloat($1)"
#endif
  parseDouble_Unchecked :: JSString -> Double
-----------------------------------------------------------------------------
parseDouble :: JSString -> Maybe Double
parseDouble string =
  case parseDouble_Unchecked string of
    double | isNaN double -> Nothing
           | otherwise -> Just double
{-# INLINE parseDouble #-}
-----------------------------------------------------------------------------
parseFloat :: JSString -> Maybe Float
parseFloat string = realToFrac <$> parseDouble string
{-# INLINE parseFloat #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return ($1).toString(); })"
#else
  "$r = String($1);"
#endif
  toString_Int :: Int -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return ($1).toString(); })"
#else
  "$r = String($1);"
#endif
  toString_Double :: Double -> JSString
-----------------------------------------------------------------------------
-- Note: GHCJS narrows Float ops via Math.fround, so $1 already holds the
-- f64 expansion of the f32 value (e.g. 3.140000104904175 for 3.14f). A
-- plain `.toString()` would print that expansion; search increasing
-- precisions until re-parsing (narrowed back to f32) recovers the
-- original value, giving the shortest round-tripping decimal.
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { for (var p = 1; p <= 9; p++) { var s = $1.toPrecision(p); if (Math.fround(parseFloat(s)) === $1) return String(parseFloat(s)); } return String($1); })"
#else
  "var floatVal = $1; var floatStr; for (var floatPrec = 1; floatPrec <= 9; floatPrec++) { floatStr = floatVal.toPrecision(floatPrec); if (Math.fround(parseFloat(floatStr)) === floatVal) break; } $r = String(parseFloat(floatStr));"
#endif
  toString_Float :: Float -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
#if GHCJS_NEW
  "(($1) => { return ($1).toString(); })"
#else
  "$r = String($1);"
#endif
  toString_Word :: Word -> JSString
-----------------------------------------------------------------------------
-- | High-resolution timestamp where one exists, wall clock where it does not.
--
-- @performance@ is absent on Lynx's background-thread realm, so this cannot be
-- a bare @performance.now()@; see 'Miso.FFI.Internal.now'.
foreign import javascript unsafe
#if GHCJS_NEW
  "(() => (typeof performance !== 'undefined' && performance && typeof performance.now === 'function') ? performance.now() : Date.now())"
#else
  "$r = (typeof performance !== 'undefined' && performance && typeof performance.now === 'function') ? performance.now() : Date.now();"
#endif
  now_ffi :: IO Double
-----------------------------------------------------------------------------
