-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
module Data.JSString
  ( -- * Types
    JSString (..)
    -- * Creation and elimination
  , pack
  , unpack
  , singleton
  , empty
    -- * Basic interface
  , cons
  , snoc
  , append
  , uncons
  , unsnoc
  , head
  , last
  , tail
  , init
  , null
  , length
  , compareLength
    -- * Transformations
  , map
  , intercalate
  , intersperse
  , transpose
  , reverse
  , replace
    -- ** Case conversion
  , toCaseFold
  , toLower
  , toUpper
  , toTitle
    -- ** Justification
  , justifyLeft
  , justifyRight
  , center
    -- * Folds
  , foldl
  , foldl'
  , foldl1
  , foldr
  , foldr1
    -- ** Special folds
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum
    -- * Construction
    -- ** Scans
  , scanl
  , scanl1
  , scanr
  , scanr1
    -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
    -- ** Generation and unfolding
  , replicate
  , unfoldr
  , unfoldrN
    -- * Substrings
    -- ** Breaking strings
  , take
  , takeEnd
  , drop
  , dropEnd
  , takeWhile
  , takeWhileEnd
  , dropWhile
  , dropWhileEnd
  , dropAround
  , strip
  , stripStart
  , stripEnd
  , splitAt
  , breakOn
  , breakOnEnd
  , break
  , span
  , group
  , groupBy
  , inits
  , tails
  -- ** Breaking into many substrings
  , splitOn
  , split
  , chunksOf
  -- ** Breaking into lines and words
  , lines
  , words
  , unlines
  , unwords
  -- * Predicates
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
    -- ** View patterns
  , stripPrefix
  , stripSuffix
  , commonPrefixes
    -- * Searching
  , filter
  -- , breakOnAll
  , find
  , partition
    -- * Indexing
  , index
  , findIndex
  , count
    -- * Zipping
  , zip
  , zipWith
   -- * Misc
  , textFromJSString
  , textToJSString
  , toJSString
  , fromJSString
  , toString_Double
  , toString_Float
  , toString_Word
  , toString_Int
  ) where
-----------------------------------------------------------------------------
#ifdef __MHS__
-- MicroHs: textFromJSString / textToJSString come from GHC.Wasm.Prim
import           Data.Text (Text)
#else
import           Data.Array.Byte (ByteArray(..))
import           Data.Text.Internal hiding (pack, empty, append)
import           GHC.Exts
import           GHC.IO
#endif
#ifdef __MHS__
import           GHC.Wasm.Prim hiding (textToJSString)
import qualified GHC.Wasm.Prim as Prim
#else
import           GHC.Wasm.Prim
#endif
import qualified Data.List as List
import qualified Data.Text as T
#ifndef __MHS__
import           Data.Char (isSpace)
#endif
import           System.IO.Unsafe (unsafePerformIO)
import           Prelude
  hiding ( length, head, tail, filter, zip
         , zipWith, unlines, unwords, null
         , map, reverse, foldl', last, init
         , foldl, foldl1, foldr, foldr1, concat
         , concatMap, any, maximum, all, minimum
         , scanl, scanl1, scanr, scanr1, replicate
         , take, drop, takeWhile, dropWhile, splitAt
         , break, span, lines, words
         )
-----------------------------------------------------------------------------
-- Note [JSString performance]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A 'JSString' is a JavaScript string. Crossing the WASM / JS boundary costs
-- far more than handling a character, so nothing here walks a string one
-- character at a time over the FFI (a loop over 'uncons' makes three
-- crossings per character). Every function is one of:
--
-- * a single foreign call to native string methods ('indexOf', 'slice',
--   'split', 'repeat', ...), or a small loop written in JavaScript;
--
-- * for functions that take a Haskell function ('map', 'filter', the folds,
--   'takeWhile', 'groupBy', ...): one conversion to 'Data.Text.Text'
--   ('toText', a 'TextEncoder' pass and a copy), the work done in Haskell,
--   and either one conversion back or a native slice of the original string.
--
-- Semantics follow "Data.Text", which is what 'Miso.String.MisoString' is on
-- the server: positions and lengths count code points, and 'words', 'lines',
-- 'strip' and friends use 'Data.Char.isSpace'. Native search results
-- ('indexOf', 'lastIndexOf', 'search') are UTF-16 code unit offsets; they
-- only ever go straight back to 'js_sliceTo' / 'js_sliceFrom', so a slice
-- never splits a surrogate pair.
--
-- Two differences from 'Data.Text' remain, both deliberate:
--
-- * A JavaScript string can hold a lone surrogate; a 'Text' cannot (it
--   becomes U+FFFD). Functions that go through 'Text' see U+FFFD there (on
--   MicroHs, which reads code points directly, they see the surrogate), and
--   joining strings can pair a lone high and low surrogate into one
--   character. Ordinary text never has lone surrogates.
--
-- * 'tail' of the empty string is the empty string, as before, rather than
--   an error.
-----------------------------------------------------------------------------
toText :: JSString -> Text
{-# INLINE toText #-}
toText = textFromJSString
-----------------------------------------------------------------------------
fromText :: Text -> JSString
{-# INLINE fromText #-}
fromText = textToJSString
-----------------------------------------------------------------------------
viaText :: (Text -> Text) -> JSString -> JSString
{-# INLINE viaText #-}
viaText f = fromText . f . toText
-----------------------------------------------------------------------------
-- | Keeps a leading U+FEFF, which "GHC.Wasm.Prim"'s 'toJSString' drops (its
-- 'TextDecoder' strips one byte order mark): on GHC this goes through
-- 'textToJSString', on MicroHs it puts the mark back.
pack :: String -> JSString
{-# INLINE pack #-}
#ifdef __MHS__
pack s@('\xFEFF' : _) = js_bom <> toJSString s
pack s = toJSString s
#else
pack = textToJSString . T.pack
#endif
-----------------------------------------------------------------------------
empty :: JSString
{-# INLINE empty #-}
empty = mempty
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return ($1 < 0x10000 ? String.fromCharCode($1) : String.fromCodePoint($1)) + $2;
  """ cons :: Char -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 + ($2 < 0x10000 ? String.fromCharCode($2) : String.fromCodePoint($2));
  """ snoc :: JSString -> Char -> JSString
-----------------------------------------------------------------------------
append :: JSString -> JSString -> JSString
{-# INLINE append #-}
append = mappend
-----------------------------------------------------------------------------
unsnoc :: JSString -> Maybe (JSString, Char)
{-# INLINE unsnoc #-}
unsnoc s
  | null s = Nothing
  | otherwise = Just (init s, last s)
-----------------------------------------------------------------------------
last :: JSString -> Char
{-# INLINE last #-}
last s =
  case js_last s of
    c | c < 0 -> error "last: empty string"
      | otherwise -> toEnum c
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  const n = $1.length;
  if (n === 0) return -1;
  const c = $1.charCodeAt(n - 1);
  if (c >= 0xDC00 && c <= 0xDFFF && n > 1) {
    const h = $1.charCodeAt(n - 2);
    if (h >= 0xD800 && h <= 0xDBFF) return $1.codePointAt(n - 2);
  }
  return c;
  """ js_last :: JSString -> Int
-----------------------------------------------------------------------------
init :: JSString -> JSString
{-# INLINE init #-}
init s
  | null s = error "init: empty string"
  | otherwise = js_init s
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  const n = $1.length;
  const c = $1.charCodeAt(n - 1);
  if (c >= 0xDC00 && c <= 0xDFFF && n > 1) {
    const h = $1.charCodeAt(n - 2);
    if (h >= 0xD800 && h <= 0xDBFF) return $1.slice(0, -2);
  }
  return $1.slice(0, -1);
  """ js_init :: JSString -> JSString
-----------------------------------------------------------------------------
-- | Compares the number of code points with @n@: by the code unit count
-- when there are no surrogates, otherwise walking at most @n + 1@ code
-- points.
compareLength :: JSString -> Int -> Ordering
{-# INLINE compareLength #-}
compareLength str n =
  case js_compareLength str n of
    c | c < 0 -> LT
      | c > 0 -> GT
      | otherwise -> EQ
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($1)) return $1.length < $2 ? -1 : $1.length > $2 ? 1 : 0;
  let n = 0;
  for (let i = 0; i < $1.length && n <= $2; i++, n++) {
    const c = $1.charCodeAt(i);
    if (c >= 0xD800 && c <= 0xDBFF && i + 1 < $1.length) {
      const d = $1.charCodeAt(i + 1);
      if (d >= 0xDC00 && d <= 0xDFFF) i++;
    }
  }
  return n < $2 ? -1 : n > $2 ? 1 : 0;
  """ js_compareLength :: JSString -> Int -> Int
-----------------------------------------------------------------------------
map :: (Char -> Char) -> JSString -> JSString
{-# INLINE map #-}
#ifdef __MHS__
map f = pack . List.map f . chars
#else
map f = viaText (T.map f)
#endif
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  const sep = $1 < 0x10000 ? String.fromCharCode($1) : String.fromCodePoint($1);
  return ((globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($2) ? Array.from($2) : $2.split('')).join(sep);
  """ intersperse :: Char -> JSString -> JSString
-----------------------------------------------------------------------------
transpose :: [JSString] -> [JSString]
{-# INLINE transpose #-}
transpose = fmap fromText . T.transpose . fmap toText
-----------------------------------------------------------------------------
-- | Reverses a t'Miso.String.MisoString'
--
-- @
-- reverse "abc"
-- "cba"
-- @
foreign import javascript unsafe
  """
  return [...$1].reverse().join('');
  """ reverse :: JSString -> JSString
-----------------------------------------------------------------------------
-- | @replace needle replacement haystack@ replaces every non-overlapping
-- occurrence of @needle@, as 'Data.Text.replace'.
replace :: JSString -> JSString -> JSString -> JSString
{-# INLINE replace #-}
replace needle replacement haystack
  | null needle = error "replace: empty input"
  | otherwise = js_replace needle replacement haystack
-----------------------------------------------------------------------------
-- split / join rather than replaceAll: a string replacement passed to
-- replaceAll still expands $& and $1.
foreign import javascript unsafe
  """
  return $3.split($1).join($2);
  """ js_replace :: JSString -> JSString -> JSString -> JSString
-----------------------------------------------------------------------------
toCaseFold :: JSString -> JSString
{-# INLINE toCaseFold #-}
toCaseFold = viaText T.toCaseFold
-----------------------------------------------------------------------------
-- | As 'Data.Text.toLower', character by character: JavaScript's
-- @toLowerCase@ also applies the final sigma rule (a word-final capital
-- sigma becomes U+03C2), which 'Data.Text' does not.
foreign import javascript unsafe
  """
  if ($1.indexOf('\\u03A3') < 0) return $1.toLowerCase();
  return Array.from($1, c => c === '\\u03A3' ? '\\u03C3' : c.toLowerCase()).join('');
  """ toLower :: JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.toUpperCase();
  """ toUpper :: JSString -> JSString
-----------------------------------------------------------------------------
toTitle :: JSString -> JSString
{-# INLINE toTitle #-}
toTitle = viaText T.toTitle
-----------------------------------------------------------------------------
justifyLeft :: Int -> Char -> JSString -> JSString
{-# INLINE justifyLeft #-}
justifyLeft k c s
  | k <= n = s
  | otherwise = s <> js_repeatChar (k - n) c
  where
    n = length s
-----------------------------------------------------------------------------
justifyRight :: Int -> Char -> JSString -> JSString
{-# INLINE justifyRight #-}
justifyRight k c s
  | k <= n = s
  | otherwise = js_repeatChar (k - n) c <> s
  where
    n = length s
-----------------------------------------------------------------------------
center :: Int -> Char -> JSString -> JSString
{-# INLINE center #-}
center k c s
  | k <= n = s
  | otherwise = js_repeatChar l c <> s <> js_repeatChar r c
  where
    n = length s
    r = (k - n) `div` 2
    l = k - n - r
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return String.fromCodePoint($2).repeat($1);
  """ js_repeatChar :: Int -> Char -> JSString
-----------------------------------------------------------------------------
foldl :: (a -> Char -> a) -> a -> JSString -> a
{-# INLINE foldl #-}
#ifdef __MHS__
foldl f z = List.foldl f z . chars
#else
foldl f z = T.foldl f z . toText
#endif
-----------------------------------------------------------------------------
foldl' :: (a -> Char -> a) -> a -> JSString -> a
{-# INLINE foldl' #-}
#ifdef __MHS__
foldl' f z = List.foldl' f z . chars
#else
foldl' f z = T.foldl' f z . toText
#endif
-----------------------------------------------------------------------------
foldl1 :: (Char -> Char -> Char) -> JSString -> Char
{-# INLINE foldl1 #-}
#ifdef __MHS__
foldl1 f s
  | null s = error "foldl1: empty string"
  | otherwise = List.foldl1 f (chars s)
#else
foldl1 f s
  | null s = error "foldl1: empty string"
  | otherwise = T.foldl1 f (toText s)
#endif
-----------------------------------------------------------------------------
foldr :: (Char -> a -> a) -> a -> JSString -> a
{-# INLINE foldr #-}
#ifdef __MHS__
foldr f z = List.foldr f z . chars
#else
foldr f z = T.foldr f z . toText
#endif
-----------------------------------------------------------------------------
foldr1 :: (Char -> Char -> Char) -> JSString -> Char
{-# INLINE foldr1 #-}
#ifdef __MHS__
foldr1 f s
  | null s = error "foldr1: empty string"
  | otherwise = List.foldr1 f (chars s)
#else
foldr1 f s
  | null s = error "foldr1: empty string"
  | otherwise = T.foldr1 f (toText s)
#endif
-----------------------------------------------------------------------------
any :: (Char -> Bool) -> JSString -> Bool
{-# INLINE any #-}
#ifdef __MHS__
any f = List.any f . chars
#else
any f s =
  case probeStart s of
    (t, whole) -> T.any f t || (not whole && T.any f (toText s))
#endif
-----------------------------------------------------------------------------
all :: (Char -> Bool) -> JSString -> Bool
{-# INLINE all #-}
#ifdef __MHS__
all f = List.all f . chars
#else
all f s =
  case probeStart s of
    (t, whole) -> T.all f t && (whole || T.all f (toText s))
#endif
-----------------------------------------------------------------------------
maximum :: JSString -> Char
{-# INLINE maximum #-}
maximum s =
  case js_maximum s of
    c | c < 0 -> error "maximum: empty string"
      | otherwise -> toEnum c
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) return -1;
  let m = -1;
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($1)) {
    for (let i = 0; i < $1.length; i++) { const c = $1.charCodeAt(i); if (c > m) m = c; }
    return m;
  }
  for (let i = 0; i < $1.length;) {
    const c = $1.codePointAt(i);
    i += c > 0xFFFF ? 2 : 1;
    if (c > m) m = c;
  }
  return m;
  """ js_maximum :: JSString -> Int
-----------------------------------------------------------------------------
minimum :: JSString -> Char
{-# INLINE minimum #-}
minimum s =
  case js_minimum s of
    c | c < 0 -> error "minimum: empty string"
      | otherwise -> toEnum c
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) return -1;
  let m = 0x110000;
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($1)) {
    for (let i = 0; i < $1.length; i++) { const c = $1.charCodeAt(i); if (c < m) m = c; }
    return m;
  }
  for (let i = 0; i < $1.length;) {
    const c = $1.codePointAt(i);
    i += c > 0xFFFF ? 2 : 1;
    if (c < m) m = c;
  }
  return m;
  """ js_minimum :: JSString -> Int
-----------------------------------------------------------------------------
scanl :: (Char -> Char -> Char) -> Char -> JSString -> JSString
{-# INLINE scanl #-}
#ifdef __MHS__
scanl f z = pack . List.scanl f z . chars
#else
scanl f z = viaText (T.scanl f z)
#endif
-----------------------------------------------------------------------------
scanl1 :: (Char -> Char -> Char) -> JSString -> JSString
{-# INLINE scanl1 #-}
#ifdef __MHS__
scanl1 f = pack . List.scanl1 f . chars
#else
scanl1 f = viaText (T.scanl1 f)
#endif
-----------------------------------------------------------------------------
scanr :: (Char -> Char -> Char) -> Char -> JSString -> JSString
{-# INLINE scanr #-}
#ifdef __MHS__
scanr f z = pack . List.scanr f z . chars
#else
scanr f z = viaText (T.scanr f z)
#endif
-----------------------------------------------------------------------------
scanr1 :: (Char -> Char -> Char) -> JSString -> JSString
{-# INLINE scanr1 #-}
#ifdef __MHS__
scanr1 f = pack . List.scanr1 f . chars
#else
scanr1 f = viaText (T.scanr1 f)
#endif
-----------------------------------------------------------------------------
mapAccumL :: (a -> Char -> (a, Char)) -> a -> JSString -> (a, JSString)
{-# INLINE mapAccumL #-}
#ifdef __MHS__
mapAccumL f z = fmap pack . List.mapAccumL f z . chars
#else
mapAccumL f z = fmap fromText . T.mapAccumL f z . toText
#endif
-----------------------------------------------------------------------------
mapAccumR :: (a -> Char -> (a, Char)) -> a -> JSString -> (a, JSString)
{-# INLINE mapAccumR #-}
#ifdef __MHS__
mapAccumR f z = fmap pack . List.mapAccumR f z . chars
#else
mapAccumR f z = fmap fromText . T.mapAccumR f z . toText
#endif
-----------------------------------------------------------------------------
unfoldr :: (a -> Maybe (Char, a)) -> a -> JSString
{-# INLINE unfoldr #-}
#ifdef __MHS__
unfoldr f = pack . List.unfoldr f
#else
unfoldr f = fromText . T.unfoldr f
#endif
-----------------------------------------------------------------------------
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> JSString
{-# INLINE unfoldrN #-}
#ifdef __MHS__
unfoldrN n f = pack . List.take n . List.unfoldr f
#else
unfoldrN n f = fromText . T.unfoldrN n f
#endif
-----------------------------------------------------------------------------
-- | The first @n@ code points. Walks only as far as the cut, and does not
-- allocate an array of the whole string.
foreign import javascript unsafe
  """
  if ($1 < 1) return "";
  const p = $2.slice(0, $1);
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test(p)) return p;
  let i = 0, k = $1;
  while (k > 0 && i < $2.length) {
    const c = $2.charCodeAt(i++);
    if (c >= 0xD800 && c <= 0xDBFF && i < $2.length) {
      const d = $2.charCodeAt(i);
      if (d >= 0xDC00 && d <= 0xDFFF) i++;
    }
    k--;
  }
  return $2.slice(0, i);
  """ take :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
-- | The last @n@ code points.
foreign import javascript unsafe
  """
  if ($1 < 1) return "";
  const t = $2.slice(-$1);
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test(t)) return t;
  let i = $2.length, k = $1;
  while (k > 0 && i > 0) {
    const c = $2.charCodeAt(--i);
    if (c >= 0xDC00 && c <= 0xDFFF && i > 0) {
      const h = $2.charCodeAt(i - 1);
      if (h >= 0xD800 && h <= 0xDBFF) i--;
    }
    k--;
  }
  return $2.slice(i);
  """ takeEnd :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
-- | All but the last @n@ code points.
foreign import javascript unsafe
  """
  if ($1 < 1) return $2;
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($2.slice(-$1))) return $2.slice(0, Math.max(0, $2.length - $1));
  let i = $2.length, k = $1;
  while (k > 0 && i > 0) {
    const c = $2.charCodeAt(--i);
    if (c >= 0xDC00 && c <= 0xDFFF && i > 0) {
      const h = $2.charCodeAt(i - 1);
      if (h >= 0xD800 && h <= 0xDBFF) i--;
    }
    k--;
  }
  return $2.slice(0, i);
  """ dropEnd :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
takeWhile :: (Char -> Bool) -> JSString -> JSString
{-# INLINE takeWhile #-}
takeWhile f s = js_sliceTo (prefixUnits f s) s
-----------------------------------------------------------------------------
takeWhileEnd :: (Char -> Bool) -> JSString -> JSString
{-# INLINE takeWhileEnd #-}
takeWhileEnd f s = js_sliceFrom (js_codeUnits s - suffixUnits f s) s
-----------------------------------------------------------------------------
dropWhile :: (Char -> Bool) -> JSString -> JSString
{-# INLINE dropWhile #-}
dropWhile f s = js_sliceFrom (prefixUnits f s) s
-----------------------------------------------------------------------------
dropWhileEnd :: (Char -> Bool) -> JSString -> JSString
{-# INLINE dropWhileEnd #-}
dropWhileEnd f s = js_sliceTo (js_codeUnits s - suffixUnits f s) s
-----------------------------------------------------------------------------
dropAround :: (Char -> Bool) -> JSString -> JSString
{-# INLINE dropAround #-}
dropAround f s
  | front >= n = mempty
  | otherwise = js_slice front (n - suffixUnits f s) s
  where
    n = js_codeUnits s
    front = prefixUnits f s
-----------------------------------------------------------------------------
-- | As 'Data.Text.strip': removes 'Data.Char.isSpace' characters (not the
-- wider set JavaScript's @trim@ removes, which includes U+FEFF).
foreign import javascript unsafe
  """
  let i = 0, j = $1.length, c;
  while (i < j && (c = $1.charCodeAt(i), (c === 32 || (c >= 9 && c <= 13) || c === 0xA0 || c === 0x1680 || (c >= 0x2000 && c <= 0x200A) || c === 0x202F || c === 0x205F || c === 0x3000))) i++;
  while (j > i && (c = $1.charCodeAt(j - 1), (c === 32 || (c >= 9 && c <= 13) || c === 0xA0 || c === 0x1680 || (c >= 0x2000 && c <= 0x200A) || c === 0x202F || c === 0x205F || c === 0x3000))) j--;
  return $1.slice(i, j);
  """ strip :: JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  let i = 0, c;
  while (i < $1.length && (c = $1.charCodeAt(i), (c === 32 || (c >= 9 && c <= 13) || c === 0xA0 || c === 0x1680 || (c >= 0x2000 && c <= 0x200A) || c === 0x202F || c === 0x205F || c === 0x3000))) i++;
  return $1.slice(i);
  """ stripStart :: JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  let j = $1.length, c;
  while (j > 0 && (c = $1.charCodeAt(j - 1), (c === 32 || (c >= 9 && c <= 13) || c === 0xA0 || c === 0x1680 || (c >= 0x2000 && c <= 0x200A) || c === 0x202F || c === 0x205F || c === 0x3000))) j--;
  return $1.slice(0, j);
  """ stripEnd :: JSString -> JSString
-----------------------------------------------------------------------------
splitAt :: Int -> JSString -> (JSString, JSString)
{-# INLINE splitAt #-}
splitAt n xs = (take n xs, drop n xs)
-----------------------------------------------------------------------------
breakOn :: JSString -> JSString -> (JSString, JSString)
{-# INLINE breakOn #-}
breakOn needle haystack
  | null needle = error "breakOn: empty needle"
  | otherwise =
      case js_indexOf needle haystack of
        i | i < 0 -> (haystack, mempty)
          | otherwise -> (js_sliceTo i haystack, js_sliceFrom i haystack)
-----------------------------------------------------------------------------
breakOnEnd :: JSString -> JSString -> (JSString, JSString)
{-# INLINE breakOnEnd #-}
breakOnEnd needle haystack
  | null needle = error "breakOnEnd: empty needle"
  | otherwise =
      case js_lastIndexOf needle haystack of
        i | i < 0 -> (mempty, haystack)
          | otherwise ->
              let j = i + js_codeUnits needle
              in (js_sliceTo j haystack, js_sliceFrom j haystack)
-----------------------------------------------------------------------------
break :: (Char -> Bool) -> JSString -> (JSString, JSString)
{-# INLINE break #-}
break f s = (js_sliceTo i s, js_sliceFrom i s)
  where
    i = prefixUnits (not . f) s
-----------------------------------------------------------------------------
span :: (Char -> Bool) -> JSString -> (JSString, JSString)
{-# INLINE span #-}
span f s = (js_sliceTo i s, js_sliceFrom i s)
  where
    i = prefixUnits f s
-----------------------------------------------------------------------------
group :: JSString -> [JSString]
{-# INLINE group #-}
group = groupBy (==)
-----------------------------------------------------------------------------
groupBy :: (Char -> Char -> Bool) -> JSString -> [JSString]
{-# INLINE groupBy #-}
#ifdef __MHS__
groupBy eq s = go 0 (chars s)
  where
    go !_ [] = []
    go !off (c:cs) =
      case List.span (eq c) cs of
        (more, rest) ->
          let end = off + charWidth c + stringWidth more
          in js_slice off end s : go end rest
#else
groupBy eq s = slices s (fmap utf16Length (T.groupBy eq (toText s)))
#endif
-----------------------------------------------------------------------------
-- | Consecutive slices of a string, of the given widths in code units:
-- one native slice each, no decoding.
slices :: JSString -> [Int] -> [JSString]
slices s = go 0
  where
    go !_ [] = []
    go !off (w:ws) = js_slice off (off + w) s : go (off + w) ws
-----------------------------------------------------------------------------
#ifdef __MHS__
-- | The width of a 'Text' in UTF-16 code units.
utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> if c > '\xFFFF' then n + 2 else n + 1) 0
#else
-- | The width of a 'Text' in UTF-16 code units: its code points ('T.length'
-- is fast), plus one per astral character, counted as the UTF-8 bytes that
-- start a 4-byte sequence (0xF0 and up). A byte loop: decoding every
-- character with a fold costs far more.
utf16Length :: Text -> Int
utf16Length t@(Text (ByteArray ba#) (I# off#) (I# len#)) = T.length t + I# (astral off# 0#)
  where
    astral :: Int# -> Int# -> Int#
    astral i# acc#
      | isTrue# (i# >=# (off# +# len#)) = acc#
      | isTrue# (geWord# (word8ToWord# (indexWord8Array# ba# i#)) 0xF0##) = astral (i# +# 1#) (acc# +# 1#)
      | otherwise = astral (i# +# 1#) acc#
#endif
-----------------------------------------------------------------------------
charWidth :: Char -> Int
{-# INLINE charWidth #-}
charWidth c = if c > '\xFFFF' then 2 else 1
#ifdef __MHS__
-----------------------------------------------------------------------------
-- | The width of a 'String' in UTF-16 code units.
stringWidth :: String -> Int
stringWidth = List.foldl' (\n c -> n + charWidth c) 0
-----------------------------------------------------------------------------
-- MicroHs: functions that take a Haskell function read the string one code
-- point per call, lazily, instead of converting it to Text. On MicroHs that
-- conversion costs about as much per character as a call, and reading
-- lazily lets 'takeWhile', 'any', 'find' and friends stop early. (On GHC the
-- conversion is far cheaper than a call per character, so those go through
-- Text.)
-----------------------------------------------------------------------------
-- | The code points of a string, lazily, one call each.
chars :: JSString -> String
chars s = go 0
  where
    n = js_codeUnits s
    go !i
      | i >= n = []
      | otherwise =
          let c = js_codePointAt s i
          in toEnum c : go (i + if c > 0xFFFF then 2 else 1)
-----------------------------------------------------------------------------
-- | The code points of a string from the end, lazily.
charsFromEnd :: JSString -> String
charsFromEnd s = go (js_codeUnits s)
  where
    go !j
      | j <= 0 = []
      | otherwise =
          let c = js_codePointBefore s j
          in toEnum c : go (j - if c > 0xFFFF then 2 else 1)
-----------------------------------------------------------------------------
-- | Code units in the longest prefix whose characters satisfy @f@.
prefixUnits :: (Char -> Bool) -> JSString -> Int
prefixUnits f = go 0 . chars
  where
    go !k (c:cs) | f c = go (k + charWidth c) cs
    go k _ = k
-----------------------------------------------------------------------------
-- | Code units in the longest suffix whose characters satisfy @f@.
suffixUnits :: (Char -> Bool) -> JSString -> Int
suffixUnits f = go 0 . charsFromEnd
  where
    go !k (c:cs) | f c = go (k + charWidth c) cs
    go k _ = k
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.codePointAt($2);
  """ js_codePointAt :: JSString -> Int -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  const c = $1.charCodeAt($2 - 1);
  if (c >= 0xDC00 && c <= 0xDFFF && $2 > 1) {
    const h = $1.charCodeAt($2 - 2);
    if (h >= 0xD800 && h <= 0xDBFF) return $1.codePointAt($2 - 2);
  }
  return c;
  """ js_codePointBefore :: JSString -> Int -> Int
#else
-----------------------------------------------------------------------------
-- GHC: functions that take a Haskell function first convert only the first
-- (or last) 64 code units: if the answer is in there, that is all they read,
-- so a tokenizer taking short tokens off a long string stays linear.
-- Otherwise they convert the whole string once, which is the fastest way
-- through a long match. Probe boundaries never split a surrogate pair.
-----------------------------------------------------------------------------
probeUnits :: Int
probeUnits = 64
-----------------------------------------------------------------------------
-- | The first 'probeUnits' code units as Text, and whether that is all.
probeStart :: JSString -> (Text, Bool)
probeStart s = (toText (js_sliceTo e s), e >= js_codeUnits s)
  where
    e = js_pairSafeEnd s probeUnits
-----------------------------------------------------------------------------
-- | The last 'probeUnits' code units as Text, and whether that is all.
probeEnd :: JSString -> (Text, Bool)
probeEnd s = (toText (js_sliceFrom st s), st <= 0)
  where
    st = js_pairSafeStart s (js_codeUnits s - probeUnits)
-----------------------------------------------------------------------------
-- | Code units in the longest prefix whose characters satisfy @f@.
prefixUnits :: (Char -> Bool) -> JSString -> Int
-- INLINE so that the predicate is known where 'T.takeWhile' runs: called
-- through an unknown function it costs about as much as a call per character.
{-# INLINE prefixUnits #-}
prefixUnits f s =
  case probeStart s of
    (t, whole) ->
      case T.span f t of
        (yes, rest)
          | whole || not (T.null rest) -> utf16Length yes
          | otherwise -> js_codePointUnits s (T.length (T.takeWhile f (toText s)))
-----------------------------------------------------------------------------
-- | Code units in the longest suffix whose characters satisfy @f@.
suffixUnits :: (Char -> Bool) -> JSString -> Int
{-# INLINE suffixUnits #-}
suffixUnits f s =
  case probeEnd s of
    (t, whole) ->
      let yes = T.takeWhileEnd f t
      in if whole || T.length yes < T.length t
           then utf16Length yes
           else js_codePointUnitsEnd s (T.length (T.takeWhileEnd f (toText s)))
-----------------------------------------------------------------------------
-- | The code units taken by the first @k@ code points (as in 'take', whose
-- fast path this shares).
foreign import javascript unsafe
  """
  if ($2 < 1) return 0;
  const p = $1.slice(0, $2);
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test(p)) return p.length;
  let i = 0, k = $2;
  while (k > 0 && i < $1.length) {
    const c = $1.charCodeAt(i++);
    if (c >= 0xD800 && c <= 0xDBFF && i < $1.length) {
      const d = $1.charCodeAt(i);
      if (d >= 0xDC00 && d <= 0xDFFF) i++;
    }
    k--;
  }
  return i;
  """ js_codePointUnits :: JSString -> Int -> Int
-----------------------------------------------------------------------------
-- | The code units taken by the last @k@ code points.
foreign import javascript unsafe
  """
  if ($2 < 1) return 0;
  const t = $1.slice(-$2);
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test(t)) return t.length;
  let i = $1.length, k = $2;
  while (k > 0 && i > 0) {
    const c = $1.charCodeAt(--i);
    if (c >= 0xDC00 && c <= 0xDFFF && i > 0) {
      const h = $1.charCodeAt(i - 1);
      if (h >= 0xD800 && h <= 0xDBFF) i--;
    }
    k--;
  }
  return $1.length - i;
  """ js_codePointUnitsEnd :: JSString -> Int -> Int
-----------------------------------------------------------------------------
-- | An end offset, moved past a low surrogate so it does not split a pair.
foreign import javascript unsafe
  """
  let e = Math.min($2, $1.length);
  if (e > 0 && e < $1.length) {
    const h = $1.charCodeAt(e - 1), l = $1.charCodeAt(e);
    if (h >= 0xD800 && h <= 0xDBFF && l >= 0xDC00 && l <= 0xDFFF) e++;
  }
  return e;
  """ js_pairSafeEnd :: JSString -> Int -> Int
-----------------------------------------------------------------------------
-- | A start offset, moved back before a high surrogate so it does not split
-- a pair.
foreign import javascript unsafe
  """
  let st = Math.max($2, 0);
  if (st > 0 && st < $1.length) {
    const h = $1.charCodeAt(st - 1), l = $1.charCodeAt(st);
    if (h >= 0xD800 && h <= 0xDBFF && l >= 0xDC00 && l <= 0xDFFF) st--;
  }
  return st;
  """ js_pairSafeStart :: JSString -> Int -> Int
#endif
-----------------------------------------------------------------------------
inits :: JSString -> [JSString]
{-# INLINE inits #-}
inits = List.reverse . go
  where
    go s
      | null s = [s]
      | otherwise = s : go (init s)
-----------------------------------------------------------------------------
tails :: JSString -> [JSString]
{-# INLINE tails #-}
tails s
  | null s = [s]
  | otherwise = s : tails (tail s)
-----------------------------------------------------------------------------
splitOn :: JSString -> JSString -> [JSString]
{-# INLINE splitOn #-}
splitOn sep str
  | null sep = error "splitOn: empty prefix"
  | otherwise = go 0
  where
    n = js_codeUnits sep
    go off =
      case js_indexOfFrom sep str off of
        i | i < 0 -> [js_sliceFrom off str]
          | otherwise -> js_slice off i str : go (i + n)
-----------------------------------------------------------------------------
-- | As 'Data.Text.split': @split (== \'a\') \"b\" == [\"b\"]@.
split :: (Char -> Bool) -> JSString -> [JSString]
{-# INLINE split #-}
#ifdef __MHS__
-- MicroHs: walk the code points ('chars'); its Text copies the rest of the
-- string on every 'T.break'.
split f s = go 0 (chars s)
  where
    go !off cs =
      case List.break f cs of
        (piece, rest) ->
          let end = off + stringWidth piece
          in case rest of
               [] -> [js_sliceFrom off s]
               (sep : rest') -> js_slice off end s : go (end + charWidth sep) rest'
#else
split f s = go 0 (toText s)
  where
    go !off t =
      case T.break f t of
        (piece, rest) ->
          let end = off + utf16Length piece
          in case T.uncons rest of
               Nothing -> [js_sliceFrom off s]
               Just (sep, rest') -> js_slice off end s : go (end + charWidth sep) rest'
#endif
-----------------------------------------------------------------------------
-- | As 'Data.Text.chunksOf': @chunksOf k \"\" == []@, and so is any
-- @chunksOf k@ with @k <= 0@.
chunksOf :: Int -> JSString -> [JSString]
{-# INLINE chunksOf #-}
chunksOf k s
  | k <= 0 || null s = []
  | otherwise = take k s : chunksOf k (drop k s)
-----------------------------------------------------------------------------
-- | As 'Data.Text.lines': a trailing newline does not start an empty line,
-- and @lines \"\" == []@.
lines :: JSString -> [JSString]
{-# INLINE lines #-}
lines s = go 0
  where
    len = js_codeUnits s
    go off
      | off >= len = []
      | otherwise =
          case js_indexOfFrom newline s off of
            i | i < 0 -> [js_sliceFrom off s]
              | otherwise -> js_slice off i s : go (i + 1)
-----------------------------------------------------------------------------
newline :: JSString
{-# NOINLINE newline #-}
newline = "\n"
-----------------------------------------------------------------------------
-- | As 'Data.Text.words': splits on any 'Data.Char.isSpace' character
-- (tabs and newlines included), dropping empty words.
words :: JSString -> [JSString]
#ifdef __MHS__
-- MicroHs: one native split. Walking the Text would be quadratic (its 'T.span'
-- copies the rest of the string) and walking a String runs in Haskell.
words s = unsafePerformIO $ do
  arr <- js_words s
  n <- js_arrayLength arr
  ws <- mapM (js_arrayIndex arr) [0 .. n - 1]
  case arr of JSArray v -> freeJSVal v
  pure ws
{-# NOINLINE words #-}
#else
-- GHC: walk the Text ('T.span' and 'T.break' slice in O(1)) and slice the
-- original string, one call per word. Measured faster than the native split,
-- whose array reads cost more than the slices.
words s = go 0 (toText s)
  where
    go !off t =
      case T.span isSpace t of
        (spaces, rest)
          | T.null rest -> []
          | otherwise ->
              let start = off + utf16Length spaces
              in case T.break isSpace rest of
                   (word, rest') ->
                     let end = start + utf16Length word
                     in js_slice start end s : go end rest'
#endif
-----------------------------------------------------------------------------
-- One native split on exactly 'Data.Char.isSpace': ASCII whitespace, U+00A0
-- and the other Unicode space separators (JavaScript's own \s also matches
-- U+2028, U+2029 and U+FEFF).
#ifdef __MHS__
foreign import javascript unsafe
  """
  const ws = $1.split(/[\\t\\n\\v\\f\\r \\u00a0\\u1680\\u2000-\\u200a\\u202f\\u205f\\u3000]+/);
  if (ws.length > 0 && ws[0] === '') ws.shift();
  if (ws.length > 0 && ws[ws.length - 1] === '') ws.pop();
  return ws;
  """ js_words :: JSString -> IO JSArray
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.length"
  js_arrayLength :: JSArray -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1[$2]"
  js_arrayIndex :: JSArray -> Int -> IO JSString
#endif
-----------------------------------------------------------------------------
unwords :: [JSString] -> JSString
{-# INLINE unwords #-}
unwords = joinWith " "
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.endsWith($1);
  """ isSuffixOf :: JSString -> JSString -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.includes($1);
  """ isInfixOf :: JSString -> JSString -> Bool
-----------------------------------------------------------------------------
stripPrefix :: JSString -> JSString -> Maybe JSString
{-# INLINE stripPrefix #-}
stripPrefix prefix string =
  case js_prefixCut prefix string of
    i | i < 0 -> Nothing
      | otherwise -> Just (js_sliceFrom i string)
-----------------------------------------------------------------------------
stripSuffix :: JSString -> JSString -> Maybe JSString
{-# INLINE stripSuffix #-}
stripSuffix suffix string =
  case js_suffixCut suffix string of
    i | i < 0 -> Nothing
      | otherwise -> Just (js_sliceTo i string)
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.startsWith($1) ? $1.length : -1;
  """ js_prefixCut :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.endsWith($1) ? $2.length - $1.length : -1;
  """ js_suffixCut :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
-- | As 'Data.Text.commonPrefixes': @Nothing@ only when there is no common
-- prefix, so @commonPrefixes \"foo\" \"foobar\" == Just (\"foo\", \"\", \"bar\")@.
commonPrefixes :: JSString -> JSString -> Maybe (JSString, JSString, JSString)
{-# INLINE commonPrefixes #-}
commonPrefixes ls rs =
  case js_commonPrefixLength ls rs of
    i | i <= 0 -> Nothing
      | otherwise -> Just (js_sliceTo i ls, js_sliceFrom i ls, js_sliceFrom i rs)
-----------------------------------------------------------------------------
-- In code units; never ends between the halves of a surrogate pair.
foreign import javascript unsafe
  """
  const n = Math.min($1.length, $2.length);
  let i = 0;
  while (i < n && $1.charCodeAt(i) === $2.charCodeAt(i)) i++;
  if (i > 0) {
    const h = $1.charCodeAt(i - 1);
    if (h >= 0xD800 && h <= 0xDBFF) {
      const a = $1.charCodeAt(i), b = $2.charCodeAt(i);
      if ((a >= 0xDC00 && a <= 0xDFFF) || (b >= 0xDC00 && b <= 0xDFFF)) i--;
    }
  }
  return i;
  """ js_commonPrefixLength :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
filter :: (Char -> Bool) -> JSString -> JSString
{-# INLINE filter #-}
#ifdef __MHS__
filter f = pack . List.filter f . chars
#else
filter f = viaText (T.filter f)
#endif
-----------------------------------------------------------------------------
-- breakOnAll :: JSString -> JSString -> [(JSString, JSString)]
-- breakOnAll = error "TODO: implement breakOnAll"
-----------------------------------------------------------------------------
find :: (Char -> Bool) -> JSString -> Maybe Char
{-# INLINE find #-}
#ifdef __MHS__
find f = List.find f . chars
#else
find f s =
  case probeStart s of
    (t, whole) ->
      case T.find f t of
        Just c -> Just c
        Nothing
          | whole -> Nothing
          | otherwise -> T.find f (toText s)
#endif
-----------------------------------------------------------------------------
partition :: (Char -> Bool) -> JSString -> (JSString, JSString)
{-# INLINE partition #-}
#ifdef __MHS__
partition f s =
  case List.partition f (chars s) of
    (yes, no) -> (pack yes, pack no)
#else
partition f s =
  case T.partition f (toText s) of
    (yes, no) -> (fromText yes, fromText no)
#endif
-----------------------------------------------------------------------------
-- | The code point at an index, as 'Data.Text.index'.
index :: JSString -> Int -> Char
{-# INLINE index #-}
index s i
  | i < 0 = error "index: negative index"
  | otherwise =
      case js_index s i of
        c | c < 0 -> error "index: index too large"
          | otherwise -> toEnum c
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($2 < $1.length && !(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($1.slice(0, $2 + 1))) return $1.charCodeAt($2);
  let k = $2;
  for (let i = 0; i < $1.length; k--) {
    const c = $1.codePointAt(i);
    if (k === 0) return c;
    i += c > 0xFFFF ? 2 : 1;
  }
  return -1;
  """ js_index :: JSString -> Int -> Int
-----------------------------------------------------------------------------
findIndex :: (Char -> Bool) -> JSString -> Maybe Int
{-# INLINE findIndex #-}
#ifdef __MHS__
findIndex f = List.findIndex f . chars
#else
findIndex f s =
  case probeStart s of
    (t, whole) ->
      case T.findIndex f t of
        Just i -> Just i
        Nothing
          | whole -> Nothing
          | otherwise -> T.findIndex f (toText s)
#endif
-----------------------------------------------------------------------------
count :: JSString -> JSString -> Int
{-# INLINE count #-}
count needle haystack
  | null needle = error "count: empty input"
  | otherwise = js_count needle haystack
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.split($1).length - 1;
  """ js_count :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
zip :: JSString -> JSString -> [(Char,Char)]
{-# INLINE zip #-}
#ifdef __MHS__
zip l r = List.zip (chars l) (chars r)
#else
zip l r = T.zip (toText l) (toText r)
#endif
-----------------------------------------------------------------------------
zipWith :: (Char -> Char -> Char) -> JSString -> JSString -> JSString
{-# INLINE zipWith #-}
#ifdef __MHS__
zipWith f l r = pack (List.zipWith f (chars l) (chars r))
#else
zipWith f l r = fromText (T.zipWith f (toText l) (toText r))
#endif
-----------------------------------------------------------------------------
#ifdef __MHS__
-- | MicroHs's own conversion, keeping a leading U+FEFF (see 'pack').
textToJSString :: Text -> JSString
textToJSString t
  | T.singleton '\xFEFF' `T.isPrefixOf` t = js_bom <> Prim.textToJSString t
  | otherwise = Prim.textToJSString t
-----------------------------------------------------------------------------
foreign import javascript unsafe "'\\uFEFF'"
  js_bom :: JSString
#else
-- One encoder and one decoder for the life of the page, rather than one per
-- conversion. @ignoreBOM@ keeps a leading U+FEFF, which a default decoder
-- drops: 'Data.Text' keeps it.
-- Encodes straight into WASM memory: a code unit is at most 3 UTF-8 bytes.
foreign import javascript unsafe "(globalThis.__misoTextEncoder || (globalThis.__misoTextEncoder = new TextEncoder())).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encode_into :: JSString -> Ptr a -> Int -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe "(globalThis.__misoTextDecoder || (globalThis.__misoTextDecoder = new TextDecoder('utf-8', {fatal: true, ignoreBOM: true}))).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_to_str :: Ptr a -> Int -> IO JSString
-----------------------------------------------------------------------------
textFromJSString :: JSString -> Text
{-# INLINE textFromJSString #-}
textFromJSString str = unsafeDupablePerformIO $
  IO $ \s0 -> case newPinnedByteArray# cap# s0 of
    (# s1, scratch# #) -> case unIO (js_encode_into str (Ptr (mutableByteArrayContents# scratch#)) (I# cap#)) s1 of
      -- The scratch buffer is sized for the worst case (3 bytes per code
      -- unit); keep only what was written, in an unpinned array of exactly
      -- that size, and let the scratch buffer go.
      (# s2, I# len# #) -> case newByteArray# len# s2 of
        (# s3, mba# #) -> case copyMutableByteArray# scratch# 0# mba# 0# len# s3 of
          s4 -> case unsafeFreezeByteArray# mba# s4 of
            (# s5, ba# #) -> (# s5, Text (ByteArray ba#) 0 (I# len#) #)
  where
    !(I# cap#) = 3 * js_codeUnits str
-----------------------------------------------------------------------------
textToJSString :: Text -> JSString
{-# INLINE textToJSString #-}
textToJSString (Text (ByteArray ba#) (I# off#) (I# len#)) = unsafeDupablePerformIO $
  IO $ \s0 -> case newPinnedByteArray# len# s0 of
    (# s1, mba# #) -> case copyByteArray# ba# off# mba# 0# len# s1 of
      s2 -> keepAlive# mba# s2 $ unIO $ js_to_str (Ptr (mutableByteArrayContents# mba#)) $ I# len#
#endif
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length === 0
  """ null :: JSString -> Bool
-----------------------------------------------------------------------------
-- | All but the first @n@ code points. See 'take'.
foreign import javascript unsafe
  """
  if ($1 < 1 || $2.length === 0) return $2;
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($2.slice(0, $1))) return $2.slice($1);
  let i = 0, k = $1;
  while (k > 0 && i < $2.length) {
    const c = $2.charCodeAt(i++);
    if (c >= 0xD800 && c <= 0xDBFF && i < $2.length) {
      const d = $2.charCodeAt(i);
      if (d >= 0xDC00 && d <= 0xDFFF) i++;
    }
    k--;
  }
  return $2.slice(i);
  """ drop :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
-- | The number of code points. Use 'null' to test for the empty string: it
-- does not walk the string.
--
-- The surrogate tests here and in 'take', 'drop', 'takeEnd', 'dropEnd',
-- 'index' and 'compareLength' are fast paths: when there are no surrogates,
-- code points are code units. For a one-byte (Latin-1) string V8 answers
-- the test without scanning.
foreign import javascript unsafe
  """
  if (!(globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/)).test($1)) return $1.length;
  let n = 0;
  for (let i = 0; i < $1.length; i++, n++) {
    const c = $1.charCodeAt(i);
    if (c >= 0xD800 && c <= 0xDBFF && i + 1 < $1.length) {
      const d = $1.charCodeAt(i + 1);
      if (d >= 0xDC00 && d <= 0xDFFF) i++;
    }
  }
  return n;
  """ length :: JSString -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.startsWith($1)
  """ isPrefixOf :: JSString -> JSString -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 < 0x10000 ? String.fromCharCode($1) : String.fromCodePoint($1);
  """ singleton :: Char -> JSString
-----------------------------------------------------------------------------
-- | The first code point.
head :: JSString -> Char
{-# INLINE head #-}
head s =
  case js_head s of
    c | c < 0 -> error "head: empty string"
      | otherwise -> toEnum c
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length === 0 ? -1 : $1.codePointAt(0);
  """ js_head :: JSString -> Int
-----------------------------------------------------------------------------
-- | All but the first code point (@\"\"@ for @\"\"@).
foreign import javascript unsafe
  """
  if ($1.length > 1) {
    const c = $1.charCodeAt(0);
    if (c >= 0xD800 && c <= 0xDBFF) {
      const d = $1.charCodeAt(1);
      if (d >= 0xDC00 && d <= 0xDFFF) return $1.slice(2);
    }
  }
  return $1.slice(1);
  """ tail :: JSString -> JSString
-----------------------------------------------------------------------------
uncons :: JSString -> Maybe (Char, JSString)
{-# INLINE uncons #-}
uncons str
  | null str = Nothing
  | otherwise = Just (head str, tail str)
-----------------------------------------------------------------------------
-- Internal: native search and slicing, in UTF-16 code units. See
-- Note [JSString performance].
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.indexOf($1);
  """ js_indexOf :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.lastIndexOf($1);
  """ js_lastIndexOf :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.slice(0, $1);
  """ js_sliceTo :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.slice($1);
  """ js_sliceFrom :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length;
  """ js_codeUnits :: JSString -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.indexOf($1, $3);
  """ js_indexOfFrom :: JSString -> JSString -> Int -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $3.slice($1, $2);
  """ js_slice :: Int -> Int -> JSString -> JSString
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
unpack :: JSString -> String
{-# INLINE unpack #-}
unpack = fromJSString
-----------------------------------------------------------------------------
intercalate :: JSString -> [JSString] -> JSString
{-# INLINE intercalate #-}
intercalate = joinWith
-----------------------------------------------------------------------------
-- | Concatenates with a separator: pushes every piece onto one JavaScript
-- array and joins it once, rather than making a new string per '(<>)'.
joinWith :: JSString -> [JSString] -> JSString
joinWith _ [] = mempty
joinWith _ [x] = x
joinWith sep xs = unsafePerformIO $ do
  arr <- js_newArray
  mapM_ (js_push arr) xs
  r <- js_join arr sep
  case arr of JSArray v -> freeJSVal v
  pure r
{-# NOINLINE joinWith #-}
-----------------------------------------------------------------------------
newtype JSArray = JSArray JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "[]"
  js_newArray :: IO JSArray
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.push($2)"
  js_push :: JSArray -> JSString -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.join($2)"
  js_join :: JSArray -> JSString -> IO JSString
-----------------------------------------------------------------------------
-- | Code point order, as 'Data.Text' (and the server) compare. JavaScript's
-- @<@ compares UTF-16 code units, which puts an astral character (a
-- surrogate pair) before U+E000 to U+FFFF. Without surrogates the two
-- orders agree, so only then is there a loop: at the first difference,
-- surrogates are moved above U+E000 to U+FFFF.
foreign import javascript unsafe
  """
  if ($1 === $2) return 0;
  const sur = globalThis.__misoSurrogate || (globalThis.__misoSurrogate = /[\\uD800-\\uDFFF]/);
  if (!sur.test($1) && !sur.test($2)) return $1 < $2 ? -1 : 1;
  // a surrogate on one side only can still meet U+E000 to U+FFFF
  if (!/[\\uD800-\\uFFFF]/.test($1) || !/[\\uD800-\\uFFFF]/.test($2)) return $1 < $2 ? -1 : 1;
  const n = Math.min($1.length, $2.length);
  for (let i = 0; i < n; i++) {
    let a = $1.charCodeAt(i), b = $2.charCodeAt(i);
    if (a !== b) {
      if (a >= 0xD800 && b >= 0xD800) {
        a = a >= 0xE000 ? a - 0x800 : a + 0x2000;
        b = b >= 0xE000 ? b - 0x800 : b + 0x2000;
      }
      return a < b ? -1 : 1;
    }
  }
  return $1.length < $2.length ? -1 : 1;
  """ jsstringOrd :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
concat :: [JSString] -> JSString
{-# INLINE concat #-}
concat = joinWith mempty
-----------------------------------------------------------------------------
concatMap :: (Char -> JSString) -> JSString -> JSString
{-# INLINE concatMap #-}
#ifdef __MHS__
concatMap f = concat . List.map f . chars
#else
concatMap f = concat . List.map f . T.unpack . toText
#endif
-----------------------------------------------------------------------------
unlines :: [JSString] -> JSString
{-# INLINE unlines #-}
unlines [] = mempty
unlines ks = joinWith newline ks <> newline
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 < 1 ? '' : $2.repeat($1);
  """ replicate :: Int -> JSString -> JSString
----------------------------------------------------------------------------
instance Ord JSString where
  compare s1 s2 =
    case jsstringOrd s1 s2 of
      0 -> EQ
      1 -> GT
      (-1) -> LT
      _ -> error "jsstringOrd: impossible"
  {-# INLINE compare #-}
----------------------------------------------------------------------------
instance Eq JSString where
  (==) = jsstringEq
  {-# INLINE (==) #-}
----------------------------------------------------------------------------
instance Semigroup JSString where
  (<>) = jsstringMappend
  {-# INLINE (<>) #-}
----------------------------------------------------------------------------
instance Monoid JSString where
  mempty = jsstringMempty
  {-# INLINE mempty #-}
  mconcat = concat
  {-# INLINE mconcat #-}
----------------------------------------------------------------------------
instance Show JSString where
  show = show . fromJSString
  {-# INLINE show #-}
----------------------------------------------------------------------------
instance IsString JSString where
  fromString = pack
  {-# INLINE fromString #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $1 === $2"
  jsstringEq :: JSString -> JSString -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $1 + $2"
  jsstringMappend :: JSString -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe "return ''"
  jsstringMempty :: JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe "return ($1).toString()"
  toString_Int :: Int -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe "return ($1).toString()"
  toString_Double :: Double -> JSString
-----------------------------------------------------------------------------
-- Note: $1 arrives widened to a JS (f64) Number, so a plain `.toString()`
-- prints the full f64 expansion of the f32 value (e.g. "3.140000104904175"
-- for 3.14f) rather than the shortest decimal that round-trips through
-- float32. Search increasing precisions until re-parsing (narrowed back to
-- f32 via Math.fround) recovers the original value.
foreign import javascript unsafe
  """
  var x = $1;
  for (var p = 1; p <= 9; p++) {
    var s = x.toPrecision(p);
    if (Math.fround(parseFloat(s)) === x) return String(parseFloat(s));
  }
  return String(x);
  """
  toString_Float :: Float -> JSString
-----------------------------------------------------------------------------
toString_Word :: Word -> JSString
{-# INLINE toString_Word #-}
toString_Word = toJSString . show
-----------------------------------------------------------------------------
