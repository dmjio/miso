-----------------------------------------------------------------------------
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
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import           Data.Array.Byte (ByteArray(..))
import           Data.Text.Internal hiding (pack, empty, append)
import           GHC.Exts
import           GHC.IO
import           GHC.Wasm.Prim
import qualified Data.List as List
import qualified Data.Text as T
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
pack :: String -> JSString
pack = toJSString
-----------------------------------------------------------------------------
empty :: JSString
empty = mempty
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return String.fromCharCode($1) + $2;
  """ cons :: Char -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 + String.fromCharCode($2);
  """ snoc :: JSString -> Char -> JSString
-----------------------------------------------------------------------------
append :: JSString -> JSString -> JSString
append = mappend
-----------------------------------------------------------------------------
unsnoc :: JSString -> Maybe (JSString, Char) 
unsnoc s
  | 0 <- length s = Nothing
  | otherwise = Just (init s, last s)
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('last: empty string');
  return $1.slice(-1).charCodeAt();
  """ last :: JSString -> Char
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('init: empty string');
  return $1.slice(0,-1);
  """ init :: JSString -> JSString
-----------------------------------------------------------------------------
compareLength :: JSString -> Int -> Ordering
compareLength str = compare (length str)
-----------------------------------------------------------------------------
map :: (Char -> Char) -> JSString -> JSString
map f s =
  case uncons s of
    Nothing -> mempty
    Just (c, next) ->
      f c `cons` map f next
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  const sep = String.fromCharCode($1)
  if ($2.length === 0) return '';
  else if ($2.length === 1) return $2;
  else return $2.split('').join(sep);
  """ intersperse :: Char -> JSString -> JSString
-----------------------------------------------------------------------------
transpose :: [JSString] -> [JSString]
transpose = fmap toJSString . List.transpose . fmap fromJSString
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
foreign import javascript unsafe
  """
  return $3.replace($1,$2);
  """ replace :: JSString -> JSString -> JSString -> JSString
-----------------------------------------------------------------------------
toCaseFold :: JSString -> JSString
toCaseFold = textToJSString . T.toCaseFold . textFromJSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.toLowerCase();
  """ toLower :: JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.toUpperCase();
  """ toUpper :: JSString -> JSString
-----------------------------------------------------------------------------
toTitle :: JSString -> JSString
toTitle = textToJSString . T.toTitle . textFromJSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 <= $3.length) return $3;
  let paddings = $1 - $3.length;
  while (paddings > 0) {
    $3 += String.fromCharCode($2);
    paddings--;
  }
  return $3;
  """ justifyLeft :: Int -> Char -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 <= $3.length) return $3;
  let paddings = $1 - $3.length;
  while (paddings > 0) {
    $3 = String.fromCharCode($2) + $3;
    paddings--;
  }
  return $3;
  """ justifyRight :: Int -> Char -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 <= $3.length) return $3;
  let paddings = ($1 - $3.length) / 2;
  let left = Math.ceil(paddings);
  let right = Math.floor(paddings);
  while (left > 0) {
    $3 = String.fromCharCode($2) + $3;
    left--;
  }
  while (right > 0) {
    $3 += String.fromCharCode($2);
    right--;
  }
  return $3;
  """ center :: Int -> Char -> JSString -> JSString
-----------------------------------------------------------------------------
foldl :: (a -> Char -> a) -> a -> JSString -> a
foldl f x ys =
  case uncons ys of
    Nothing -> x
    Just (c, next) -> foldl f (f x c) next
-----------------------------------------------------------------------------
foldl1 :: (Char -> Char -> Char) -> JSString -> Char
foldl1 f xs =
  case uncons xs of
    Nothing -> error "foldl1: empty string"
    Just (c,next) ->
      foldl f c next
-----------------------------------------------------------------------------
foldr :: (Char -> a -> a) -> a -> JSString -> a
foldr f x ys =
  case uncons ys of
    Nothing -> x
    Just (c, next) ->
      f c (foldr f x next)
-----------------------------------------------------------------------------
foldr1 :: (Char -> Char -> Char) -> JSString -> Char
foldr1 f ys =
  case uncons ys of
    Nothing -> error "foldr1: empty string"
    Just (c, next)
      | length next == 0 -> c
      | otherwise -> f c (foldr1 f next)
-----------------------------------------------------------------------------
any :: (Char -> Bool) -> JSString -> Bool
any f str =
  case uncons str of
    Nothing -> False
    Just (c, next) ->
      f c || any f next
-----------------------------------------------------------------------------
all :: (Char -> Bool) -> JSString -> Bool
all f str =
  case uncons str of
    Nothing -> True
    Just (c, next) ->
      f c && all f next
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('maximum: empty string');

  let max = $1[0].charCodeAt();
  for (let i = 0; i < $1.length; i++) {
    if (max < $1[i].charCodeAt()) {
      max = $1[i].charCodeAt();
    }
  }
  return max;
  """ maximum :: JSString -> Char
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('minimum: empty string');

  let min = $1[0].charCodeAt();
  for (let i = 0; i < $1.length; i++) {
    if ($1[i].charCodeAt() < min) {
      min = $1[i].charCodeAt();
    }
  }
  return min;
  """ minimum :: JSString -> Char
-----------------------------------------------------------------------------
scanl :: (Char -> Char -> Char) -> Char -> JSString -> JSString
scanl f x ys =
  case uncons ys of
    Nothing -> singleton x
    Just (c, next) ->
      x `cons` scanl f (f x c) next
-----------------------------------------------------------------------------
scanl1 :: (Char -> Char -> Char) -> JSString -> JSString
scanl1 f ys =
  case uncons ys of
    Nothing -> mempty
    Just (c, next) ->
      scanl f c next 
-----------------------------------------------------------------------------
scanr :: (Char -> Char -> Char) -> Char -> JSString -> JSString
scanr f q0 ys = 
  case uncons ys of
    Nothing -> singleton q0
    Just (x, xs) ->
      case uncons (scanr f q0 xs) of
        Just (q, qss) -> do
          let qs = q `cons` qss
          f x q `cons` qs
        Nothing -> error "scanr: impossible" 
-----------------------------------------------------------------------------
scanr1 :: (Char -> Char -> Char) -> JSString -> JSString
scanr1 f ys = 
  case uncons ys of
    Nothing -> mempty
    Just (x, xs)
      | length xs == 0 -> singleton x
      | otherwise -> do
          case uncons (scanr1 f xs) of
            Just (q, qss) -> f x q `cons` (q `cons` qss)
            Nothing -> error "scanr: impossible" 
-----------------------------------------------------------------------------
mapAccumL :: (a -> Char -> (a, Char)) -> a -> JSString -> (a, JSString)
mapAccumL f x str =
  case uncons str of
    Nothing -> (x, str)
    Just (c, next) -> do
      let (a, c') = f x c
      cons c' <$> mapAccumL f a next
-----------------------------------------------------------------------------
mapAccumR :: (a -> Char -> (a, Char)) -> a -> JSString -> (a, JSString)
mapAccumR f x str =
  case uncons str of
    Nothing -> (x, str)
    Just (c, next) ->
      case mapAccumR f x next of
        (a, qs) ->
          case f a c of
            (a', k) -> (a', k `cons` qs)
-----------------------------------------------------------------------------
unfoldr :: (a -> Maybe (Char, a)) -> a -> JSString
unfoldr f x = do
  case f x of
    Nothing -> mempty
    Just (c, y) ->
      c `cons` unfoldr f y
-----------------------------------------------------------------------------
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> JSString
unfoldrN n f seed = go seed mempty
  where
    go x acc
      | length acc == n = acc
      | otherwise =
          case f x of
            Nothing -> mempty
            Just (c,y) -> go y (c `cons` acc)
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 < 1) return "";
  return $2.slice(0, $1);
  """ take :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 < 1) return "";
  return $2.slice(-$1);
  """ takeEnd :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 < 1) return "";
  return $2.slice(0, -$1);
  """ dropEnd :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
takeWhile :: (Char -> Bool) -> JSString -> JSString
takeWhile f xs =
  case uncons xs of
    Nothing -> mempty
    Just (c, next) ->
      if f c
        then c `cons` takeWhile f next
        else mempty
-----------------------------------------------------------------------------
takeWhileEnd :: (Char -> Bool) -> JSString -> JSString
takeWhileEnd f = reverse . takeWhile f . reverse
-----------------------------------------------------------------------------
dropWhile :: (Char -> Bool) -> JSString -> JSString
dropWhile f xs =
  case uncons xs of
    Nothing -> xs
    Just (c, next) ->
      if f c
        then dropWhile f next
        else xs
-----------------------------------------------------------------------------
dropWhileEnd :: (Char -> Bool) -> JSString -> JSString
dropWhileEnd f = reverse . dropWhile f . reverse
-----------------------------------------------------------------------------
dropAround :: (Char -> Bool) -> JSString -> JSString
dropAround f = dropWhile f . dropWhileEnd f
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.trim();
  """ strip :: JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.trimStart();
  """ stripStart :: JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.trimEnd();
  """ stripEnd :: JSString -> JSString
-----------------------------------------------------------------------------
splitAt :: Int -> JSString -> (JSString, JSString)
splitAt n xs = (take n xs, drop n xs)
-----------------------------------------------------------------------------
breakOn :: JSString -> JSString -> (JSString, JSString)
breakOn n _ | 0 <- length n = error "breakOn: empty needle"
breakOn needle haystack = go (mempty, haystack)
  where
    go (acc, stack) =
      if needle `isPrefixOf` stack
        then (acc, stack)
        else
          case uncons stack of
            Nothing -> (acc, stack)
            Just (c,next) ->
              go (acc `snoc` c, next)
-----------------------------------------------------------------------------
breakOnEnd :: JSString -> JSString -> (JSString, JSString)
breakOnEnd n _ | 0 <- length n = error "breakOnEnd: empty needle"
breakOnEnd needle haystack = go (mempty, haystack)
  where
    go (acc, stack) =
      if needle `isSuffixOf` stack
        then (stack, acc)
        else
          case unsnoc stack of
            Nothing -> (stack, acc)
            Just (next, c) ->
              go (c `cons` acc, next)
-----------------------------------------------------------------------------
break :: (Char -> Bool) -> JSString -> (JSString, JSString)
break f s = go (mempty, s)
  where
    go (failed, rest) =
      case uncons rest of
        Nothing -> (failed, rest)
        Just (c,next) ->
          if f c
            then (failed, c `cons` next)
            else go (failed `snoc` c, next)
-----------------------------------------------------------------------------
span :: (Char -> Bool) -> JSString -> (JSString, JSString)
span f s = (takeWhile f s, dropWhile f s)
-----------------------------------------------------------------------------
group :: JSString -> [JSString]
group = groupBy (==)
-----------------------------------------------------------------------------
groupBy :: (Char -> Char -> Bool) -> JSString -> [JSString]
groupBy eq s' =
  case uncons s' of
    Nothing -> []
    Just (c, next) -> do
      let (ys, zs) = span (eq c) next
      (c `cons` ys) : groupBy eq zs
-----------------------------------------------------------------------------
inits :: JSString -> [JSString]
inits s = 
  case unsnoc s of
    Nothing -> [""]
    Just (next, _) -> inits next <> [s]
-----------------------------------------------------------------------------
tails :: JSString -> [JSString]
tails s =
  case uncons s of
    Nothing -> [""]
    Just (_, next) -> s : tails next
-----------------------------------------------------------------------------
splitOn :: JSString -> JSString -> [JSString]
splitOn prefix _ | 0 <- length prefix = error "splitOn: empty prefix"
splitOn prefix str = go str mempty
  where
    go s acc | 0 <- length s = [acc]
    go s acc = do
      if prefix `isPrefixOf` s
        then acc : go (drop (length prefix) s) mempty
        else
          case uncons s of
            Nothing -> [acc]
            Just (c,next) ->
              go next (acc `snoc` c)
-----------------------------------------------------------------------------
split :: (Char -> Bool) -> JSString -> [JSString]
split f = go
  where
    go str | 0 <- length str = [empty]
    go str = do
      let
        found = takeWhile (not . f) str
        next = drop 1 (dropWhile (not . f) str)
      found : go next
-----------------------------------------------------------------------------
chunksOf :: Int -> JSString -> [JSString]
chunksOf 0 _ = []
chunksOf n s =
  case (take n s, drop n s) of
    (hd, tl) ->
      if length tl == 0
        then [hd]
        else hd : chunksOf n tl
-----------------------------------------------------------------------------
lines :: JSString -> [JSString]
lines = splitOn "\n"
-----------------------------------------------------------------------------
words :: JSString -> [JSString]
words s = go (strip s)
  where
    go xs | length xs == 0 = []
    go xs = do
      let next = dropWhile (==' ') xs
      let payload = takeWhile (/=' ') next
      payload : go (drop (length payload) next)
-----------------------------------------------------------------------------
unwords :: [JSString] -> JSString
unwords = intercalate " "
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
stripPrefix prefix string
  | not (prefix `isPrefixOf` string) = Nothing
  | otherwise = Just (drop (length prefix) string)
-----------------------------------------------------------------------------
stripSuffix :: JSString -> JSString -> Maybe JSString
stripSuffix suffix string
  | not (suffix `isSuffixOf` string) = Nothing
  | otherwise = Just (dropEnd (length suffix) string)
-----------------------------------------------------------------------------
commonPrefixes :: JSString -> JSString -> Maybe (JSString, JSString, JSString)
commonPrefixes ls rs | length ls == 0 || length rs == 0 = Nothing
commonPrefixes ls' rs' = go mempty ls' rs'
  where
    go acc ls rs =
      case (uncons ls, uncons rs) of
        (Just (l,lss), Just (r,rss)) ->
          if l == r
            then go (acc `snoc` l) lss rss
            else
              if null acc
                then Nothing
                else Just (acc, l `cons` lss, r `cons` rss)
        _ -> Nothing
-----------------------------------------------------------------------------
filter :: (Char -> Bool) -> JSString -> JSString
filter f xs =
  case uncons xs of
    Nothing -> mempty
    Just (c,next) ->
      if f c
        then c `cons` filter f next
        else filter f next
-----------------------------------------------------------------------------
-- breakOnAll :: JSString -> JSString -> [(JSString, JSString)]
-- breakOnAll = error "TODO: implement breakOnAll"
-----------------------------------------------------------------------------
find :: (Char -> Bool) -> JSString -> Maybe Char
find f xs = do
  (c,next) <- uncons xs
  if f c
    then pure c
    else find f next
-----------------------------------------------------------------------------
partition :: (Char -> Bool) -> JSString -> (JSString, JSString)
partition f xs = (filter f xs, filter (not . f) xs)
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('index: empty string')
  return $1[$2].charCodeAt();
  """ index :: JSString -> Int -> Char
-----------------------------------------------------------------------------
findIndex :: (Char -> Bool) -> JSString -> Maybe Int
findIndex f xs = go xs
  where
    len = length xs - 1
    go zs = do
      (next, ys) <- uncons zs
      if f next
        then pure (len - length ys)
        else go ys
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('count: empty string')
  return $2.split($1).length - 1;
  """ count :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
zip :: JSString -> JSString -> [(Char,Char)]
zip l r =
  case (uncons l, uncons r) of
    (Just (l',ls), Just (r',rs)) ->
      (l',r') : zip ls rs
    _ -> []
-----------------------------------------------------------------------------
zipWith :: (Char -> Char -> Char) -> JSString -> JSString -> JSString
zipWith f l r =
  case (uncons l, uncons r) of
    (Just (l', ls), Just (r', rs)) ->
      f l' r' `cons` zipWith f ls rs
    _ -> mempty
-----------------------------------------------------------------------------
newtype JSUint8Array = JSUint8Array JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "(new TextEncoder()).encode($1)"
  js_str_encode :: JSString -> IO JSUint8Array
-----------------------------------------------------------------------------
foreign import javascript unsafe "$1.byteLength"
  js_buf_len :: JSUint8Array -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe "(new Uint8Array(__exports.memory.buffer, $2, $1.byteLength)).set($1)"
  js_from_buf :: JSUint8Array -> Ptr a -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_to_str :: Ptr a -> Int -> IO JSString
-----------------------------------------------------------------------------
textFromJSString :: JSString -> Text
textFromJSString str = unsafeDupablePerformIO $ do
  buf <- js_str_encode str
  I# len# <- js_buf_len buf
  IO $ \s0 -> case newByteArray# len# s0 of
    (# s1, mba# #) -> case unIO (js_from_buf buf (Ptr (mutableByteArrayContents# mba#))) s1 of
      (# s2, _ #) -> case unIO (freeJSVal (coerce buf)) s2 of
        (# s3, _ #) -> case unsafeFreezeByteArray# mba# s3 of
          (# s4, ba# #) -> (# s4, Text (ByteArray ba#) 0 (I# len#) #)
-----------------------------------------------------------------------------
textToJSString :: Text -> JSString
textToJSString (Text (ByteArray ba#) (I# off#) (I# len#)) = unsafeDupablePerformIO $
  IO $ \s0 -> case newPinnedByteArray# len# s0 of
    (# s1, mba# #) -> case copyByteArray# ba# off# mba# 0# len# s1 of
      s2 -> keepAlive# mba# s2 $ unIO $ js_to_str (Ptr (mutableByteArrayContents# mba#)) $ I# len#
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length === 0
  """ null :: JSString -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 < 1 || $2.length === 0) return $2;
  return $2.slice($1, $2.length);
  """ drop :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
foldl' :: (a -> Char -> a) -> a -> JSString -> a
foldl' f x ys =
  case uncons ys of
    Nothing -> x
    Just (c, next) -> do
      let !z = f x c
      foldl' f z next
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length
  """ length :: JSString -> Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $2.startsWith($1)
  """ isPrefixOf :: JSString -> JSString -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return String.fromCharCode($1);
  """ singleton :: Char -> JSString
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === 0) throw new Error ('head: empty string');
  return $1.slice(0).charCodeAt();
  """ head :: JSString -> Char
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.slice(1,$1.length)
  """ tail :: JSString -> JSString
-----------------------------------------------------------------------------
uncons :: JSString -> Maybe (Char, JSString)
uncons str
  | 0 <- length str = Nothing
  | otherwise = Just (head str, tail str)
-----------------------------------------------------------------------------
unpack :: JSString -> String
unpack = fromJSString
-----------------------------------------------------------------------------
intercalate :: JSString -> [JSString] -> JSString
intercalate sep = \case
  [] -> mempty
  [x] -> x
  (x:xs) -> x <> sep <> intercalate sep xs
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 === $2) return 0;
  else if ($1 > $2) return 1;
  else return -1;
  """ jsstringOrd :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
concat :: [JSString] -> JSString
concat = mconcat
-----------------------------------------------------------------------------
concatMap :: (Char -> JSString) -> JSString -> JSString
concatMap f str =
  case uncons str of
    Nothing -> mempty
    Just (c, next) -> f c <> concatMap f next
-----------------------------------------------------------------------------
unlines :: [JSString] -> JSString
unlines ks = concat [ snoc k '\n' | k <- ks ]
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 < 1) { return ''; }
  else if ($1 === 1) { return $2; }
  else {
    const inc = $2;
    while (--$1) {
      $2 += inc;
    }
    return $2;
  }
  """ replicate :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
-- | `ToJSON` for `JSString`
instance ToJSON JSString where
  toJSON = String . textFromJSString
----------------------------------------------------------------------------
-- | `FromJSON` for `JSString`
instance FromJSON JSString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (textToJSString x)
----------------------------------------------------------------------------
instance ToJSONKey JSString
----------------------------------------------------------------------------
instance FromJSONKey JSString
----------------------------------------------------------------------------
instance Ord JSString where
  compare s1 s2 =
    case jsstringOrd s1 s2 of
      0 -> EQ
      1 -> GT
      (-1) -> LT
      _ -> error "jsstringOrd: impossible"
----------------------------------------------------------------------------
instance Eq JSString where
  (==) = jsstringEq
----------------------------------------------------------------------------
instance Semigroup JSString where
  (<>) = jsstringMappend
----------------------------------------------------------------------------
instance Monoid JSString where
  mempty = jsstringMempty
----------------------------------------------------------------------------
instance Show JSString where
  show = show . fromJSString
----------------------------------------------------------------------------
instance IsString JSString where
  fromString = toJSString
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
