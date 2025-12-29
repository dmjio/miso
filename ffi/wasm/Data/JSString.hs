-----------------------------------------------------------------------------
{-# LANGUAGE MultilineStrings  #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
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
  , breakOnAll
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
import GHC.Wasm.Prim (JSString(..), fromJSString, toJSString)
-----------------------------------------------------------------------------
import Data.Aeson
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Prelude
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
unsnoc s = do
  if length s == 0
    then Nothing
    else Just (init s, last s)
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
map f = textToJSString . T.map f . textFromJSString
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
transpose = fmap textToJSString . T.transpose . fmap textFromJSString
-----------------------------------------------------------------------------
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
justifyLeft :: Int -> Char -> JSString -> JSString
justifyLeft k n = textToJSString . T.justifyLeft k n . textFromJSString
-----------------------------------------------------------------------------
justifyRight :: Int -> Char -> JSString -> JSString
justifyRight k n = textToJSString . T.justifyRight k n . textFromJSString
-----------------------------------------------------------------------------
center :: Int -> Char -> JSString -> JSString
center k n = textToJSString . T.center k n . textFromJSString
-----------------------------------------------------------------------------
foldl :: (a -> Char -> a) -> a -> JSString -> a
foldl = undefined
-----------------------------------------------------------------------------
foldl1 :: (Char -> Char -> Char) -> JSString -> Char
foldl1 = undefined
-----------------------------------------------------------------------------
foldr :: (Char -> a -> a) -> a -> JSString -> a
foldr = undefined
-----------------------------------------------------------------------------
foldr1 :: (Char -> Char -> Char) -> JSString -> Char
foldr1 = undefined
-----------------------------------------------------------------------------
any :: (Char -> Bool) -> JSString -> Bool
any = undefined
-----------------------------------------------------------------------------
all :: (Char -> Bool) -> JSString -> Bool
all = undefined
-----------------------------------------------------------------------------
maximum :: a
maximum = undefined
-----------------------------------------------------------------------------
minimum :: a
minimum = undefined
-----------------------------------------------------------------------------
scanl :: a
scanl = undefined
-----------------------------------------------------------------------------
scanl1 :: a
scanl1 = undefined
-----------------------------------------------------------------------------
scanr :: a
scanr = undefined
-----------------------------------------------------------------------------
scanr1 :: a
scanr1 = undefined
-----------------------------------------------------------------------------
mapAccumL :: a
mapAccumL = undefined
-----------------------------------------------------------------------------
mapAccumR :: a
mapAccumR = undefined
-----------------------------------------------------------------------------
unfoldr :: a
unfoldr = undefined
-----------------------------------------------------------------------------
unfoldrN :: a
unfoldrN = undefined
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 < 1) return "";
  return $2.slice(0, $1);
  """ take :: Int -> JSString -> JSString
-----------------------------------------------------------------------------
takeEnd :: Int -> JSString -> JSString
takeEnd n = reverse . take n . reverse
-----------------------------------------------------------------------------
dropEnd :: Int -> JSString -> JSString
dropEnd n = reverse . drop n . reverse
-----------------------------------------------------------------------------
takeWhile :: a
takeWhile = undefined
-----------------------------------------------------------------------------
takeWhileEnd :: a
takeWhileEnd = undefined
-----------------------------------------------------------------------------
dropWhile :: a
dropWhile = undefined
-----------------------------------------------------------------------------
dropWhileEnd :: a
dropWhileEnd = undefined
-----------------------------------------------------------------------------
dropAround :: a
dropAround = undefined
-----------------------------------------------------------------------------
strip :: a
strip = undefined
-----------------------------------------------------------------------------
stripStart :: a
stripStart = undefined
-----------------------------------------------------------------------------
stripEnd :: a
stripEnd = undefined
-----------------------------------------------------------------------------
splitAt :: a
splitAt = undefined
-----------------------------------------------------------------------------
breakOn :: a
breakOn = undefined
-----------------------------------------------------------------------------
breakOnEnd :: a
breakOnEnd = undefined
-----------------------------------------------------------------------------
break :: a
break = undefined
-----------------------------------------------------------------------------
span :: a
span = undefined
-----------------------------------------------------------------------------
group :: a
group = undefined
-----------------------------------------------------------------------------l
groupBy :: a
groupBy = undefined
-----------------------------------------------------------------------------
inits :: a
inits = undefined
-----------------------------------------------------------------------------
tails :: a
tails = undefined
-----------------------------------------------------------------------------
splitOn :: a
splitOn = undefined
-----------------------------------------------------------------------------
split :: a
split = undefined
-----------------------------------------------------------------------------
chunksOf :: a
chunksOf = undefined
-----------------------------------------------------------------------------
lines :: a
lines = undefined
-----------------------------------------------------------------------------
words :: a
words = undefined
-----------------------------------------------------------------------------
unwords :: a
unwords = undefined
-----------------------------------------------------------------------------
isSuffixOf :: a
isSuffixOf = undefined
-----------------------------------------------------------------------------
isInfixOf :: a
isInfixOf = undefined
-----------------------------------------------------------------------------
stripPrefix :: a
stripPrefix = undefined
-----------------------------------------------------------------------------
stripSuffix :: a
stripSuffix = undefined
-----------------------------------------------------------------------------
commonPrefixes :: a
commonPrefixes = undefined
-----------------------------------------------------------------------------
filter :: a
filter = undefined
-----------------------------------------------------------------------------
breakOnAll :: a
breakOnAll = undefined
-----------------------------------------------------------------------------
find :: a
find = undefined
-----------------------------------------------------------------------------
partition :: a
partition = undefined
-----------------------------------------------------------------------------
index :: a
index = undefined
-----------------------------------------------------------------------------
findIndex :: a
findIndex = undefined
-----------------------------------------------------------------------------
count :: a
count = undefined
-----------------------------------------------------------------------------
zip :: a
zip = undefined
-----------------------------------------------------------------------------
zipWith :: a
zipWith = undefined
-----------------------------------------------------------------------------
textFromJSString :: JSString -> Text
textFromJSString = undefined
-----------------------------------------------------------------------------
textToJSString :: Text -> JSString
textToJSString = undefined
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length === 0
  """ null :: JSString -> Bool
-----------------------------------------------------------------------------
drop :: Int -> JSString -> JSString
drop = undefined
-----------------------------------------------------------------------------
foldl' :: (a -> Char -> a) -> a -> JSString -> a
foldl' = undefined
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length
  """ length :: JSString -> Int
-----------------------------------------------------------------------------
isPrefixOf :: JSString -> JSString -> Bool
isPrefixOf = undefined
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
intercalate = undefined
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1.length === $2.length) return 0;
  else if ($1.length > $2.length) return 1;
  else return -1;
  """ jsstringOrd :: JSString -> JSString -> Int
-----------------------------------------------------------------------------
concat :: [JSString] -> JSString
concat = undefined
-----------------------------------------------------------------------------
concatMap :: (Char -> JSString) -> JSString -> JSString
concatMap = undefined
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
  show = fromJSString
----------------------------------------------------------------------------
instance IsString JSString where
  fromString = toJSString
-----------------------------------------------------------------------------
foreign import javascript "return $1 === $2"
  jsstringEq :: JSString -> JSString -> Bool
-----------------------------------------------------------------------------
foreign import javascript "return $1 + $2"
  jsstringMappend :: JSString -> JSString -> JSString
-----------------------------------------------------------------------------
foreign import javascript "return ''"
  jsstringMempty :: JSString
-----------------------------------------------------------------------------
