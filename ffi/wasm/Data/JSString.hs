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
  , unpack, unpack'
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
  , foldl1'
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
  , group'
  , groupBy
  , inits
  , tails
  -- ** Breaking into many substrings
  , splitOn, splitOn'
  , split
  , chunksOf, chunksOf'
  -- ** Breaking into lines and words
  , lines, lines'
  , words, words'
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
  , breakOnAll, breakOnAll'
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
pack :: a
pack = undefined
unpack' :: a
unpack' = undefined
empty :: a
empty = undefined
cons :: a
cons = undefined
snoc :: a
snoc = undefined
append :: a
append = undefined
unsnoc :: a
unsnoc = undefined
last :: a
last = undefined
init :: a
init = undefined
compareLength :: a
compareLength = undefined
map :: a
map = undefined
intersperse :: a
intersperse = undefined
transpose :: a
transpose = undefined
reverse :: a
reverse = undefined
replace :: a
replace = undefined
toCaseFold :: a
toCaseFold = undefined
toLower :: a
toLower = undefined
toUpper :: a
toUpper = undefined
toTitle :: a
toTitle = undefined
justifyLeft :: a
justifyLeft = undefined
justifyRight :: a
justifyRight = undefined
center :: a
center = undefined
foldl :: a
foldl = undefined
foldl1 :: a
foldl1 = undefined
foldl1' :: a
foldl1' = undefined
foldr :: a
foldr = undefined
foldr1 :: a
foldr1 = undefined
any :: a
any = undefined
all :: a
all = undefined
maximum :: a
maximum = undefined
minimum :: a
minimum = undefined
scanl :: a
scanl = undefined
scanl1 :: a
scanl1 = undefined
scanr :: a
scanr = undefined
scanr1 :: a
scanr1 = undefined
mapAccumL :: a
mapAccumL = undefined
mapAccumR :: a
mapAccumR = undefined
unfoldr :: a
unfoldr = undefined
unfoldrN :: a
unfoldrN = undefined
take :: a
take = undefined
takeEnd :: a
takeEnd = undefined
dropEnd :: a
dropEnd = undefined
takeWhile :: a
takeWhile = undefined
takeWhileEnd :: a
takeWhileEnd = undefined
dropWhile :: a
dropWhile = undefined
dropWhileEnd :: a
dropWhileEnd = undefined
dropAround :: a
dropAround = undefined
strip :: a
strip = undefined
stripStart :: a
stripStart = undefined
stripEnd :: a
stripEnd = undefined
splitAt :: a
splitAt = undefined
breakOn :: a
breakOn = undefined
breakOnEnd :: a
breakOnEnd = undefined
break :: a
break = undefined
span :: a
span = undefined
group :: a
group = undefined
group' :: a
group' = undefined
groupBy :: a
groupBy = undefined
inits :: a
inits = undefined
tails :: a
tails = undefined
splitOn :: a
splitOn = undefined
splitOn' :: a
splitOn' = undefined
split :: a
split = undefined
chunksOf :: a
chunksOf = undefined
chunksOf' :: a
chunksOf' = undefined
lines :: a
lines = undefined
lines' :: a
lines' = undefined
words :: a
words = undefined
words' :: a
words' = undefined
unwords :: a
unwords = undefined
isSuffixOf :: a
isSuffixOf = undefined
isInfixOf :: a
isInfixOf = undefined
stripPrefix :: a
stripPrefix = undefined
stripSuffix :: a
stripSuffix = undefined
commonPrefixes :: a
commonPrefixes = undefined
filter :: a
filter = undefined
breakOnAll :: a
breakOnAll = undefined
breakOnAll' :: a
breakOnAll' = undefined
find :: a
find = undefined
partition :: a
partition = undefined
index :: a
index = undefined
findIndex :: a
findIndex = undefined
count :: a
count = undefined
zip :: a
zip = undefined
zipWith :: a
zipWith = undefined
-----------------------------------------------------------------------------
textFromJSString :: JSString -> Text
textFromJSString = undefined
-----------------------------------------------------------------------------
textToJSString :: Text -> JSString
textToJSString = undefined
-----------------------------------------------------------------------------
null :: JSString -> Bool
null = undefined
-----------------------------------------------------------------------------
drop :: Int -> JSString -> JSString
drop = undefined
-----------------------------------------------------------------------------
foldl' :: (a -> Char -> a) -> a -> JSString -> a
foldl' = undefined
-----------------------------------------------------------------------------
length :: JSString -> Int
length = undefined
-----------------------------------------------------------------------------
isPrefixOf :: JSString -> JSString -> Bool
isPrefixOf = undefined
-----------------------------------------------------------------------------
singleton :: Char -> JSString
singleton = undefined
-----------------------------------------------------------------------------
head :: JSString -> Char
head = undefined
-----------------------------------------------------------------------------
tail :: JSString -> JSString
tail = undefined
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
unlines = undefined
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
