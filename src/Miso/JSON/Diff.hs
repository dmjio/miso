-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON.Diff
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- A pure implementation of [JSON Patch (RFC 6902)](https://www.rfc-editor.org/rfc/rfc6902)
-- over 'Value'. Compute the difference between two documents with 'diffPatch',
-- and apply a patch with 'applyPatch'. Paths use
-- [JSON Pointer (RFC 6901)](https://www.rfc-editor.org/rfc/rfc6901).
--
-- No FFI — this works on every backend (browser\/WASM, native, SSR). A natural
-- use is shipping model /deltas/ across the Lynx dual-thread boundary:
--
-- @
-- let patch = 'diffPatch' ('toJSON' oldModel) ('toJSON' newModel)   -- on the BTS
-- -- ship ('toJSON' patch); on the MTS:
-- case 'applyPatch' patch mirroredValue of
--   Right v -> ...          -- v == toJSON newModel
--   Left  _ -> resync       -- fall back to a full model send
-- @
--
-- 'diffPatch' is /naive/: it emits @add@\/@remove@\/@replace@ (no @move@\/@copy@
-- detection and no minimal array edit script), but it is correct — applying the
-- result to the first document yields the second.
-----------------------------------------------------------------------------
module Miso.JSON.Diff
  ( -- * Types
    Pointer (..)
  , Op (..)
  , Patch (..)
    -- * Diff \/ apply
  , diffPatch
  , applyPatch
  , applyOp
    -- * JSON Pointer (RFC 6901)
  , renderPointer
  , parsePointer
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import           Miso.JSON.Types (Value(..))
import           Miso.String (MisoString, ms, fromMisoString)
-----------------------------------------------------------------------------
-- | A [JSON Pointer](https://www.rfc-editor.org/rfc/rfc6901): the decoded
-- (unescaped) path tokens. @Pointer []@ is the whole document.
newtype Pointer = Pointer [MisoString]
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | A single JSON Patch operation. For 'Move' and 'Copy' the first 'Pointer'
-- is the source (@from@) and the second is the destination (@path@).
data Op
  = Add     Pointer Value
  | Remove  Pointer
  | Replace Pointer Value
  | Move    Pointer Pointer
  | Copy    Pointer Pointer
  | Test    Pointer Value
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | An ordered list of operations, applied left-to-right.
newtype Patch = Patch [Op]
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Render a 'Pointer' to its RFC 6901 string form (e.g. @\"\/todos\/3\/done\"@).
renderPointer :: Pointer -> MisoString
renderPointer (Pointer toks) =
  ms (concatMap (\t -> '/' : escape (fromMisoString t)) toks)
  where
    escape :: String -> String
    escape = concatMap $ \case
      '~' -> "~0"
      '/' -> "~1"
      c   -> [c]
-----------------------------------------------------------------------------
-- | Parse an RFC 6901 pointer string into a 'Pointer'.
parsePointer :: MisoString -> Pointer
parsePointer s = case fromMisoString s :: String of
  ""        -> Pointer []
  '/' : str -> Pointer (map (ms . unescape) (splitSlash str))
  str       -> Pointer (map (ms . unescape) (splitSlash str))
  where
    unescape = \case
      '~' : '1' : cs -> '/' : unescape cs
      '~' : '0' : cs -> '~' : unescape cs
      c : cs         -> c : unescape cs
      []             -> []
    splitSlash str = case break (== '/') str of
      (a, [])    -> [a]
      (a, _ : b) -> a : splitSlash b
-----------------------------------------------------------------------------
-- | Apply a 'Patch' to a document, left-to-right. Returns 'Left' with a message
-- if any operation cannot be applied (e.g. a missing key or a failed @test@).
applyPatch :: Patch -> Value -> Either MisoString Value
applyPatch (Patch ops) doc = foldl (\acc op -> acc >>= applyOp op) (Right doc) ops
-----------------------------------------------------------------------------
-- | Apply a single operation.
applyOp :: Op -> Value -> Either MisoString Value
applyOp op doc = case op of
  Add (Pointer p) v     -> addAt p v doc
  Remove (Pointer p)    -> removeAt p doc
  Replace (Pointer p) v -> replaceAt p v doc
  Move (Pointer f) (Pointer p) -> do
    v    <- getAt f doc
    doc' <- removeAt f doc
    addAt p v doc'
  Copy (Pointer f) (Pointer p) -> do
    v <- getAt f doc
    addAt p v doc
  Test (Pointer p) v -> do
    v' <- getAt p doc
    if v' == v
      then Right doc
      else Left ("Miso.JSON.Diff: test failed at " <> renderPointer (Pointer p))
-----------------------------------------------------------------------------
getAt :: [MisoString] -> Value -> Either MisoString Value
getAt []       v = Right v
getAt (t : ts) v = case v of
  Object m -> maybe (Left ("missing key: " <> t)) (getAt ts) (M.lookup t m)
  Array xs -> parseIndex t >>= \i ->
    maybe (Left "array index out of range") (getAt ts) (xs `atMay` i)
  _        -> Left "cannot descend into scalar"
-----------------------------------------------------------------------------
addAt :: [MisoString] -> Value -> Value -> Either MisoString Value
addAt []  v _ = Right v
addAt [t] v container = case container of
  Object m -> Right (Object (M.insert t v m))
  Array xs
    | t == "-"  -> Right (Array (xs ++ [v]))
    | otherwise -> parseIndex t >>= \i ->
        if i >= 0 && i <= length xs
          then Right (Array (insertAt i v xs))
          else Left "add: array index out of range"
  _ -> Left "add: parent is a scalar"
addAt (t : ts) v container = descend t container (addAt ts v)
-----------------------------------------------------------------------------
replaceAt :: [MisoString] -> Value -> Value -> Either MisoString Value
replaceAt []  v _ = Right v
replaceAt [t] v container = case container of
  Object m
    | M.member t m -> Right (Object (M.insert t v m))
    | otherwise    -> Left ("replace: missing key: " <> t)
  Array xs -> parseIndex t >>= \i ->
    if i >= 0 && i < length xs
      then Right (Array (setAt i v xs))
      else Left "replace: array index out of range"
  _ -> Left "replace: parent is a scalar"
replaceAt (t : ts) v container = descend t container (replaceAt ts v)
-----------------------------------------------------------------------------
removeAt :: [MisoString] -> Value -> Either MisoString Value
removeAt []  _ = Left "remove: cannot remove the whole document"
removeAt [t] container = case container of
  Object m
    | M.member t m -> Right (Object (M.delete t m))
    | otherwise    -> Left ("remove: missing key: " <> t)
  Array xs -> parseIndex t >>= \i ->
    if i >= 0 && i < length xs
      then Right (Array (deleteAt i xs))
      else Left "remove: array index out of range"
  _ -> Left "remove: parent is a scalar"
removeAt (t : ts) container = descend t container (removeAt ts)
-----------------------------------------------------------------------------
-- | Descend into the child at token @t@, transform it, and rebuild the parent.
descend
  :: MisoString
  -> Value
  -> (Value -> Either MisoString Value)
  -> Either MisoString Value
descend t container f = case container of
  Object m -> case M.lookup t m of
    Just child -> (\c -> Object (M.insert t c m)) <$> f child
    Nothing    -> Left ("missing key: " <> t)
  Array xs -> parseIndex t >>= \i -> case xs `atMay` i of
    Just child -> (\c -> Array (setAt i c xs)) <$> f child
    Nothing    -> Left "array index out of range"
  _ -> Left "cannot descend into scalar"
-----------------------------------------------------------------------------
parseIndex :: MisoString -> Either MisoString Int
parseIndex t = case reads (fromMisoString t) of
  [(i, "")] | i >= 0 -> Right i
  _                  -> Left ("invalid array index: " <> t)
-----------------------------------------------------------------------------
atMay :: [a] -> Int -> Maybe a
atMay xs i
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise               = Nothing

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [ if j == i then v else x | (j, x) <- zip [0 :: Int ..] xs ]

insertAt :: Int -> a -> [a] -> [a]
insertAt i v xs = let (a, b) = splitAt i xs in a ++ v : b

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = let (a, b) = splitAt i xs in a ++ drop 1 b
-----------------------------------------------------------------------------
-- | Compute a 'Patch' that transforms the first document into the second.
--
-- Correct but not minimal: emits @remove@\/@replace@\/@add@ (arrays are diffed
-- element-by-element by index; trailing elements are added\/removed).
diffPatch :: Value -> Value -> Patch
diffPatch a b = Patch (go [] a b)
  where
    go :: [MisoString] -> Value -> Value -> [Op]
    go _    x y | x == y = []
    go path (Object x) (Object y) =
         [ Remove (Pointer (path ++ [k]))
         | k <- M.keys x, not (M.member k y)
         ]
      ++ [ o
         | (k, yv) <- M.toList y, M.member k x
         , o <- go (path ++ [k]) (x M.! k) yv
         ]
      ++ [ Add (Pointer (path ++ [k])) yv
         | (k, yv) <- M.toList y, not (M.member k x)
         ]
    go path (Array x) (Array y) =
      let n = min (length x) (length y)
      in    [ o
            | i <- [0 .. n - 1]
            , o <- go (path ++ [ms i]) (x !! i) (y !! i)
            ]
         ++ [ Add (Pointer (path ++ [ms i])) (y !! i)
            | i <- [length x .. length y - 1]
            ]
         ++ [ Remove (Pointer (path ++ [ms i]))
            | i <- reverse [length y .. length x - 1]
            ]
    go path _ y = [ Replace (Pointer path) y ]
-----------------------------------------------------------------------------
-- RFC 6902 wire format: a patch is an array of @{ op, path, [value|from] }@.
instance ToJSON Op where
  toJSON = object . \case
    Add p v     -> [ "op" .= ("add" :: MisoString),     "path" .= renderPointer p, "value" .= v ]
    Remove p    -> [ "op" .= ("remove" :: MisoString),  "path" .= renderPointer p ]
    Replace p v -> [ "op" .= ("replace" :: MisoString), "path" .= renderPointer p, "value" .= v ]
    Move f p    -> [ "op" .= ("move" :: MisoString),    "from" .= renderPointer f, "path" .= renderPointer p ]
    Copy f p    -> [ "op" .= ("copy" :: MisoString),    "from" .= renderPointer f, "path" .= renderPointer p ]
    Test p v    -> [ "op" .= ("test" :: MisoString),    "path" .= renderPointer p, "value" .= v ]
-----------------------------------------------------------------------------
instance FromJSON Op where
  parseJSON = withObject "Miso.JSON.Diff.Op" $ \o -> do
    let path = parsePointer <$> o .: "path"
        from = parsePointer <$> o .: "from"
    op <- o .: "op"
    case (op :: MisoString) of
      "add"     -> Add     <$> path <*> o .: "value"
      "remove"  -> Remove  <$> path
      "replace" -> Replace <$> path <*> o .: "value"
      "move"    -> Move    <$> from <*> path
      "copy"    -> Copy    <$> from <*> path
      "test"    -> Test    <$> path <*> o .: "value"
      other     -> fail ("Miso.JSON.Diff: unknown op: " <> fromMisoString other)
-----------------------------------------------------------------------------
instance ToJSON Patch where
  toJSON (Patch ops) = Array (map toJSON ops)
-----------------------------------------------------------------------------
instance FromJSON Patch where
  parseJSON (Array xs) = Patch <$> traverse parseJSON xs
  parseJSON _          = fail "Miso.JSON.Diff.Patch: expected an array of operations"
-----------------------------------------------------------------------------
