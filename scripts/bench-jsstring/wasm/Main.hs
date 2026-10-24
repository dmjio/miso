-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
-----------------------------------------------------------------------------
-- | Benchmark and property check for "Data.JSString" (the 'MisoString' of the
-- WASM and MicroHs builds). The GHC build runs in Node (@run.mjs@), the
-- MicroHs one in headless Chromium (@run-browser.mjs@).
--
-- * Property check: every function of the new "Data.JSString" against
--   "Data.Text" on random strings (letters, spaces of every kind, emoji).
--
-- * Benchmark: every function, old ("OldJSString", miso master 8bc7848)
--   against new, on 1 KB and 10 KB inputs, plus two whole workloads from
--   haskell-miso.org's search.
--
-- Output is one line per result, @PROP|...@ or @BENCH|...@, for @run.mjs@.
-- @-fno-full-laziness@ and @-fno-cse@ keep each timed call from being
-- floated out of its loop and shared.
module Main (main) where
-----------------------------------------------------------------------------
import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad
import           Data.Char (isSpace, ord, toUpper)
import           Data.IORef
import qualified Data.JSString as N
import qualified Data.List as L
import qualified Data.Text as T
import           GHC.Wasm.Prim
import qualified OldJSString as O
import           System.IO
import           System.Mem (performGC)
-----------------------------------------------------------------------------
#ifndef __MHS__
-- MicroHs runs 'main' itself.
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
foreign import javascript unsafe "performance.now()"
  now :: IO Double
-----------------------------------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getMode
  when (args /= "bench") properties
  when (args /= "props") benchmarks
  putStrLn "DONE"
-----------------------------------------------------------------------------
foreign import javascript unsafe "globalThis.benchMode || 'all'"
  getMode_ :: IO JSString
-----------------------------------------------------------------------------
getMode :: IO String
getMode = fromJSString <$> getMode_
-----------------------------------------------------------------------------
-- | Knobs for slower runtimes (MicroHs): the largest input size, the number
-- of random cases per property, and whether to run the whole 110 KB
-- workloads.
foreign import javascript unsafe "globalThis.benchSizes || '1000,10000'"
  getSizes_ :: IO JSString
-----------------------------------------------------------------------------
-- | Input sizes, from a comma-separated list.
getSizes :: IO [Int]
getSizes = fmap (read . N.unpack) . N.splitOn "," <$> getSizes_
-----------------------------------------------------------------------------
foreign import javascript unsafe "globalThis.benchCases || 2000"
  getCases :: IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe "globalThis.benchWhole !== false"
  wholeWorkloads :: IO Bool
-----------------------------------------------------------------------------
-- Random inputs ------------------------------------------------------------
-----------------------------------------------------------------------------
-- | A small linear congruential generator, so runs are reproducible. Its
-- state is an 'Integer': 'Int' is 32 bits on MicroHs, which throws on
-- overflow.
newtype Gen = Gen (IORef Integer)
-----------------------------------------------------------------------------
newGen :: Int -> IO Gen
newGen seed = Gen <$> newIORef (toInteger seed)
-----------------------------------------------------------------------------
rand :: Gen -> Int -> IO Int
rand (Gen ref) n = do
  x <- readIORef ref
  let x' = (x * 1103515245 + 12345) `mod` 2147483648
  writeIORef ref x'
  pure (fromInteger (x' `mod` toInteger (max 1 n)))
-----------------------------------------------------------------------------
pick :: Gen -> [a] -> IO a
pick g xs = (xs !!) <$> rand g (length xs)
-----------------------------------------------------------------------------
pieces :: [String]
pieces =
  [ "a", "b", "ab", "m", "z", " ", "  ", "\t", "\n", "\r", "x\n", "::"
  , "\160", "\12288", "\8232", "\65279", "\233", "\128512", "AB"
  , "\931", "\931\927\931", "\57344"
  ]
-----------------------------------------------------------------------------
genString :: Gen -> Int -> IO String
genString g maxPieces = do
  n <- rand g (maxPieces + 1)
  concat <$> replicateM n (pick g pieces)
-----------------------------------------------------------------------------
genNonEmpty :: Gen -> Int -> IO String
genNonEmpty g maxPieces = do
  s <- genString g maxPieces
  if null s then genNonEmpty g maxPieces else pure s
-----------------------------------------------------------------------------
-- Property check -----------------------------------------------------------
-----------------------------------------------------------------------------
-- | Runs @cases@ random checks. Each one yields the new result and the
-- "Data.Text" result, both rendered with 'show'; a call that throws counts
-- as the string "error", so both sides must agree on that too.
prop :: String -> (Gen -> IO (String, IO String, IO String)) -> IO ()
prop name check = do
  cases <- getCases
  g <- newGen 42
  failures <- newIORef (0 :: Int)
  example <- newIORef ""
  forM_ [1 .. cases] $ \_ -> do
    (input, new, ref) <- check g
    n <- attempt new
    r <- attempt ref
    when (n /= r) $ do
      modifyIORef' failures (+ 1)
      e <- readIORef example
      when (null e) $
        writeIORef example (input <> " -> new " <> n <> ", Data.Text " <> r)
  f <- readIORef failures
  e <- readIORef example
  putStrLn ("PROP|" <> name <> "|" <> show (cases - f) <> "|" <> show cases <> "|" <> e)
  where
    attempt :: IO String -> IO String
    attempt act = do
      r <- try (act >>= evaluate . forceString)
      pure (either failed id r)
    failed :: SomeException -> String
    failed _ = "error"
    forceString s = length s `seq` s
-----------------------------------------------------------------------------
-- | Shorthands: a property over one random string.
p1 :: Show b => String -> (N.JSString -> b) -> (T.Text -> b) -> IO ()
p1 name f g = prop name $ \gen -> do
  s <- genString gen 10
  pure (show s, pure (show (f (N.pack s))), pure (show (g (T.pack s))))
-----------------------------------------------------------------------------
-- | As 'p1', for functions returning strings.
p1s :: String -> (N.JSString -> N.JSString) -> (T.Text -> T.Text) -> IO ()
p1s name f g = p1 name (N.unpack . f) (T.unpack . g)
-----------------------------------------------------------------------------
p1l :: String -> (N.JSString -> [N.JSString]) -> (T.Text -> [T.Text]) -> IO ()
p1l name f g = p1 name (fmap N.unpack . f) (fmap T.unpack . g)
-----------------------------------------------------------------------------
-- | A property over a non-empty string (for partial functions).
p1ne :: Show b => String -> (N.JSString -> b) -> (T.Text -> b) -> IO ()
p1ne name f g = prop name $ \gen -> do
  s <- genNonEmpty gen 10
  pure (show s, pure (show (f (N.pack s))), pure (show (g (T.pack s))))
-----------------------------------------------------------------------------
-- | A property over a random @Int@ and string.
pInt :: Show b => String -> (Int -> N.JSString -> b) -> (Int -> T.Text -> b) -> IO ()
pInt name f g = prop name $ \gen -> do
  k <- subtract 2 <$> rand gen 14
  s <- genString gen 10
  pure (show (k, s), pure (show (f k (N.pack s))), pure (show (g k (T.pack s))))
-----------------------------------------------------------------------------
-- | A property over a non-empty needle and a string.
pNeedle :: Show b => String -> (N.JSString -> N.JSString -> b) -> (T.Text -> T.Text -> b) -> IO ()
pNeedle name f g = prop name $ \gen -> do
  n <- genNonEmpty gen 2
  s <- genString gen 10
  pure (show (n, s), pure (show (f (N.pack n) (N.pack s))), pure (show (g (T.pack n) (T.pack s))))
-----------------------------------------------------------------------------
-- | A property over any two strings.
p2 :: Show b => String -> (N.JSString -> N.JSString -> b) -> (T.Text -> T.Text -> b) -> IO ()
p2 name f g = prop name $ \gen -> do
  a <- genString gen 6
  b <- genString gen 6
  pure (show (a, b), pure (show (f (N.pack a) (N.pack b))), pure (show (g (T.pack a) (T.pack b))))
-----------------------------------------------------------------------------
-- | A property for each of a few predicates.
pPred :: Show b => String -> ((Char -> Bool) -> N.JSString -> b) -> ((Char -> Bool) -> T.Text -> b) -> IO ()
pPred name f g =
  forM_ predicates $ \(pname, q) ->
    p1 (name <> " " <> pname) (f q) (g q)
-----------------------------------------------------------------------------
predicates :: [(String, Char -> Bool)]
predicates =
  [ ("(== 'a')", (== 'a'))
  , ("isSpace", isSpace)
  , ("(> 'm')", (> 'm'))
  , ("(== '\\128512')", (== '\128512'))
  ]
-----------------------------------------------------------------------------
pChar :: Show b => String -> (Char -> N.JSString -> b) -> (Char -> T.Text -> b) -> IO ()
pChar name f g = prop name $ \gen -> do
  c <- pick gen "a \128512\233"
  s <- genString gen 10
  pure (show (c, s), pure (show (f c (N.pack s))), pure (show (g c (T.pack s))))
-----------------------------------------------------------------------------
pList :: Show b => String -> ([N.JSString] -> b) -> ([T.Text] -> b) -> IO ()
pList name f g = prop name $ \gen -> do
  n <- rand gen 5
  xs <- replicateM n (genString gen 4)
  pure (show xs, pure (show (f (fmap N.pack xs))), pure (show (g (fmap T.pack xs))))
-----------------------------------------------------------------------------
properties :: IO ()
properties = do
  -- Creation and elimination
  p1 "pack/unpack" N.unpack T.unpack
  pChar "singleton" (\c _ -> N.unpack (N.singleton c)) (\c _ -> T.unpack (T.singleton c))
  -- Basic interface
  pChar "cons" (\c s -> N.unpack (N.cons c s)) (\c s -> T.unpack (T.cons c s))
  pChar "snoc" (\c s -> N.unpack (N.snoc s c)) (\c s -> T.unpack (T.snoc s c))
  p2 "append" (\a b -> N.unpack (N.append a b)) (\a b -> T.unpack (T.append a b))
  p1 "uncons" (fmap (fmap N.unpack) . N.uncons) (fmap (fmap T.unpack) . T.uncons)
  p1 "unsnoc" (fmap (\(a, c) -> (N.unpack a, c)) . N.unsnoc) (fmap (\(a, c) -> (T.unpack a, c)) . T.unsnoc)
  p1ne "head" N.head T.head
  p1ne "last" N.last T.last
  p1ne "tail" (N.unpack . N.tail) (T.unpack . T.tail)
  p1ne "init" (N.unpack . N.init) (T.unpack . T.init)
  p1 "null" N.null T.null
  p1 "length" N.length T.length
  pInt "compareLength" (\k s -> N.compareLength s k) (\k s -> T.compareLength s k)
  -- Transformations
  p1s "map toUpper" (N.map toUpper) (T.map toUpper)
  pList "intercalate" (N.unpack . N.intercalate ", ") (T.unpack . T.intercalate ", ")
  pChar "intersperse" (\c s -> N.unpack (N.intersperse c s)) (\c s -> T.unpack (T.intersperse c s))
  pList "transpose" (fmap N.unpack . N.transpose) (fmap T.unpack . T.transpose)
  p1s "reverse" N.reverse T.reverse
  prop "replace" $ \gen -> do
    n <- genNonEmpty gen 2
    r <- genString gen 2
    s <- genString gen 10
    pure ( show (n, r, s)
         , pure (N.unpack (N.replace (N.pack n) (N.pack r) (N.pack s)))
         , pure (T.unpack (T.replace (T.pack n) (T.pack r) (T.pack s)))
         )
  p1s "toCaseFold" N.toCaseFold T.toCaseFold
  p1s "toLower" N.toLower T.toLower
  p1s "toUpper" N.toUpper T.toUpper
  p1s "toTitle" N.toTitle T.toTitle
  pInt "justifyLeft" (\k s -> N.unpack (N.justifyLeft k '.' s)) (\k s -> T.unpack (T.justifyLeft k '.' s))
  pInt "justifyRight" (\k s -> N.unpack (N.justifyRight k '.' s)) (\k s -> T.unpack (T.justifyRight k '.' s))
  pInt "center" (\k s -> N.unpack (N.center k '.' s)) (\k s -> T.unpack (T.center k '.' s))
  -- Folds
  p1 "foldl" (N.foldl (flip (:)) []) (T.foldl (flip (:)) [])
  p1 "foldl'" (N.foldl' (\n c -> (n * 31 + ord c) `mod` 1000003) 7) (T.foldl' (\n c -> (n * 31 + ord c) `mod` 1000003) 7)
  p1ne "foldl1" (N.foldl1 max) (T.foldl1 max)
  p1 "foldr" (N.foldr (:) []) (T.foldr (:) [])
  p1ne "foldr1" (N.foldr1 min) (T.foldr1 min)
  pList "concat" (N.unpack . N.concat) (T.unpack . T.concat)
  p1s "concatMap" (N.concatMap (\c -> if c == 'a' then "<a>" else N.singleton c))
                  (T.concatMap (\c -> if c == 'a' then "<a>" else T.singleton c))
  pPred "any" N.any T.any
  pPred "all" N.all T.all
  p1ne "maximum" N.maximum T.maximum
  p1ne "minimum" N.minimum T.minimum
  -- Scans and accumulating maps
  p1s "scanl" (N.scanl max 'a') (T.scanl max 'a')
  p1s "scanl1" (N.scanl1 max) (T.scanl1 max)
  p1s "scanr" (N.scanr max 'a') (T.scanr max 'a')
  p1s "scanr1" (N.scanr1 max) (T.scanr1 max)
  p1 "mapAccumL" (fmap N.unpack . N.mapAccumL step (0 :: Int)) (fmap T.unpack . T.mapAccumL step 0)
  p1 "mapAccumR" (fmap N.unpack . N.mapAccumR step (0 :: Int)) (fmap T.unpack . T.mapAccumR step 0)
  -- Generation and unfolding
  pInt "replicate" (\k s -> N.unpack (N.replicate k s)) (\k s -> T.unpack (T.replicate k s))
  p1 "unfoldr" (\s -> N.unpack (N.unfoldr (unfold (N.unpack s)) 0)) (\s -> T.unpack (T.unfoldr (unfold (T.unpack s)) 0))
  pInt "unfoldrN" (\k s -> N.unpack (N.unfoldrN k (unfold (N.unpack s)) 0)) (\k s -> T.unpack (T.unfoldrN k (unfold (T.unpack s)) 0))
  -- Substrings
  pInt "take" (\k -> N.unpack . N.take k) (\k -> T.unpack . T.take k)
  pInt "takeEnd" (\k -> N.unpack . N.takeEnd k) (\k -> T.unpack . T.takeEnd k)
  pInt "drop" (\k -> N.unpack . N.drop k) (\k -> T.unpack . T.drop k)
  pInt "dropEnd" (\k -> N.unpack . N.dropEnd k) (\k -> T.unpack . T.dropEnd k)
  pPred "takeWhile" (\q -> N.unpack . N.takeWhile q) (\q -> T.unpack . T.takeWhile q)
  pPred "takeWhileEnd" (\q -> N.unpack . N.takeWhileEnd q) (\q -> T.unpack . T.takeWhileEnd q)
  pPred "dropWhile" (\q -> N.unpack . N.dropWhile q) (\q -> T.unpack . T.dropWhile q)
  pPred "dropWhileEnd" (\q -> N.unpack . N.dropWhileEnd q) (\q -> T.unpack . T.dropWhileEnd q)
  pPred "dropAround" (\q -> N.unpack . N.dropAround q) (\q -> T.unpack . T.dropAround q)
  p1s "strip" N.strip T.strip
  p1s "stripStart" N.stripStart T.stripStart
  p1s "stripEnd" N.stripEnd T.stripEnd
  pInt "splitAt" (\k -> both N.unpack . N.splitAt k) (\k -> both T.unpack . T.splitAt k)
  pNeedle "breakOn" (\n -> both N.unpack . N.breakOn n) (\n -> both T.unpack . T.breakOn n)
  pNeedle "breakOnEnd" (\n -> both N.unpack . N.breakOnEnd n) (\n -> both T.unpack . T.breakOnEnd n)
  pPred "break" (\q -> both N.unpack . N.break q) (\q -> both T.unpack . T.break q)
  pPred "span" (\q -> both N.unpack . N.span q) (\q -> both T.unpack . T.span q)
  p1l "group" N.group T.group
  p1l "groupBy" (N.groupBy sameKind) (T.groupBy sameKind)
  p1l "inits" N.inits T.inits
  p1l "tails" N.tails T.tails
  -- Breaking into many substrings
  pNeedle "splitOn" (\n -> fmap N.unpack . N.splitOn n) (\n -> fmap T.unpack . T.splitOn n)
  pPred "split" (\q -> fmap N.unpack . N.split q) (\q -> fmap T.unpack . T.split q)
  pInt "chunksOf" (\k -> fmap N.unpack . N.chunksOf k) (\k -> fmap T.unpack . T.chunksOf k)
  p1l "lines" N.lines T.lines
  p1l "words" N.words T.words
  pList "unlines" (N.unpack . N.unlines) (T.unpack . T.unlines)
  pList "unwords" (N.unpack . N.unwords) (T.unpack . T.unwords)
  -- Predicates and view patterns
  pNeedle "isPrefixOf" N.isPrefixOf T.isPrefixOf
  pNeedle "isSuffixOf" N.isSuffixOf T.isSuffixOf
  pNeedle "isInfixOf" N.isInfixOf T.isInfixOf
  p2 "stripPrefix" (\a b -> N.unpack <$> N.stripPrefix a b) (\a b -> T.unpack <$> T.stripPrefix a b)
  p2 "stripSuffix" (\a b -> N.unpack <$> N.stripSuffix a b) (\a b -> T.unpack <$> T.stripSuffix a b)
  p2 "commonPrefixes" (\a b -> three N.unpack <$> N.commonPrefixes a b) (\a b -> three T.unpack <$> tCommonPrefixes a b)
  -- Searching, indexing, zipping
  pPred "filter" (\q -> N.unpack . N.filter q) (\q -> T.unpack . T.filter q)
  pPred "find" N.find T.find
  pPred "partition" (\q -> both N.unpack . N.partition q) (\q -> both T.unpack . T.partition q)
  prop "index" $ \gen -> do
    s <- genNonEmpty gen 10
    i <- rand gen (length s)
    pure (show (s, i), pure (show (N.index (N.pack s) i)), pure (show (T.index (T.pack s) i)))
  pPred "findIndex" N.findIndex T.findIndex
  pNeedle "count" N.count T.count
  p2 "zip" N.zip T.zip
  p2 "zipWith" (\a b -> N.unpack (N.zipWith max a b)) (\a b -> T.unpack (T.zipWith max a b))
  -- Conversions
  p1 "textFromJSString" (T.unpack . N.textFromJSString) T.unpack
  prop "textToJSString" $ \gen -> do
    s <- genString gen 10
    pure (show s, pure (N.unpack (N.textToJSString (T.pack s))), pure s)
  prop "toJSString/fromJSString" $ \gen -> do
    s <- genString gen 10
    pure (show s, pure (fromJSString (toJSString s)), pure s)
  prop "toString_Int" $ \gen -> do
    k <- subtract 1000000 <$> rand gen 2000000
    pure (show k, pure (N.unpack (N.toString_Int k)), pure (show k))
  prop "toString_Word" $ \gen -> do
    k <- fromIntegral <$> rand gen 2000000
    pure (show k, pure (N.unpack (N.toString_Word k)), pure (show (k :: Word)))
  prop "toString_Double (reads back)" $ \gen -> do
    a <- rand gen 2000000
    b <- rand gen 999
    let d = fromIntegral (a - 1000000) / fromIntegral (b + 1) :: Double
    pure (show d, pure (show (read (N.unpack (N.toString_Double d)) :: Double)), pure (show d))
  prop "toString_Float (reads back)" $ \gen -> do
    a <- rand gen 2000000
    b <- rand gen 999
    let d = fromIntegral (a - 1000000) / fromIntegral (b + 1) :: Float
    pure (show d, pure (show (read (N.unpack (N.toString_Float d)) :: Float)), pure (show d))
  p1 "empty" (\_ -> N.unpack N.empty) (\_ -> T.unpack T.empty)
  -- Instances
  p2 "compare" compare compare
  p2 "(==)" (==) (==)
  p2 "(<>)" (\a b -> N.unpack (a <> b)) (\a b -> T.unpack (a <> b))
  where
    step :: Int -> Char -> (Int, Char)
    step n c = (n + 1, if even n then toUpper c else c)
    unfold :: String -> Int -> Maybe (Char, Int)
    unfold s i = if i < length s then Just (s !! i, i + 1) else Nothing
    sameKind a b = isSpace a == isSpace b
    both f (a, b) = (f a, f b)
    three f (a, b, c) = (f a, f b, f c)
-----------------------------------------------------------------------------
-- | 'T.commonPrefixes', which MicroHs's "Data.Text" does not have.
tCommonPrefixes :: T.Text -> T.Text -> Maybe (T.Text, T.Text, T.Text)
#ifdef __MHS__
tCommonPrefixes a b =
  case go (T.unpack a) (T.unpack b) of
    ([], _, _) -> Nothing
    (p, x, y) -> Just (T.pack p, T.pack x, T.pack y)
  where
    go (x:xs) (y:ys) | x == y = let (p, r1, r2) = go xs ys in (x : p, r1, r2)
    go xs ys = ([], xs, ys)
#else
tCommonPrefixes = T.commonPrefixes
#endif
-----------------------------------------------------------------------------
-- Benchmark ----------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Prose-like text: words, spaces, the odd newline, accented letter and
-- emoji, cut to @n@ code points. Never contains @|@.
prose :: Int -> IO N.JSString
prose n = do
  g <- newGen 7
  let vocab = [ "component", "view", "update", "model", "effect", "subscription"
              , "router", "fetch", "event", "decoder", "attribute", "miso"
              , "haskell", "wasm", "render", "patch", "café", "\128512" ]
      go acc len
        | len >= n = pure (take n (concat (L.reverse acc)))
        | otherwise = do
            w <- pick g vocab
            sep <- pick g [" ", " ", " ", " ", ", ", ".\n"]
            go (sep : w : acc) (len + length w + length sep)
  N.pack <$> go [] 0
-----------------------------------------------------------------------------
-- | Times @act input@: after a GC (so one run does not pay for the garbage
-- of the last), the median over 5 batches of at least 40 ms each, in
-- microseconds per run. A run of 200 ms or more is timed once. @act@ takes
-- its input as an argument so that no result can be shared between runs.
time :: In -> (In -> IO ()) -> IO Double
time input act = do
  performGC
  t0 <- now
  act input
  t1 <- now
  let once = t1 - t0
  if once >= 200
    then pure (once * 1000)
    else do
      -- Double the repetitions until a batch takes 40 ms: one run can
      -- measure as 0 with a coarse clock (Chromium rounds to 100 us).
      let calibrate reps = do
            t2 <- now
            forM_ [1 .. reps] $ \_ -> act input
            t3 <- now
            if t3 - t2 >= 40 || reps >= 1000000 then pure reps else calibrate (reps * 2)
      reps <- calibrate (1 :: Int)
      batches <- replicateM 5 $ do
        performGC
        t2 <- now
        forM_ [1 .. reps] $ \_ -> act input
        t3 <- now
        pure ((t3 - t2) * 1000 / fromIntegral reps)
      pure (L.sort batches !! 2)
-----------------------------------------------------------------------------
-- | One benchmark row: old and new on the same input.
bench :: String -> String -> In -> (In -> IO ()) -> (In -> IO ()) -> IO ()
bench name workload input old new = do
  o <- time input old
  n <- time input new
  putStrLn ("BENCH|" <> name <> "|" <> workload <> "|" <> show o <> "|" <> show n)
-----------------------------------------------------------------------------
-- | The inputs of one benchmark size.
data In = In
  { s, t, sNeedle, sPiped, sPadded, csv, nine, index, page' :: N.JSString
  , list :: [N.JSString]
  , size :: Int
  , text :: T.Text
  , str :: String
  }
-----------------------------------------------------------------------------
-- Forcing results, so that the timed work is actually done.
fS :: N.JSString -> IO ()
fS s = void (evaluate s)
-----------------------------------------------------------------------------
fL :: [N.JSString] -> IO ()
fL = mapM_ fS
-----------------------------------------------------------------------------
fP :: (N.JSString, N.JSString) -> IO ()
fP (a, b) = fS a >> fS b
-----------------------------------------------------------------------------
fV :: a -> IO ()
fV x = void (evaluate x)
-----------------------------------------------------------------------------
fM :: Maybe N.JSString -> IO ()
fM = onJust fS
-----------------------------------------------------------------------------
-- | 'mapM_' over a 'Maybe' (MicroHs's 'mapM_' takes a list).
onJust :: (a -> IO ()) -> Maybe a -> IO ()
onJust = maybe (pure ())
-----------------------------------------------------------------------------
benchmarks :: IO ()
benchmarks = do
  -- Whole workloads from haskell-miso.org's search
  doc <- prose 2500
  let entry i = N.pack ("/docs/page-" <> show (i :: Int) <> "/\t") <> doc
      index0 = N.intercalate "\n" (fmap entry [1 .. 44])
  page <- prose 3000
  let pageNeedle = page <> " startSub " <> N.take 200 page
  fS index0 >> fS pageNeedle
  let whole = In { s = "", t = "", sNeedle = "", sPiped = "", sPadded = "", csv = "", nine = ""
                 , index = index0, page' = pageNeedle, list = [], size = 0, text = "", str = "" }
  everything <- wholeWorkloads
  when everything $
    bench "lines" "search index, 110 KB, 44 lines" whole (\In{..} -> fL (O.lines index)) (\In{..} -> fL (N.lines index))
  when everything $ bench "breakOn" "one search keystroke: 8 pages of 3 KB" whole
    (\In{..} -> replicateM_ 8 (fP (O.breakOn "startSub" page'))) (\In{..} -> replicateM_ 8 (fP (N.breakOn "startSub" page')))
  -- Every function at two sizes
  sizes <- getSizes
  forM_ sizes $ \n -> do
    s0 <- prose n
    t0 <- prose (n `div` 2)
    let input = In
          { s = s0, t = t0
          , sNeedle = s0 <> "startSub" <> N.take 50 s0
          , sPiped = s0 <> "|"
          , sPadded = "   \t " <> s0 <> " \n  "
          , csv = N.intercalate "," (N.words s0)
          , nine = N.take 9 s0
          , index = "", page' = ""
          , list = N.words s0
          , size = n
          , text = T.pack (N.unpack s0)
          , str = N.unpack s0
          }
        w = show n <> " chars"
    mapM_ (fS . ($ input)) [s, t, sNeedle, sPiped, sPadded, csv, nine] >> fL (list input)
    fV (T.length (text input)) >> fV (length (str input))
    let b name o n' = bench name w input o n'
    -- Creation and elimination
    b "pack" (\In{..} -> (fS (O.pack (N.unpack s)))) (\In{..} -> (fS (N.pack (N.unpack s))))
    b "unpack" (\In{..} -> (fV (length (O.unpack s)))) (\In{..} -> (fV (length (N.unpack s))))
    b "singleton" (\In{..} -> (fS (O.singleton 'x'))) (\In{..} -> (fS (N.singleton 'x')))
    -- Basic interface
    b "cons" (\In{..} -> (fS (O.cons 'x' s))) (\In{..} -> (fS (N.cons 'x' s)))
    b "snoc" (\In{..} -> (fS (O.snoc s 'x'))) (\In{..} -> (fS (N.snoc s 'x')))
    b "append" (\In{..} -> (fS (O.append s t))) (\In{..} -> (fS (N.append s t)))
    b "uncons" (\In{..} -> (onJust (fS . snd) (O.uncons s))) (\In{..} -> (onJust (fS . snd) (N.uncons s)))
    b "unsnoc" (\In{..} -> (onJust (fS . fst) (O.unsnoc s))) (\In{..} -> (onJust (fS . fst) (N.unsnoc s)))
    b "head" (\In{..} -> (fV (O.head s))) (\In{..} -> (fV (N.head s)))
    b "last" (\In{..} -> (fV (O.last s))) (\In{..} -> (fV (N.last s)))
    b "tail" (\In{..} -> (fS (O.tail s))) (\In{..} -> (fS (N.tail s)))
    b "init" (\In{..} -> (fS (O.init s))) (\In{..} -> (fS (N.init s)))
    b "null" (\In{..} -> (fV (O.null s))) (\In{..} -> (fV (N.null s)))
    b "length" (\In{..} -> (fV (O.length s))) (\In{..} -> (fV (N.length s)))
    b "compareLength" (\In{..} -> (fV (O.compareLength s 10))) (\In{..} -> (fV (N.compareLength s 10)))
    -- Transformations
    b "map" (\In{..} -> (fS (O.map toUpper s))) (\In{..} -> (fS (N.map toUpper s)))
    b "intercalate" (\In{..} -> (fS (O.intercalate ", " list))) (\In{..} -> (fS (N.intercalate ", " list)))
    b "intersperse" (\In{..} -> (fS (O.intersperse '-' s))) (\In{..} -> (fS (N.intersperse '-' s)))
    b "transpose" (\In{..} -> (fL (O.transpose [nine, nine, nine]))) (\In{..} -> (fL (N.transpose [nine, nine, nine])))
    b "reverse" (\In{..} -> (fS (O.reverse s))) (\In{..} -> (fS (N.reverse s)))
    b "replace" (\In{..} -> (fS (O.replace "view" "VIEW" s))) (\In{..} -> (fS (N.replace "view" "VIEW" s)))
    b "toCaseFold" (\In{..} -> (fS (O.toCaseFold s))) (\In{..} -> (fS (N.toCaseFold s)))
    b "toLower" (\In{..} -> (fS (O.toLower s))) (\In{..} -> (fS (N.toLower s)))
    b "toUpper" (\In{..} -> (fS (O.toUpper s))) (\In{..} -> (fS (N.toUpper s)))
    b "toTitle" (\In{..} -> (fS (O.toTitle s))) (\In{..} -> (fS (N.toTitle s)))
    b "justifyLeft" (\In{..} -> (fS (O.justifyLeft (2 * size) '.' s))) (\In{..} -> (fS (N.justifyLeft (2 * size) '.' s)))
    b "justifyRight" (\In{..} -> (fS (O.justifyRight (2 * size) '.' s))) (\In{..} -> (fS (N.justifyRight (2 * size) '.' s)))
    b "center" (\In{..} -> (fS (O.center (2 * size) '.' s))) (\In{..} -> (fS (N.center (2 * size) '.' s)))
    -- Folds
    b "foldl" (\In{..} -> (fV (O.foldl (\n c -> n + ord c) 0 s))) (\In{..} -> (fV (N.foldl (\n c -> n + ord c) 0 s)))
    b "foldl'" (\In{..} -> (fV (O.foldl' (\n c -> n + ord c) 0 s))) (\In{..} -> (fV (N.foldl' (\n c -> n + ord c) 0 s)))
    b "foldl1" (\In{..} -> (fV (O.foldl1 max s))) (\In{..} -> (fV (N.foldl1 max s)))
    b "foldr" (\In{..} -> (fV (O.foldr (\c n -> n + ord c) 0 s))) (\In{..} -> (fV (N.foldr (\c n -> n + ord c) 0 s)))
    b "foldr1" (\In{..} -> (fV (O.foldr1 max s))) (\In{..} -> (fV (N.foldr1 max s)))
    b "concat" (\In{..} -> (fS (O.concat list))) (\In{..} -> (fS (N.concat list)))
    b "concatMap" (\In{..} -> (fS (O.concatMap (\c -> if c == ' ' then "  " else O.singleton c) s))) (\In{..} -> (fS (N.concatMap (\c -> if c == ' ' then "  " else N.singleton c) s)))
    b "any (no match)" (\In{..} -> (fV (O.any (== '|') s))) (\In{..} -> (fV (N.any (== '|') s)))
    b "all" (\In{..} -> (fV (O.all (/= '|') s))) (\In{..} -> (fV (N.all (/= '|') s)))
    b "maximum" (\In{..} -> (fV (O.maximum s))) (\In{..} -> (fV (N.maximum s)))
    b "minimum" (\In{..} -> (fV (O.minimum s))) (\In{..} -> (fV (N.minimum s)))
    -- Scans and accumulating maps
    b "scanl" (\In{..} -> (fS (O.scanl max 'a' s))) (\In{..} -> (fS (N.scanl max 'a' s)))
    b "scanl1" (\In{..} -> (fS (O.scanl1 max s))) (\In{..} -> (fS (N.scanl1 max s)))
    b "scanr" (\In{..} -> (fS (O.scanr max 'a' s))) (\In{..} -> (fS (N.scanr max 'a' s)))
    b "scanr1" (\In{..} -> (fS (O.scanr1 max s))) (\In{..} -> (fS (N.scanr1 max s)))
    b "mapAccumL" (\In{..} -> (fS (snd (O.mapAccumL (\n c -> (n + 1, c)) (0 :: Int) s)))) (\In{..} -> (fS (snd (N.mapAccumL (\n c -> (n + 1, c)) (0 :: Int) s))))
    b "mapAccumR" (\In{..} -> (fS (snd (O.mapAccumR (\n c -> (n + 1, c)) (0 :: Int) s)))) (\In{..} -> (fS (snd (N.mapAccumR (\n c -> (n + 1, c)) (0 :: Int) s))))
    -- Generation and unfolding
    b "replicate" (\In{..} -> (fS (O.replicate size "ab"))) (\In{..} -> (fS (N.replicate size "ab")))
    b "unfoldr" (\In{..} -> (fS (O.unfoldr (\i -> if i < size then Just ('x', i + 1) else Nothing) (0 :: Int)))) (\In{..} -> (fS (N.unfoldr (\i -> if i < size then Just ('x', i + 1) else Nothing) (0 :: Int))))
    b "unfoldrN" (\In{..} -> (fS (O.unfoldrN size (\i -> Just ('x', i + 1)) (0 :: Int)))) (\In{..} -> (fS (N.unfoldrN size (\i -> Just ('x', i + 1)) (0 :: Int))))
    -- Substrings
    b "take" (\In{..} -> (fS (O.take (size `div` 2) s))) (\In{..} -> (fS (N.take (size `div` 2) s)))
    b "takeEnd" (\In{..} -> (fS (O.takeEnd (size `div` 2) s))) (\In{..} -> (fS (N.takeEnd (size `div` 2) s)))
    b "drop" (\In{..} -> (fS (O.drop (size `div` 2) s))) (\In{..} -> (fS (N.drop (size `div` 2) s)))
    b "dropEnd" (\In{..} -> (fS (O.dropEnd (size `div` 2) s))) (\In{..} -> (fS (N.dropEnd (size `div` 2) s)))
    b "takeWhile (whole string)" (\In{..} -> (fS (O.takeWhile (/= '|') s))) (\In{..} -> (fS (N.takeWhile (/= '|') s)))
    b "takeWhileEnd (whole string)" (\In{..} -> (fS (O.takeWhileEnd (/= '|') s))) (\In{..} -> (fS (N.takeWhileEnd (/= '|') s)))
    b "dropWhile (whole string)" (\In{..} -> (fS (O.dropWhile (/= '|') s))) (\In{..} -> (fS (N.dropWhile (/= '|') s)))
    b "dropWhile (first 5)" (\In{..} -> (fS (O.dropWhile isSpace sPadded))) (\In{..} -> (fS (N.dropWhile isSpace sPadded)))
    b "dropWhileEnd" (\In{..} -> (fS (O.dropWhileEnd (/= '|') s))) (\In{..} -> (fS (N.dropWhileEnd (/= '|') s)))
    b "dropAround" (\In{..} -> (fS (O.dropAround isSpace sPadded))) (\In{..} -> (fS (N.dropAround isSpace sPadded)))
    b "strip" (\In{..} -> (fS (O.strip sPadded))) (\In{..} -> (fS (N.strip sPadded)))
    b "stripStart" (\In{..} -> (fS (O.stripStart sPadded))) (\In{..} -> (fS (N.stripStart sPadded)))
    b "stripEnd" (\In{..} -> (fS (O.stripEnd sPadded))) (\In{..} -> (fS (N.stripEnd sPadded)))
    b "splitAt" (\In{..} -> (fP (O.splitAt (size `div` 2) s))) (\In{..} -> (fP (N.splitAt (size `div` 2) s)))
    b "breakOn" (\In{..} -> (fP (O.breakOn "startSub" sNeedle))) (\In{..} -> (fP (N.breakOn "startSub" sNeedle)))
    b "breakOnEnd" (\In{..} -> (fP (O.breakOnEnd "startSub" sNeedle))) (\In{..} -> (fP (N.breakOnEnd "startSub" sNeedle)))
    b "break" (\In{..} -> (fP (O.break (== '|') sPiped))) (\In{..} -> (fP (N.break (== '|') sPiped)))
    b "span" (\In{..} -> (fP (O.span (/= '|') sPiped))) (\In{..} -> (fP (N.span (/= '|') sPiped)))
    b "group" (\In{..} -> (fL (O.group s))) (\In{..} -> (fL (N.group s)))
    b "groupBy" (\In{..} -> (fL (O.groupBy (\x y -> isSpace x == isSpace y) s))) (\In{..} -> (fL (N.groupBy (\x y -> isSpace x == isSpace y) s)))
    b "inits (1/10 of input)" (\In{..} -> (fL (O.inits (O.take (size `div` 10) s)))) (\In{..} -> (fL (N.inits (N.take (size `div` 10) s))))
    b "tails (1/10 of input)" (\In{..} -> (fL (O.tails (O.take (size `div` 10) s)))) (\In{..} -> (fL (N.tails (N.take (size `div` 10) s))))
    -- Breaking into many substrings
    b "splitOn" (\In{..} -> (fL (O.splitOn "," csv))) (\In{..} -> (fL (N.splitOn "," csv)))
    b "split" (\In{..} -> (fL (O.split (== ',') csv))) (\In{..} -> (fL (N.split (== ',') csv)))
    b "chunksOf 10" (\In{..} -> (fL (O.chunksOf 10 s))) (\In{..} -> (fL (N.chunksOf 10 s)))
    b "lines" (\In{..} -> (fL (O.lines s))) (\In{..} -> (fL (N.lines s)))
    b "words" (\In{..} -> (fL (O.words s))) (\In{..} -> (fL (N.words s)))
    b "unlines" (\In{..} -> (fS (O.unlines list))) (\In{..} -> (fS (N.unlines list)))
    b "unwords" (\In{..} -> (fS (O.unwords list))) (\In{..} -> (fS (N.unwords list)))
    -- Predicates and view patterns
    b "isPrefixOf" (\In{..} -> (fV (O.isPrefixOf nine s))) (\In{..} -> (fV (N.isPrefixOf nine s)))
    b "isSuffixOf" (\In{..} -> (fV (O.isSuffixOf nine s))) (\In{..} -> (fV (N.isSuffixOf nine s)))
    b "isInfixOf" (\In{..} -> (fV (O.isInfixOf "startSub" sNeedle))) (\In{..} -> (fV (N.isInfixOf "startSub" sNeedle)))
    b "stripPrefix" (\In{..} -> (fM (O.stripPrefix nine s))) (\In{..} -> (fM (N.stripPrefix nine s)))
    b "stripSuffix" (\In{..} -> (fM (O.stripSuffix "|" sPiped))) (\In{..} -> (fM (N.stripSuffix "|" sPiped)))
    b "commonPrefixes" (\In{..} -> (onJust (\(x, _, _) -> fS x) (O.commonPrefixes s sPiped))) (\In{..} -> (onJust (\(x, _, _) -> fS x) (N.commonPrefixes s sPiped)))
    -- Searching, indexing, zipping
    b "filter" (\In{..} -> (fS (O.filter (/= ' ') s))) (\In{..} -> (fS (N.filter (/= ' ') s)))
    b "find (no match)" (\In{..} -> (fV (O.find (== '|') s))) (\In{..} -> (fV (N.find (== '|') s)))
    b "partition" (\In{..} -> (fP (O.partition isSpace s))) (\In{..} -> (fP (N.partition isSpace s)))
    b "index (middle)" (\In{..} -> (fV (O.index s (size `div` 2)))) (\In{..} -> (fV (N.index s (size `div` 2))))
    b "findIndex (no match)" (\In{..} -> (fV (O.findIndex (== '|') s))) (\In{..} -> (fV (N.findIndex (== '|') s)))
    b "count" (\In{..} -> (fV (O.count "view" s))) (\In{..} -> (fV (N.count "view" s)))
    b "zip" (\In{..} -> (fV (length (O.zip s t)))) (\In{..} -> (fV (length (N.zip s t))))
    b "zipWith" (\In{..} -> (fS (O.zipWith max s t))) (\In{..} -> (fS (N.zipWith max s t)))
    -- Conversions
    b "textFromJSString" (\In{..} -> fV (T.length (O.textFromJSString s))) (\In{..} -> fV (T.length (N.textFromJSString s)))
    b "textToJSString" (\In{..} -> fS (O.textToJSString text)) (\In{..} -> fS (N.textToJSString text))
    b "toJSString" (\In{..} -> fS (O.toJSString str)) (\In{..} -> fS (N.toJSString str))
    b "fromJSString" (\In{..} -> fV (length (O.fromJSString s))) (\In{..} -> fV (length (N.fromJSString s)))
    b "toString_Int" (\In{..} -> fS (O.toString_Int (size * 12345))) (\In{..} -> fS (N.toString_Int (size * 12345)))
    b "toString_Word" (\In{..} -> fS (O.toString_Word (fromIntegral size * 12345))) (\In{..} -> fS (N.toString_Word (fromIntegral size * 12345)))
    b "toString_Double" (\In{..} -> fS (O.toString_Double (fromIntegral size / 7))) (\In{..} -> fS (N.toString_Double (fromIntegral size / 7)))
    b "toString_Float" (\In{..} -> fS (O.toString_Float (fromIntegral size / 7))) (\In{..} -> fS (N.toString_Float (fromIntegral size / 7)))
    b "empty" (\In{..} -> fS (O.empty <> nine)) (\In{..} -> fS (N.empty <> nine))
    -- Diagnostics (temporary): raw Data.Text operations on a converted string
    b "diag: T.length . textFromJSString" (\In{..} -> fV (T.length (O.textFromJSString s))) (\In{..} -> fV (T.length (N.textFromJSString s)))
    b "diag: T.length . T.takeWhile . textFromJSString" (\In{..} -> fV (T.length (T.takeWhile (/= '|') (O.textFromJSString s)))) (\In{..} -> fV (T.length (T.takeWhile (/= '|') (N.textFromJSString s))))
    b "diag: T.any . textFromJSString" (\In{..} -> fV (T.any (== '|') (O.textFromJSString s))) (\In{..} -> fV (T.any (== '|') (N.textFromJSString s)))
    b "diag: T.length (precomputed Text)" (\In{..} -> fV (T.length text)) (\In{..} -> fV (T.length text))
    b "diag: T.length . T.takeWhile (precomputed Text)" (\In{..} -> fV (T.length (T.takeWhile (/= '|') text))) (\In{..} -> fV (T.length (T.takeWhile (/= '|') text)))
    b "diag: take via N.take . T.length" (\In{..} -> fS (N.take (T.length (T.takeWhile (/= '|') (N.textFromJSString s))) s)) (\In{..} -> fS (N.takeWhile (/= '|') s))
    -- Instances
    b "compare" (\In{..} -> fV (compare s sPiped)) (\In{..} -> fV (compare s sPiped))
    b "(==)" (\In{..} -> fV (s == sPiped)) (\In{..} -> fV (s == sPiped))
-----------------------------------------------------------------------------
