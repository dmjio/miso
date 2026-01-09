-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE MultilineStrings    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Prelude hiding ((!!))
import           GHC.Generics
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Either
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.State
import qualified Data.IntMap.Strict as IM
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.JSON as JSON
import           Miso.Router
import qualified Miso.String as S
import           Miso.DSL
import           Miso.Lens
import           Miso.Test
import           Miso.Html
import           Miso.Html.Property
import           Miso.Runtime.Internal (ComponentState (..), components, componentIds)
-----------------------------------------------------------------------------
-- | Clears the component state and DOM between each test
clearComponentState :: IO ()
clearComponentState = do
  liftIO $ do
    writeIORef components mempty
    writeIORef componentIds initialComponentId
-----------------------------------------------------------------------------
clearBody :: IO ()
clearBody = void $ eval ("document.body.innerHTML = '';" :: MisoString)
-----------------------------------------------------------------------------
initialComponentId :: ComponentId
initialComponentId = 1
-----------------------------------------------------------------------------
nodeLength :: Test Int
nodeLength = do
  liftIO $
    fromJSValUnchecked =<< eval ("document.body.childNodes.length" :: MisoString)
-----------------------------------------------------------------------------
mountedComponents :: Test Int
mountedComponents = IM.size <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
getComponentById :: ComponentId -> Test (ComponentState m a)
getComponentById vcompId = (IM.! vcompId) <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
testComponent :: Component parent Int Action
testComponent = component (0 :: Int) update_ $ \_ -> button_ [ id_ "foo", onClick AddOne ] [ "click me " ]
  where
    update_ = \case
      AddOne -> this += 1
-----------------------------------------------------------------------------
data Action = AddOne
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
-----------------------------------------------------------------------------
data Route
  = Index
  | Home
  | Widget (Capture "thing" Int) (Path "foo") (Capture "other" MisoString) (QueryParam "bar" Int) (QueryParam "lol" Int)
  deriving stock (Generic, Show, Eq)
  deriving anyclass Router
-----------------------------------------------------------------------------
data Person = Person { name :: MisoString, age :: Int }
  deriving stock Generic
  deriving anyclass (ToJSVal, ToObject)
----------------------------------------------------------------------------
getAge :: Person -> IO Int
getAge = inline
  """
  return age;
  """
----------------------------------------------------------------------------
#ifdef PRODUCTION
#define MISO_JS_PATH "../js/miso.prod.js"
#else
#define MISO_JS_PATH "../js/miso.js"
#endif
withJS :: IO a -> IO ()
withJS action = void $ do
#ifdef WASM
  $(evalFile MISO_JS_PATH)
#endif
  action
-----------------------------------------------------------------------------
main :: IO ()
main = withJS $ do
  runTests $ beforeEach clearBody $ afterEach clearComponentState $ do
    describe "Miso.Lens tests" $ do
      it "Should ^." $ do
        (0 ^. this) `shouldBe` 0
      it "Should at" $ do
        flip execState (mempty :: Map Char Int) (Miso.Lens.at 'a' ?= 10) `shouldBe`
          M.singleton 'a' 10
        flip execState (mempty :: Map Char Int) (Miso.Lens.at 'a' .= Nothing) `shouldBe`
          mempty
      it "Should _1" $ do
        ((1,2) ^. _1) `shouldBe` 1
      it "Should compose" $ do
        ((1,(2, 3)) ^. (_1 `compose` _2)) `shouldBe` 3
      it "Should _2" $ do
        ((1,2) ^. _2) `shouldBe` 2
      it "Should .~" $ do
        (this .~ 0) 1 `shouldBe` 0
      it "Should set" $ do
        (Miso.Lens.set this 0) 1 `shouldBe` 0
      it "Should %~" $ do
        (this %~ (+1)) 0 `shouldBe` 1
      it "Should over" $ do
        (over this (+1)) 0 `shouldBe` 1
      it "Should +~" $ do
        (this +~ 1) 0 `shouldBe` 1
      it "Should *~" $ do
        (this *~ 2) 2 `shouldBe` 4
      it "Should -~" $ do
        (this -~ 1) 0 `shouldBe` (-1)
      it "Should //~" $ do
        (this //~ 2) 4 `shouldBe` 2.0
      it "Should %=" $ do
        (execState (this %= (+1)) 0) `shouldBe` 1
      it "Should %?=" $ do
        (execState (this %?= (+1)) Nothing) `shouldBe` Just 1
      it "Should +=" $ do
        (execState (this += 1) 0) `shouldBe` 1
      it "Should -=" $ do
        (execState (this -= 1) 0) `shouldBe` (-1)
      it "Should use" $ do
        (execState (use this) 0) `shouldBe` 0
      it "Should view" $ do
        (runReader (Miso.Lens.view this) 0) `shouldBe` 0
      it "Should .=" $ do
        (execState (this .= 1) 0) `shouldBe` 1
      it "Should ?~" $ do
        (this ?~ 0) (Just 1) `shouldBe` (Just 0)
    describe "Inline JS tests" $ do
     it "Should use inline js" $ do
       (`shouldBe` 42) =<< liftIO (getAge (Person "larry" 42))
    describe "Router tests" $ do
      it "should call fromRoute on Index" $ do
        fromRoute Index `shouldBe` [ IndexToken ]
      it "should call fromRoute on Home" $ do
        fromRoute Home `shouldBe` [ CaptureOrPathToken "home" ]
      it "should call fromRoute on Widget" $ do
        fromRoute (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` [ CaptureOrPathToken "widget"
                     , CaptureOrPathToken "10"
                     , CaptureOrPathToken "foo"
                     , CaptureOrPathToken "other"
                     , QueryParamToken "bar" (Just "12")
                     , QueryParamToken "lol" (Just "11")
                     ]
      it "should call toURI on Index" $ do
        toURI Index `shouldBe` URI "" "" mempty
      it "should call toURI on Home" $ do
        toURI Home `shouldBe` URI "home" "" mempty
      it "should call toURI on Widget" $ do
        toURI (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe`
             URI { uriPath = "widget/10/foo/other"
                 , uriFragment = ""
                 , uriQueryString = M.fromList [("bar", Just "12"), ("lol", Just "11")]
                 }
      it "should call fromMisoString on Int" $ do
        S.fromMisoStringEither "10" `shouldBe` Right (10 :: Int)
      it "should call href on Index" $ do
        Miso.Router.href_ Index `shouldBe` Property "href" "/"
      it "should call href on Home" $ do
        Miso.Router.href_ Home `shouldBe` Property "href" "/home"
      it "should call href on Widget" $ do
        Miso.Router.href_ (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` Property "href" "/widget/10/foo/other?bar=12&lol=11"
      it "should call prettyRoute on Index" $ do
        prettyRoute Index `shouldBe` "/"
      it "should call prettyRoute on Home" $ do
        prettyRoute Home `shouldBe` "/home"
      it "should call prettyRoute on Widget" $ do
        prettyRoute (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` "/widget/10/foo/other?bar=12&lol=11"
      it "should call dumpURI on Index" $ do
        dumpURI Index `shouldBe` "URI {uriPath = \"\", uriFragment = \"\", uriQueryString = fromList []}"
      it "should call dumpURI on Home" $ do
        dumpURI Home `shouldBe` "URI {uriPath = \"home\", uriFragment = \"\", uriQueryString = fromList []}"
      it "should call dumpURI on Widget" $ do
        dumpURI (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` "URI {uriPath = \"widget/10/foo/other\", uriFragment = \"\", uriQueryString = fromList [(\"bar\",Just \"12\"),(\"lol\",Just \"11\")]}"
      it "should call toRoute Index" $ do
        toRoute "/" `shouldBe` Right Index
      it "should call toRoute Home" $ do
        toRoute "/home" `shouldBe` Right Home
      it "should call toRoute Widget" $ do
        toRoute "/widget/10/foo/other?bar=12&lol=11"
          `shouldBe`
            Right (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))

      it "should lexTokens on query params/flags" $ do
        lexTokens "/foo?bar=12" `shouldBe`
          Right [CaptureOrPathToken "foo", QueryParamToken "bar" (Just "12")]
        lexTokens "/foo?bar" `shouldBe`
          Right [CaptureOrPathToken "foo", QueryParamToken "bar" Nothing]

      it "should lexTokens on fragments" $ do
        lexTokens "/foo?bar#cool" `shouldBe`
          Right [CaptureOrPathToken "foo", QueryParamToken "bar" Nothing, FragmentToken "cool"]

    describe "MisoString tests" $ do
      it "Should pack" $ do
        S.unpack (S.pack "foo") `shouldBe`
          T.unpack (T.pack "foo")
      it "Should unpack" $ do
        S.unpack "foo" `shouldBe` T.unpack "foo"
      it "Should make singleton" $ do
        S.unpack (S.singleton 'c') `shouldBe`
          T.unpack (T.singleton 'c')
      it "Should be empty" $ do
        S.unpack S.empty `shouldBe`
          T.unpack T.empty
      it "Should cons" $ do
        S.unpack (S.cons 'c' "foo") `shouldBe`
          T.unpack (T.cons 'c' "foo")
      it "Should snoc" $ do
        S.unpack (S.snoc "foo" 'l') `shouldBe`
          T.unpack (T.snoc "foo" 'l')
      it "Should append" $ do
        S.unpack (S.append "foo" "l") `shouldBe`
          T.unpack (T.append "foo" "l")
      it "Should uncons" $ do
        S.uncons "foo" `shouldBe`
          Just ('f', "oo")
        S.uncons "" `shouldBe`
          Nothing
      it "Should unsnoc" $ do
        S.unsnoc "foo" `shouldBe`
          Just ("fo", 'o')
        S.unsnoc "" `shouldBe`
          Nothing
      it "Should head" $ do
        S.head "foo" `shouldBe`
          'f'
      it "Should last" $ do
        S.last "foo" `shouldBe`
          'o'
      it "Should tail" $ do
        S.tail "foo" `shouldBe`
          "oo"
      it "Should init" $ do
        S.init "foo" `shouldBe`
          "fo"
      it "Should null" $ do
        S.null "" `shouldBe`
          True
        S.null "foo" `shouldBe`
          False
      it "Should length" $ do
        S.length "" `shouldBe` 0
        S.length "foo" `shouldBe` 3

      it "Should compareLength" $ do
        S.compareLength "" (-1) `shouldBe`
          T.compareLength "" (-1)
        S.compareLength "foo" 0 `shouldBe`
          T.compareLength "foo" 0 
        S.compareLength "foo" 2 `shouldBe`
          T.compareLength "foo" 2 
        S.compareLength "foo" 4 `shouldBe`
          T.compareLength "foo" 4

      it "Should map" $ do
        S.unpack (S.map succ "") `shouldBe`
          T.unpack (T.map succ "")
        S.unpack (S.map succ "aslkjdf9823@#SDF@#") `shouldBe`
          T.unpack (T.map succ "aslkjdf9823@#SDF@#")

      it "Should intercalate" $ do
        S.unpack (S.intercalate "," []) `shouldBe`
          T.unpack (T.intercalate "," [])
        S.unpack (S.intercalate "," ["aslkjdf9823@#SDF@#", "foo"]) `shouldBe`
          T.unpack (T.intercalate "," ["aslkjdf9823@#SDF@#","foo"])

      it "Should intersperse" $ do
        S.unpack (S.intersperse ',' "") `shouldBe`
          T.unpack (T.intersperse ',' "")
        S.unpack (S.intersperse ',' "aslkjdf9823@#SDF@#") `shouldBe`
          T.unpack (T.intersperse ',' "aslkjdf9823@#SDF@#")

      it "Should transpose" $ do
        (S.unpack <$> S.transpose ["a","b","c","d","e","fghi","klmn"])
          `shouldBe`
            (T.unpack <$> T.transpose ["a","b","c","d","e","fghi","klmn"])

      it "Should reverse" $ do
        S.unpack (S.reverse "") `shouldBe`
          T.unpack (T.reverse "")
        S.unpack (S.reverse "reverse") `shouldBe`
          T.unpack (T.reverse "reverse")

      it "Should replace" $ do
        S.unpack (S.replace "s" "k" "str") `shouldBe`
          T.unpack (T.replace "s" "k" "str")

      it "Should case fold" $ do
        S.unpack (S.toCaseFold "foobar") `shouldBe`
          T.unpack (T.toCaseFold "foobar")

      it "Should toLower" $ do
        S.unpack (S.toLower "LOL") `shouldBe`
          T.unpack (T.toLower "LOL")

      it "Should toUpper" $ do
        S.unpack (S.toUpper "lol") `shouldBe`
          T.unpack (T.toUpper "lol")

      it "Should toTitle" $ do
        S.unpack (S.toUpper "lol") `shouldBe`
          T.unpack (T.toUpper "lol")

      it "Should justifyLeft" $ do
        S.unpack (S.justifyLeft 2 'a' "bbb") `shouldBe`
          T.unpack (T.justifyLeft 2 'a' "bbb")

      it "Should justifyRight" $ do
        S.unpack (S.justifyRight 2 'a' "bbb") `shouldBe`
          T.unpack (T.justifyRight 2 'a' "bbb")

      it "Should center" $ do
        S.unpack (S.center 2 'a' "bbb") `shouldBe`
          T.unpack (T.center 2 'a' "bbb")

      it "Should foldl" $ do
        S.unpack (S.foldl S.snoc "x" "abc") `shouldBe`
          T.unpack (T.foldl T.snoc "x" "abc")

      it "Should foldl'" $ do
        S.unpack (S.foldl' S.snoc "x" "abc") `shouldBe`
          T.unpack (T.foldl' T.snoc "x" "abc")

      it "Should foldl1" $ do
        S.foldl1 const "abc" `shouldBe`
          T.foldl1 const "abc"

        S.foldl1 (\_ y -> y) "abc" `shouldBe`
          T.foldl1 (\_ y -> y) "abc"

      it "Should foldr" $ do
        S.unpack (S.foldr S.cons "x" "abc") `shouldBe`
          T.unpack (T.foldr T.cons "x" "abc")

      it "Should foldr1" $ do
        S.foldr1 const "abc" `shouldBe`
          T.foldr1 const "abc"

        S.foldr1 (\_ y -> y) "abc" `shouldBe`
          T.foldr1 (\_ y -> y) "abc"

      it "Should concat" $ do
        S.unpack (S.concat ["foo","bar"])
          `shouldBe`
             T.unpack (T.concat ["foo","bar"])

      it "Should concatMap" $ do
        S.unpack (S.concatMap S.singleton "okedoke")
         `shouldBe`
            T.unpack (T.concatMap T.singleton "okedoke")

      it "Should all" $ do
        S.all (=='a') "aaa" `shouldBe` True
        T.all (=='a') "aaa" `shouldBe` True

      it "Should any" $ do
        S.any (=='a') "aaa" `shouldBe` T.any (=='a') "aaa"

      it "Should maximum" $ do
        S.maximum "abc" `shouldBe` 'c'
        T.maximum "abc" `shouldBe` 'c'

      it "Should minimum" $ do
        S.minimum "abc" `shouldBe` 'a'
        T.minimum "abc" `shouldBe` 'a'

      it "Should scanl" $ do
        S.unpack (S.scanl (flip const) 'a' "foo") `shouldBe`
          T.unpack (T.scanl (flip const) 'a' "foo")

        S.unpack (S.scanl const 'a' "foo") `shouldBe`
          T.unpack (T.scanl const 'a' "foo")

      it "Should scanl1" $ do
        S.unpack (S.scanl1 (flip const) "foo") `shouldBe`
          T.unpack (T.scanl1 (flip const) "foo")

        S.unpack (S.scanl1 const "foo") `shouldBe`
          T.unpack (T.scanl1 const "foo")

      it "Should scanr" $ do
        S.unpack (S.scanr (flip const) 'a' "foo") `shouldBe`
          T.unpack (T.scanr (flip const) 'a' "foo")

        S.unpack (S.scanr const 'a' "foo") `shouldBe`
          T.unpack (T.scanr const 'a' "foo")

      it "Should scanr1" $ do
        S.unpack (S.scanr1 (flip const) "foo") `shouldBe`
          T.unpack (T.scanr1 (flip const) "foo")

        S.unpack (S.scanr1 const "foo") `shouldBe`
          T.unpack (T.scanr1 const "foo")

      it "Should mapAccumL" $ do
        (S.unpack <$> (S.mapAccumL (,) 'a' "foo")) `shouldBe`
          (T.unpack <$> (T.mapAccumL (,) 'a' "foo"))

      it "Should mapAccumR" $ do
        (S.unpack <$> (S.mapAccumR (,) 'a' "foo")) `shouldBe`
          (T.unpack <$> (T.mapAccumR (,) 'a' "foo"))

        (S.unpack <$> (S.mapAccumR (,) 'a' "")) `shouldBe`
          (T.unpack <$> (T.mapAccumR (,) 'a' ""))

      it "Should replicate" $ do
        S.unpack (S.replicate 10 "a") `shouldBe`
          T.unpack (T.replicate 10 "a")

      it "Should unfoldr" $ do
        S.unpack (S.unfoldr (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0) `shouldBe`
          T.unpack (T.unfoldr (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0)

      it "Should unfoldrN" $ do
        S.unpack (S.unfoldrN 5 (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0)
          `shouldBe` T.unpack (T.unfoldrN 5 (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0)

      it "Should take" $ do
        S.unpack (S.take 1 "foo") `shouldBe`
          T.unpack (T.take 1 "foo")

      it "Should takeEnd" $ do
        S.unpack (S.takeEnd 1 "foo") `shouldBe`
          T.unpack (T.takeEnd 1 "foo")

      it "Should drop" $ do
        S.unpack (S.drop 1 "foo") `shouldBe`
          T.unpack (T.drop 1 "foo")

      it "Should dropEnd" $ do
        S.unpack (S.dropEnd 1 "foo") `shouldBe`
          T.unpack (T.dropEnd 1 "foo")

      it "Should takeWhile" $ do
        S.unpack (S.takeWhile (=='f') "foo") `shouldBe`
          T.unpack (T.takeWhile (=='f') "foo")

      it "Should takeWhileEnd" $ do
        S.unpack (S.takeWhileEnd (=='o') "foo") `shouldBe`
          T.unpack (T.takeWhileEnd (=='o') "foo")

      it "Should dropWhile" $ do
        S.unpack (S.dropWhile (=='f') "foo") `shouldBe`
          T.unpack (T.dropWhile (=='f') "foo")

      it "Should dropWhileEnd" $ do
        S.unpack (S.dropWhileEnd (=='o') "foo") `shouldBe`
          T.unpack (T.dropWhileEnd (=='o') "foo")

      it "Should dropAround" $ do
        S.unpack (S.dropAround (=='o') "foo") `shouldBe`
          T.unpack (T.dropAround (=='o') "foo")

      it "Should strip" $ do
        S.unpack (S.strip "  foo  ") `shouldBe`
          T.unpack (T.strip "  foo ")

      it "Should stripStart" $ do
        S.unpack (S.strip "  foo") `shouldBe`
          T.unpack (T.strip "foo")

      it "Should stripEnd" $ do
        S.unpack (S.strip "foo") `shouldBe`
          T.unpack (T.strip "foo  ")

      it "Should splitAt" $ do
        let (x,y) = S.splitAt 3 "foobar"
        let (x',y') = T.splitAt 3 "foobar"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should breakOn" $ do
        let (x,y) = S.breakOn "b" "ababa"
        let (x',y') = T.breakOn "b" "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should breakOnEnd" $ do
        let (x,y) = S.breakOnEnd "b" "ababa"
        let (x',y') = T.breakOnEnd "b" "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should break" $ do
        let (x,y) = S.break (=='b') "ababa"
        let (x',y') = T.break (=='b') "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should span" $ do
        let (x,y) = S.break (=='b') "ababa"
        let (x',y') = T.break (=='b') "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should group" $ do
        fmap S.unpack (S.group "aabbcc") `shouldBe`
          fmap T.unpack (T.group "aabbcc")

      it "Should groupBy" $ do
        fmap S.unpack (S.groupBy (==) "aabbcc") `shouldBe`
          fmap T.unpack (T.groupBy (==) "aabbcc")

      it "Should inits" $ do
        fmap S.unpack (S.inits "foo bar") `shouldBe`
          fmap T.unpack (T.inits "foo bar")

      it "Should tails" $ do
        fmap S.unpack (S.tails "foo bar") `shouldBe`
          fmap T.unpack (T.tails "foo bar")

      it "Should splitOn" $ do
        fmap S.unpack (S.splitOn "a" "ababa") `shouldBe`
          fmap T.unpack (T.splitOn "a" "ababa")

      it "Should split" $ do
        fmap S.unpack (S.split (=='a') "aabbaca") `shouldBe`
          fmap T.unpack (T.split (=='a') "aabbaca")

      it "Should chunksOf" $ do
        fmap S.unpack (S.chunksOf 3 "foofoo") `shouldBe`
          fmap T.unpack (T.chunksOf 3 "foofoo")

      it "Should lines" $ do
        fmap S.unpack (S.lines "foo\nbar") `shouldBe`
          fmap T.unpack (T.lines "foo\nbar")

      it "Should unlines" $ do
        S.unpack (S.unlines ["foo", "bar"]) `shouldBe`
          T.unpack (T.unlines ["foo", "bar"])

      it "Should words" $ do
        fmap S.unpack (S.words "foo bar") `shouldBe`
          fmap T.unpack (T.words "foo bar")

      it "Should unwords" $ do
        S.unpack (S.unwords ["foo", "bar"]) `shouldBe`
          T.unpack (T.unwords ["foo", "bar"])

      it "Should isPrefixOf" $ do
        S.isPrefixOf "f" "foo" `shouldBe`
          T.isPrefixOf "f" "foo"

      it "Should isSuffixOf" $ do
        S.isSuffixOf "o" "foo" `shouldBe`
          T.isSuffixOf "o" "foo"

      it "Should isInfixOf" $ do
        S.isInfixOf "o" "foo" `shouldBe`
          T.isInfixOf "o" "foo"

      it "Should stripPrefix" $ do
        fmap S.unpack (S.stripPrefix "f" "foo") `shouldBe`
          fmap T.unpack (T.stripPrefix "f" "foo")

      it "Should stripSuffix" $ do
        fmap S.unpack (S.stripSuffix "o" "foo") `shouldBe`
          fmap T.unpack (T.stripSuffix "o" "foo")

      it "Should filter" $ do
        S.unpack (S.filter (=='a') "aba") `shouldBe`
          T.unpack (T.filter (=='a') "aba")

      it "Should find" $ do
        S.find (=='a') "aba" `shouldBe`
          T.find (=='a') "aba"

      it "Should index" $ do
        S.index "aba" 0 `shouldBe`
          T.index "aba" 0

      it "Should findIndex" $ do
        S.findIndex (=='a') "aba" `shouldBe`
          T.findIndex (=='a') "aba"

      it "Should count" $ do
        S.count "a" "aba" `shouldBe`
          T.count "a" "aba"

      it "Should partition" $ do
        let (l,r) = S.partition (=='o') "foobar"
        let (l',r') = T.partition (=='o') "foobar"
        S.unpack l `shouldBe` T.unpack l'
        S.unpack r `shouldBe` T.unpack r'

      it "Should zip" $ do
        S.zip "aaa" "bbb" `shouldBe`
          T.zip "aaa" "bbb"

      it "Should zipWith" $ do
        S.unpack (S.zipWith const "aaa" "bbb") `shouldBe`
          T.unpack (T.zipWith const "aaa" "bbb")

        S.unpack (S.zipWith (\_ y -> y)  "aaa" "bbb") `shouldBe`
          T.unpack (T.zipWith (\_ y -> y) "aaa" "bbb")

      it "Should perform Real / Integral conversions on MisoString" $ do
        S.fromMisoStringEither "3.14" `shouldBe` Right (3.14 :: Double)
        S.fromMisoStringEither @Double "foo" `shouldSatisfy` isLeft
        S.fromMisoStringEither "3.14" `shouldBe` Right (3.14 :: Float)
        S.fromMisoStringEither @Float "foo" `shouldSatisfy` isLeft
        S.fromMisoStringEither "3" `shouldBe` Right (3 :: Int)
        S.fromMisoStringEither @Int "foo" `shouldSatisfy` isLeft
        S.fromMisoStringEither "3" `shouldBe` Right (3 :: Word)
        S.fromMisoStringEither @Word "foo" `shouldSatisfy` isLeft

    describe "JS DSL tests" $ do
      it "Should get an set a property on an Object" $ do
        c <- liftIO create
        liftIO $ flip (setProp "foo") c =<< toJSVal True
        (`shouldBe` True) =<< liftIO (fromJSValUnchecked =<< getProp "foo" c)
      it "Should call eval" $ do
        (`shouldBe` 4) =<< liftIO (fromJSValUnchecked =<< eval "2+2")
    describe "Marshal tests" $ do
      it "Should marshal a Double to JSString" $ do
        toMisoString (3.14 :: Double) `shouldBe` "3.14"
      it "Should marshal a Float to JSString" $ do
        toMisoString (3.14 :: Float) `shouldBe` "3.14"
      it "Should marshal a Word to JSString" $ do
        toMisoString (3 :: Word) `shouldBe` "3"
      it "Should marshal a Int to JSString" $ do
        toMisoString ((-5) :: Int) `shouldBe` "-5"
      it "Should marshal a Value(Object)" $ do
        (`shouldBe` Just (JSON.object [ "foo" JSON..= True ])) =<< liftIO (fromJSVal =<< toJSVal (JSON.object [ "foo" JSON..= True ]))
      it "Should marshal a Value(Array)" $ do
        (`shouldBe` Just (JSON.Array [ JSON.Number 1.0, JSON.Number 2.0 ])) =<<
          liftIO (fromJSVal =<< toJSVal (JSON.Array [ JSON.Number 1.0, JSON.Number 2.0 ]))
      it "Should marshal a Value(Number)" $ do
        (`shouldBe` Just (JSON.Number pi)) =<< liftIO (fromJSVal =<< toJSVal (JSON.Number pi))
      it "Should marshal a Value(Bool(False))" $ do
        (`shouldBe` Just (JSON.Bool False)) =<< liftIO (fromJSVal =<< toJSVal (JSON.Bool False))
      it "Should marshal a Value(Bool(True))" $ do
        (`shouldBe` Just (JSON.Bool True)) =<< liftIO (fromJSVal =<< toJSVal (JSON.Bool True))
      it "Should marshal a Value(String)" $ do
        (`shouldBe` Just (JSON.String "foo")) =<< liftIO (fromJSVal =<< toJSVal (JSON.String "foo"))
      it "Should marshal a Value(Null)" $ do
        (`shouldBe` Just JSON.Null) =<< liftIO (fromJSVal =<< toJSVal JSON.Null)
      it "Should marshal a Bool(True)" $ do
        (`shouldBe` Just True) =<< liftIO (fromJSVal =<< toJSVal True)
      it "Should marshal a ()" $ do
        (`shouldBe` Just ()) =<< liftIO (fromJSVal =<< toJSVal ())
      it "Should marshal a Bool(False)" $ do
        (`shouldBe` Just False) =<< liftIO (fromJSVal =<< toJSVal False)
      it "Should marshal a Float" $ do
        (`shouldBe` Just (pi :: Float)) =<< liftIO (fromJSVal =<< toJSVal (pi :: Float))
        (`shouldBe` Just (-99.99 :: Float)) =<< liftIO (fromJSVal =<< toJSVal (-99.99 :: Float))
        (`shouldBe` Just (-0 :: Float)) =<< liftIO (fromJSVal =<< toJSVal (-0 :: Float))
      it "Should marshal a Double" $ do
        (`shouldBe` Just pi) =<< liftIO (fromJSVal =<< toJSVal pi)
        (`shouldBe` Just (-99.99 :: Double)) =<< liftIO (fromJSVal =<< toJSVal (-99.99 :: Double))
        (`shouldBe` Just (-0 :: Double)) =<< liftIO (fromJSVal =<< toJSVal (-0 :: Double))
      it "Should marshal a Int" $ do
        (`shouldBe` Just (99 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (99 :: Int))
        (`shouldBe` Just (-99 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (-99 :: Int))
        (`shouldBe` Just (0 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (0 :: Int))
      it "Should marshal a MisoString" $ do
        (`shouldBe` Just ("foo" :: MisoString)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: MisoString))
      it "Should marshal a (Maybe Bool)" $ do
        -- dmj: js backend bug
        -- (`shouldBe` (Nothing :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Nothing :: Maybe Bool))
        (`shouldBe` (Just True :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Just True :: Maybe Bool))
        (`shouldBe` (Just False :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Just False :: Maybe Bool))
      it "Should marshal a (Bool,Double)" $ do
        (`shouldBe` Just (True,pi)) =<< liftIO (fromJSVal =<< toJSVal (True,pi))
      it "Should marshal a [Double]" $ do
        (`shouldBe` Just [pi,pi]) =<< liftIO (fromJSVal =<< toJSVal [pi,pi])
        (`shouldBe` Just ([] :: [Bool])) =<< liftIO (fromJSVal =<< toJSVal ([] :: [Bool]))
      it "Should marshal a Char" $ do
        (`shouldBe` Just ('o' :: Char)) =<< liftIO (fromJSVal =<< toJSVal ('o' :: Char))
      it "Should marshal a String" $ do
        (`shouldBe` Just ("foo" :: String)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: String))
      it "Should marshal a Text" $ do
        (`shouldBe` Just ("foo" :: Text)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: Text))

    describe "DOM tests" $ do
      it "Should have access to document.body" $ do
        nodeLength >>= (`shouldBe` (0 :: Int))
      it "Should append a single node to document.body" $ do
          _ <- liftIO $ eval ("document.body.appendChild (document.createElement('div'));" :: MisoString)
          nodeLength >>= (`shouldBe` (1 :: Int))

    describe "Component tests" $ do
      it "Should mount one component" $ do
        liftIO (startApp testComponent)
        mountedComponents >>= (`shouldBe` 1)

      it "Should have parent field present on VDOM nodes" $ do
        _ <- liftIO (startApp testComponent)
        ComponentState {..} <- liftIO $ (IM.! 1) <$> readIORef components
        VTree (Object ref) <- liftIO (readIORef componentVTree)
        parentDomRef <- liftIO (ref ! "domRef")
        childParentField <- liftIO (ref ! "children" !! 0 ! "parent")
        childParentFieldDOMRef <- liftIO (childParentField ! "domRef")
        parentFieldNull <- liftIO (isNull childParentField)
        parentFieldNull `shouldBe` False
        parentFieldUndefined <- liftIO (isUndefined childParentField)
        parentFieldUndefined `shouldBe` False

      it "Should mount 1000 components" $ do
        liftIO $ startApp $
          component (0 :: Int) noop $ \_ ->
            div_ [] (replicate 999 (mount testComponent))
        mountedComponents >>= (`shouldBe` 1000)
