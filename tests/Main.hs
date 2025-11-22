-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Control.Monad.State
import Control.Exception hiding (catch)
import Control.Monad
import Language.Javascript.JSaddle
import System.Exit
-----------------------------------------------------------------------------
import Miso
-----------------------------------------------------------------------------
initJSDOM :: JSM ()
initJSDOM = do
  _ <- global # (ms "initJSDOM") $ ()
  pure ()
-----------------------------------------------------------------------------
main :: IO ()
main = run $ do
  initJSDOM
  runTests $ do
    it "Should have access to document.body" $ do
      len <- fromJSValUnchecked =<<
        jsg "document" ! "body" ! "childNodes" ! "length"
      len `shouldBe` (0 :: Int)
    it "Should append a single node to document.body" $ do
      _ <- eval (ms "document.body.appendChild (document.createElement('div'));")
      len <- fromJSValUnchecked =<< do
        jsg "document"
          ! "body"
          ! "childNodes"
          ! "length"
      len `shouldBe` (1 :: Int)
-----------------------------------------------------------------------------
it :: String -> JSM (Bool, String) -> TestM ()
it testName test = do
  (testResult, msg, time) <- (do
     ((x,msg),t) <- liftJSM (clock test)
     pure (x,msg,t))
      `catch` (\(e :: SomeException) ->
         pure (False, show e, 0))
  modify (Test testName testResult msg time:)
-----------------------------------------------------------------------------
data TestResult
  = TestResult
  { failed :: Int
  , passed :: Int
  , total :: Int
  , totalDuration :: Double
  , tests :: [Test]
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Test
  = Test
  { name :: String
  , result :: Bool
  , errorMessage :: String
  , duration :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
type TestM a = StateT [Test] JSM a
-----------------------------------------------------------------------------
should
  :: (Show a, Eq a, Applicative f)
  => (a -> a -> Bool)
  -> a
  -> a
  -> f (Bool, String)
should f x y =
  pure $
    if f x y
      then (True, mempty)
      else (False, "Expecting: " ++ show y ++ " but got: " ++ show x)
-----------------------------------------------------------------------------
shouldNotBe
  :: (Show a, Eq a, Applicative f)
  => a
  -> a
  -> f (Bool, String)
shouldNotBe = should (/=)
-----------------------------------------------------------------------------
shouldBe
  :: (Show a, Eq a, Applicative f)
  => a
  -> a
  -> f (Bool, String)
shouldBe = should (==)
-----------------------------------------------------------------------------
clock :: JSM a -> JSM (a, Double)
clock action = do
  start <- liftJSM now
  x <- action
  stop <- liftJSM now
  pure (x, (stop - start) / 1000)
-----------------------------------------------------------------------------
runTests :: TestM a -> JSM ()
runTests ts = do
  summary <- toResult <$> execStateT ts []
  liftIO $ do
    prettyTests (tests summary)
    when (failed summary > 0) exitFailure
    exitSuccess
      where
        toResult :: [Test] -> TestResult
        toResult xs = TestResult {..}
          where
            passed   = length (filter result xs)
            failed   = length (filter (not . result) xs)
            tests    = xs
            total    = length xs
            totalDuration = sum (map duration xs)
-----------------------------------------------------------------------------
prettyTests :: [Test] -> JSM ()
prettyTests tests = forM_ tests $ \Test {..} ->
  if result
    then jsg "console" # "log" $ [ms "✅", ms "%c" <> ms name, ms "color:green;"]
    else jsg "console" # "log" $ [ms "❌", ms "%c" <> ms name, ms "color:red;"]
-----------------------------------------------------------------------------
