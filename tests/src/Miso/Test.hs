-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Test
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- An hspec-like miso test framework. Meant for testing @miso@ @Component@.
-- The testing framework operates in the jsaddle JSM monad and has access
-- to the DOM courtesy of JSDOM (this is subject to change).
--
-- @
--
-- main :: IO ()
-- main = runTests $ do
--   describe "Arithmetic tests" $ do
--     it "2 + 2 = 4" $ do
--       (2 + 2) `shouldBe` 4
--
-- @
--
----------------------------------------------------------------------------
module Miso.Test
  ( -- * Test Combinators
    describe
  , it
  , expect
  , before
  , shouldBe
  , shouldNotBe
  , runTests
  -- * Utils
  , jsm
  , choose
  -- * Types
  , Test
  ) where
-----------------------------------------------------------------------------
import           Control.Exception (SomeException)
import           Language.Javascript.JSaddle.Monad
import           Text.Printf
import           Control.Monad.State
import           Control.Monad
import           Language.Javascript.JSaddle
import           System.Exit
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
-----------------------------------------------------------------------------
describe :: MisoString -> Test () -> Test ()
describe name tests = do
  currentTestGroup .= name
  tests
-----------------------------------------------------------------------------
it :: MisoString -> Test () -> Test ()
it name action = do
  preamble <- use beforeAction
  liftJSM preamble
  total += 1
  currentTestName .= name
  Clocked {..} <- clock action
  currentTestTime .= time
  totalDuration += time
  successful <- use currentTestResult
  errorMessage <- use currentErrorMessage
  if successful
    then passed += 1
    else failed += 1
  testGroup <- use currentTestGroup
  caughtEx <- use caughtException
  when (successful || caughtEx) $ do
    liftJSM $ prettyTest CurrentTest
      { duration = time
      , ..
      }
  currentTestResult .= True
  currentErrorMessage .= mempty
  caughtException .= False
-----------------------------------------------------------------------------
data CurrentTest
  = CurrentTest
  { testGroup :: MisoString
  , name :: MisoString
  , successful :: Bool
  , errorMessage :: MisoString
  , duration :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
type Test a = StateT TestState JSM a
-----------------------------------------------------------------------------
data TestState
  = TestState
  { _currentTestGroup :: MisoString
  , _currentErrorMessage :: MisoString
  , _currentTestName :: MisoString
  , _currentTestTime :: Double
  , _expects :: Int
  , _failed :: Int
  , _passed :: Int
  , _total :: Int
  , _totalDuration :: Double
  , _currentTestResult :: Bool
  , _beforeAction :: JSM ()
  , _caughtException :: Bool
  }
-----------------------------------------------------------------------------
emptyTestState :: TestState
emptyTestState = TestState mempty mempty mempty 0 0 0 0 0 0 True (pure ()) False
-----------------------------------------------------------------------------
beforeAction :: Lens TestState (JSM ())
beforeAction = lens _beforeAction $ \r x -> r { _beforeAction = x }
-----------------------------------------------------------------------------
expects :: Lens TestState Int
expects = lens _expects $ \r x -> r { _expects = x }
-----------------------------------------------------------------------------
caughtException :: Lens TestState Bool
caughtException = lens _caughtException $ \r x -> r { _caughtException = x }
-----------------------------------------------------------------------------
totalDuration :: Lens TestState Double
totalDuration = lens _totalDuration $ \r x -> r { _totalDuration = x }
-----------------------------------------------------------------------------
passed :: Lens TestState Int
passed = lens _passed $ \r x -> r { _passed = x }
-----------------------------------------------------------------------------
failed :: Lens TestState Int
failed = lens _failed $ \r x -> r { _failed = x }
-----------------------------------------------------------------------------
total :: Lens TestState Int
total = lens _total $ \r x -> r { _total = x }
-----------------------------------------------------------------------------
currentTestResult :: Lens TestState Bool
currentTestResult = lens _currentTestResult $ \r x -> r { _currentTestResult = x }
-----------------------------------------------------------------------------
currentTestName :: Lens TestState MisoString
currentTestName = lens _currentTestName $ \r x -> r { _currentTestName = x }
-----------------------------------------------------------------------------
currentErrorMessage :: Lens TestState MisoString
currentErrorMessage = lens _currentErrorMessage $ \r x -> r { _currentErrorMessage = x }
-----------------------------------------------------------------------------
currentTestGroup :: Lens TestState MisoString
currentTestGroup = lens _currentTestGroup $ \r x -> r { _currentTestGroup = x }
-----------------------------------------------------------------------------
currentTestTime :: Lens TestState Double
currentTestTime = lens _currentTestTime $ \r x -> r { _currentTestTime = x }
-----------------------------------------------------------------------------
-- | Primitive for performing expectations in an 'it' block.
expect
  :: (Eq a, Show a)
  => (a -> a -> Bool)
  -> a
  -> a
  -> Test ()
expect f x y = do
  let succeeded = f x y
  name <- use currentTestName
  start <- use currentTestTime
  groupName <- use currentTestGroup
  expects += 1
  currentTestResult %= (&& succeeded)
  when (not succeeded) $ jsm $ do
    stop <- now
    prettyTest (CurrentTest groupName name succeeded expectationMessage (stop - start))
      where
        expectationMessage = mconcat
          [ "Expecting: "
          , yellow
          , ms (show y)
          , "\n"
          , reset
          , "      "
          , cyan <> "↳ " <> reset <> "Received:  "
          , red
          , ms (show x)
          , " \n"
          , reset
          ]
-----------------------------------------------------------------------------
-- | Perform an expectation in an 'it' block.
--
-- The complement of 'shouldBe'.
--
shouldNotBe
  :: (Show a, Eq a)
  => a
  -> a
  -> Test ()
shouldNotBe = expect (/=)
-----------------------------------------------------------------------------
data Expectation
  = Expectation
  { expectation :: Bool
  , expectationName :: MisoString
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Perform an assertion (expect) in an 'it' block.
--
-- Performs an assertion (this calls 'expect')
--
shouldBe
  :: (Show a, Eq a)
  => a
  -> a
  -> Test ()
shouldBe = expect (==)
-----------------------------------------------------------------------------
-- | Execute a JSM action before each 'it' block.
--
-- This is useful for scenarios like clearing the global @Component@ state.
--
before
  :: JSM ()
  -> Test ()
  -> Test ()
before action x = do
  beforeAction %= \f -> f >> action
  x
-----------------------------------------------------------------------------
data Clocked a
  = Clocked
  { time :: Double
  , result :: Either String a
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
clock :: Test a -> Test (Clocked a)
clock action = do
  start <- jsm now
  currentTestTime .= start
  -- dmj: ^ we set current to start here for use w/ expect() failures
  result <- (Right <$> action) `catch`
    (\(e :: SomeException) -> do
        stop <- jsm now
        currentErrorMessage .= ms e
        caughtException .= True
        currentTestResult %= (&& False)
        currentTestTime .= stop - start
        pure $ Left (show e))
  stop <- liftJSM now
  let time = stop - start
  currentTestTime .= time
  pure Clocked {..}
-----------------------------------------------------------------------------
runTests :: Test a -> IO ()
runTests ts = run $ initJSDOM >> do
  summary <- execStateT ts emptyTestState
  printSummary summary
  liftIO $ do
    when (summary ^. failed > 0) exitFailure
    exitSuccess
-----------------------------------------------------------------------------
formatMillis :: Double -> MisoString
formatMillis duration = ms (printf "%.3f ms" duration :: String)
-----------------------------------------------------------------------------
prettyTest :: CurrentTest -> JSM ()
prettyTest CurrentTest {..} = void $
  if successful
    then
      jsg ("console" :: MisoString) # ("log" :: MisoString) $
         [ green
             <> "✓"
         , reset <> ms testGroup
         , ">"
         , white
             <> ms name
         , gray
             <> "["
             <> formatMillis duration
             <> "]"
         ]
    else
      jsg ("console" :: MisoString) # ("log" :: MisoString) $
         [ red <> "✗"
         , reset <> testGroup
         , ">"
         , white
             <> name
             <> gray
             <> " ["
             <> formatMillis duration
             <> "]"
         , cyan
             <> "\n      ↳ "
             <> reset
             <> errorMessage
         ]
-----------------------------------------------------------------------------
printSummary :: TestState -> JSM ()
printSummary TestState {..} = void $
  jsg ("console" :: MisoString) # ("log" :: MisoString) $
    [ "\n  "
        <> green
        <> ms _passed
        <> " passed"
    , "\n  "
        <> red
        <> ms _failed
        <> " failed"
    , "\n  "
        <> reset
        <> "Ran "
        <> ms _total
        <> " tests"
        <> gray
        <> " ["
        <> formatMillis _totalDuration
        <> "]"
    , "\n  "
        <> reset
        <> ms _expects
        <> " expect() calls"
    ]
-----------------------------------------------------------------------------
-- | colors
green, cyan, yellow, red, reset, white, gray :: MisoString
green = "\x1b[32m"
gray = "\x1b[90m"
red = "\x1b[31m"
reset = "\x1b[0m"
yellow = "\x1b[33m"
cyan = "\x1b[36m"
white = "\x1b[37m"
-----------------------------------------------------------------------------
initJSDOM :: JSM ()
initJSDOM = do
  _ <- global # ("initJSDOM" :: String) $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Convenience for calling 'liftJSM'
jsm :: JSM a -> Test a
jsm = liftJSM
-----------------------------------------------------------------------------
-- | Return a random integer between the first two provided (min, max]
--
-- The maximum is exclusive and the minimum is inclusive
--
choose
  :: Int
  -- ^ min
  -> Int
  -- ^ max
  -> JSM Int
choose x y = fromJSValUnchecked =<< do
  global # ("getRandomNumber" :: MisoString) $ (x,y)
-----------------------------------------------------------------------------
