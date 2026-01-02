-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Test
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- An hspec-like [miso](https://github.com/dmjio/miso) testing framework. Meant for testing @miso@ @Component@.
-- The testing framework operates in the t'IO' monad and has access
-- to the DOM courtesy of [Playwright](https://playwright.dev/).
--
-- @
--
-- main :: IO ()
-- main = runTests $ do
--   describe "Arithmetic tests" $ do
--     it "2 + 2 = 4" $ do
--       (2 + 2) \`shouldBe\` 4
-- @
--
----------------------------------------------------------------------------
module Miso.Test
  ( -- * Test Combinators
    describe
  , it
  , expect
  , beforeEach
  , afterEach
  , shouldBe
  , shouldSatisfy
  , shouldNotBe
  , runTests
  -- * Utils
  , choose
  -- * Types
  , Test
  , TestState
  ) where
-----------------------------------------------------------------------------
import           Control.Exception (IOException)
import           Control.Monad.Except (catchError)
import           Text.Printf
import           Control.Monad.State
import           Control.Monad
import           System.Exit
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
-----------------------------------------------------------------------------
-- | Used to group a bunch of expectations using 'it'. Testing out
-- will include the test description in its output.
describe
  :: MisoString
  -- ^ Description of test group
  -> Test ()
  -- ^ Group of tests to run
  -> Test ()
describe name tests = do
  currentTestGroup .= name
  tests
-----------------------------------------------------------------------------
-- | Used to make multiple expectations using 'shouldBe' / 'shouldNotBe'.
--
it
  :: MisoString
  -- ^ Name of test to execute
  -> Test ()
  -- ^ Test holding multiple expectations
  -> Test ()
it name action = do
  preamble <- use beforeAction
  liftIO preamble
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
    liftIO $ prettyTest CurrentTest
      { duration = time
      , ..
      }
  conclusion <- use afterAction
  liftIO conclusion
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
-- | The monad that executes tests
type Test a = StateT TestState IO a
-----------------------------------------------------------------------------
-- | Internal type for managing test state
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
  , _beforeAction :: IO ()
  , _afterAction :: IO ()
  , _caughtException :: Bool
  }
-----------------------------------------------------------------------------
emptyTestState :: TestState
emptyTestState = TestState mempty mempty mempty 0 0 0 0 0 0 True (pure ()) (pure ()) False
-----------------------------------------------------------------------------
beforeAction :: Lens TestState (IO ())
beforeAction = lens _beforeAction $ \r x -> r { _beforeAction = x }
-----------------------------------------------------------------------------
afterAction :: Lens TestState (IO ())
afterAction = lens _afterAction $ \r x -> r { _afterAction = x }
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
  when (not succeeded) $ liftIO $ do
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
-- | Primitive for performing expectations in an 'it' block.
shouldSatisfy
  :: (Eq a, Show a)
  => a
  -> (a -> Bool)
  -> Test ()
shouldSatisfy x f = do
  let succeeded = f x
  name <- use currentTestName
  start <- use currentTestTime
  groupName <- use currentTestGroup
  expects += 1
  currentTestResult %= (&& succeeded)
  when (not succeeded) $ liftIO $ do
    stop <- now
    prettyTest (CurrentTest groupName name succeeded expectationMessage (stop - start))
      where
        expectationMessage = mconcat
          [ "Expecting: "
          , yellow
          , ms (show x)
          , " to satisfy predicate"
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
-- | Performs an expectation in an 'it' block.
--
shouldBe
  :: (Show a, Eq a)
  => a
  -> a
  -> Test ()
shouldBe = expect (==)
-----------------------------------------------------------------------------
-- | Execute a t'IO' action before each 'it' block.
--
-- This is useful for scenarios like clearing the global t'Component' state.
--
beforeEach
  :: IO ()
  -> Test ()
  -> Test ()
beforeEach action x = do
  beforeAction %= \f -> f >> action
  x
-----------------------------------------------------------------------------
-- | Execute a t'IO' after each 'it' block.
--
-- This is useful for scenarios like clearing the global t'Component' state.
--
afterEach
  :: IO ()
  -> Test ()
  -> Test ()
afterEach action x = do
  afterAction %= \f -> f >> action
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
  start <- liftIO now
  currentTestTime .= start
  -- dmj: ^ we set current to start here for use w/ expect() failures
  result <- (Right <$> action) `catchError` errorHandler start
  stop <- liftIO now
  let time = stop - start
  currentTestTime .= time
  pure Clocked {..}
    where
      errorHandler start (e :: IOException) = do
        stop <- liftIO now
        currentErrorMessage .= ms e
        caughtException .= True
        currentTestResult %= (&& False)
        currentTestTime .= stop - start
        pure $ Left (show e)
-----------------------------------------------------------------------------
-- | Executes a block of tests in 'describe' blocks.
runTests :: Test a -> IO ()
runTests ts = do
#ifdef JSDOM
  _ <- global # ("initJSDOM" :: String) $ ()
#endif
  summary <- execStateT ts emptyTestState
  printSummary summary
  when (summary ^. failed > 0) $ do
    consoleLog "ERROR"
    liftIO exitFailure
  consoleLog "SUCCESS"
  liftIO exitSuccess
-----------------------------------------------------------------------------
formatMillis :: Double -> MisoString
formatMillis duration = ms (printf "%.3f ms" duration :: String)
-----------------------------------------------------------------------------
prettyTest :: CurrentTest -> IO ()
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
printSummary :: TestState -> IO ()
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
-- | Return a random integer between the first two provided [min, max)
--
-- The maximum is exclusive and the minimum is inclusive
--
choose
  :: Int
  -- ^ min
  -> Int
  -- ^ max
  -> IO Int
choose x y = fromJSValUnchecked =<< do
  global # "getRandomNumber" $ (x,y)
-----------------------------------------------------------------------------
