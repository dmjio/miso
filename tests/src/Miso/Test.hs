-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Miso.Test
-- Copyright   : (C) 2016-2026 David M. Johnson
-- License     : BSD3-style (see the file LICENSE)
-- Maintainer  : David M. Johnson <code@dmj.io>
-- Stability   : experimental
-- Portability : non-portable
--
-- = Overview
--
-- "Miso.Test" is a lightweight hspec-style testing framework for miso
-- 'Miso.Types.Component' integration tests. It runs in the browser (or a
-- JSDOM environment) via
-- <https://playwright.dev/ Playwright>, giving each test full access to
-- the live DOM.
--
-- Tests are written as @'StateT' 'TestState' IO@, which means they can call
-- any 'IO' action — including miso DOM operations and 'Miso.FFI' calls — directly.
-- Each 'it' block is timed and its result is printed with coloured output.
-- After all tests run, 'runTests' prints a summary and calls
-- @'System.Exit.exitSuccess'@ or @'System.Exit.exitFailure'@ so the process
-- exit code reflects the test outcome.
--
-- = Quick start
--
-- @
-- import "Miso.Test"
-- import "Miso.Runtime.Internal" (components)
-- import Data.IORef (writeIORef)
-- import qualified Data.IntMap.Strict as IM
--
-- main :: IO ()
-- main = 'runTests' $ do
--   'describe' \"Arithmetic\" $ do
--     'it' \"2 + 2 = 4\" $
--       (2 + 2) \`'shouldBe'\` (4 :: Int)
--     'it' \"3 /= 4\" $
--       (3 :: Int) \`'shouldNotBe'\` 4
--
--   'describe' \"Component\" $
--     'beforeEach' (writeIORef components IM.empty) $ do
--       'it' \"mounts without error\" $ do
--         -- mount a component and assert on DOM state
--         pure ()
-- @
--
-- = Combinators
--
-- * 'describe' — group related tests under a label.
-- * 'it' — declare a single named test; times it and records pass\/fail.
-- * 'beforeEach' — run an 'IO' action before every 'it' in the wrapped block.
-- * 'afterEach' — run an 'IO' action after every 'it' in the wrapped block.
--
-- = Assertion primitives
--
-- * @x \`'shouldBe'\` y@ — assert @x == y@; pretty-prints a diff on failure.
-- * @x \`'shouldNotBe'\` y@ — assert @x \/= y@.
-- * @x \`'shouldSatisfy'\` p@ — assert that the predicate @p x@ holds.
-- * @'expect' f x y@ — assert @f x y@; the most general form.
--
-- Multiple assertions within a single 'it' block are all evaluated; the
-- block is marked as failed if any assertion fails.
--
-- = Running tests
--
-- @'runTests' :: 'Test' a -> IO ()@ is the entry point. It executes the
-- test tree, prints per-test results and a final summary (pass count,
-- fail count, total tests, total duration, and @expect()@ call count),
-- then exits:
--
-- * @'System.Exit.exitSuccess'@ — all tests passed.
-- * @'System.Exit.exitFailure'@ — one or more tests failed.
--
-- When compiled with @-DJSDOM@, 'runTests' additionally calls
-- @globalThis.initJSDOM()@ before running any tests, enabling headless
-- DOM testing outside a real browser.
--
-- = Utilities
--
-- * 'choose' @min max@ — returns a uniformly random @Int@ in @[min, max)@
--   using the JS @getRandomNumber@ global. Useful for property-style tests
--   that need random inputs.
--
-- = Types
--
-- * @'Test' a = 'Control.Monad.State.StateT' 'TestState' IO a@ — the test monad.
-- * 'TestState' — internal state tracking per-test timing, pass\/fail counts,
--   before\/after hooks, and error messages.
--
-- = See also
--
-- * "Miso.Runtime.Internal" — 'Miso.Runtime.Internal.components' IORef for resetting state between tests
-- * "Miso.Random" — 'Miso.Random.newStdGen', 'Miso.Random.replicateRM' for random values
-- * "Miso" — the component and effect API under test
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
  -- ^ Name of the enclosing 'describe' group
  , name :: MisoString
  -- ^ Name of the 'it' test case
  , successful :: Bool
  -- ^ 'True' if all assertions in the test passed
  , errorMessage :: MisoString
  -- ^ Human-readable failure message, empty on success
  , duration :: Double
  -- ^ Wall-clock execution time in milliseconds
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The monad that executes tests
type Test a = StateT TestState IO a
-----------------------------------------------------------------------------
-- | Internal type for managing test state
data TestState
  = TestState
  { _currentTestGroup :: MisoString
  -- ^ Label of the current 'describe' group
  , _currentErrorMessage :: MisoString
  -- ^ Accumulated failure message for the current 'it' block
  , _currentTestName :: MisoString
  -- ^ Name of the currently executing 'it' block
  , _currentTestTime :: Double
  -- ^ Start time (or elapsed time) of the current 'it' block in milliseconds
  , _expects :: Int
  -- ^ Total number of 'expect' calls made across all tests
  , _failed :: Int
  -- ^ Count of 'it' blocks that had at least one failing assertion
  , _passed :: Int
  -- ^ Count of 'it' blocks where all assertions passed
  , _total :: Int
  -- ^ Total number of 'it' blocks registered
  , _totalDuration :: Double
  -- ^ Cumulative wall-clock time for all 'it' blocks in milliseconds
  , _currentTestResult :: Bool
  -- ^ 'True' while no assertion has failed in the current 'it' block
  , _beforeAction :: IO ()
  -- ^ Action run before each 'it' block (registered via 'beforeEach')
  , _afterAction :: IO ()
  -- ^ Action run after each 'it' block (registered via 'afterEach')
  , _caughtException :: Bool
  -- ^ 'True' if the current 'it' block threw an 'Control.Exception.IOException'
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
  -- ^ Comparison predicate; the test passes when this returns 'True'
  -> a
  -- ^ Actual value (shown in failure output as "Received")
  -> a
  -- ^ Expected value (shown in failure output as "Expecting")
  -> Test ()
expect f x y = do
  let succeeded = f x y
  name <- use currentTestName
  start <- use currentTestTime
  groupName <- use currentTestGroup
  expects += 1
  currentTestResult %= (&& succeeded)
  unless succeeded $ liftIO $ do
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
  -- ^ Actual value to test
  -> (a -> Bool)
  -- ^ Predicate that must hold for the assertion to pass
  -> Test ()
shouldSatisfy x f = do
  let succeeded = f x
  name <- use currentTestName
  start <- use currentTestTime
  groupName <- use currentTestGroup
  expects += 1
  currentTestResult %= (&& succeeded)
  unless succeeded $ liftIO $ do
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
  -- ^ Actual value
  -> a
  -- ^ Value it must NOT equal
  -> Test ()
shouldNotBe = expect (/=)
-----------------------------------------------------------------------------
-- | Performs an expectation in an 'it' block.
--
shouldBe
  :: (Show a, Eq a)
  => a
  -- ^ Actual value
  -> a
  -- ^ Expected value it must equal
  -> Test ()
shouldBe = expect (==)
-----------------------------------------------------------------------------
-- | Execute a t'IO' action before each 'it' block.
--
-- This is useful for scenarios like clearing the global t'Component' state.
--
beforeEach
  :: IO ()
  -- ^ Action to run before each 'it' block in the wrapped group
  -> Test ()
  -- ^ Test group to wrap
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
  -- ^ Action to run after each 'it' block in the wrapped group
  -> Test ()
  -- ^ Test group to wrap
  -> Test ()
afterEach action x = do
  afterAction %= \f -> f >> action
  x
-----------------------------------------------------------------------------
data Clocked a
  = Clocked
  { time :: Double
  -- ^ Wall-clock duration in milliseconds
  , result :: Either String a
  -- ^ @'Left' errMsg@ if an exception was caught; @'Right' a@ on success
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
runTests
  :: Test a
  -- ^ The test tree to execute (built from 'describe' \/ 'it' blocks)
  -> IO ()
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
