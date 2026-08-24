-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Trace
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Trace" provides functions for tracing values to the browser's
-- developer console, in the spirit of "Debug.Trace" from @base@. Where
-- "Debug.Trace" writes to @stderr@, these functions write to the browser
-- console using
-- <https://developer.mozilla.org/en-US/docs/Web/API/console/log_static console.log>,
-- <https://developer.mozilla.org/en-US/docs/Web/API/console/warn_static console.warn>
-- and
-- <https://developer.mozilla.org/en-US/docs/Web/API/console/error_static console.error>,
-- gaining the browser's affordances such as severity filtering and stack
-- traces.
--
-- The motivation is debugging /pure/ code: places where 'IO' is
-- unavailable or inconvenient, such as a miso application's @view@
-- function or pure helpers called from @update@.
--
-- Like "Debug.Trace", these functions are implemented with
-- 'unsafePerformIO' and are not referentially transparent: they are meant
-- only as a debugging aid and should not be used in production code.
-- Since Haskell is lazily evaluated, a trace fires when (and only when)
-- the traced expression is forced, so messages can appear out of order,
-- once, or not at all.
--
-- = Naming conventions
--
-- The functions follow the naming conventions of "Debug.Trace":
--
-- * @trace*@ functions log with @console.log@, @traceWarn*@ with
--   @console.warn@, and @traceError*@ with @console.error@.
-- * @*Show@ variants accept any 'Show'-able value instead of a string.
-- * @*Id@ variants return the traced value itself.
-- * @*With@ variants trace the result of applying a function to the value.
-- * @*M@ variants trace inside an 'Applicative' (e.g. miso's
--   'Miso.Effect.Effect' monad, or 'IO').
--
-- = See also
--
-- * "Debug.Trace" — the @base@ equivalent, on which this API is modeled
-- * "Miso.FFI" — 'consoleLog', 'consoleWarn', 'consoleError'
----------------------------------------------------------------------------
module Miso.Trace
  ( -- ** Logging (@console.log@)
    trace
  , traceId
  , traceWith
  , traceShow
  , traceShowId
  , traceShowWith
  , traceM
  , traceShowM
    -- ** Errors (@console.error@)
  , traceError
  , traceErrorId
  , traceErrorWith
  , traceErrorShow
  , traceErrorShowId
  , traceErrorShowWith
  , traceErrorM
  , traceErrorShowM
    -- ** Warnings (@console.warn@)
  , traceWarn
  , traceWarnId
  , traceWarnWith
  , traceWarnShow
  , traceWarnShowId
  , traceWarnShowWith
  , traceWarnM
  , traceWarnShowM
    -- ** Generalized tracing
  , traceTo
  ) where
-----------------------------------------------------------------------------
import           System.IO.Unsafe (unsafePerformIO)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.FFI
import           Miso.String
-----------------------------------------------------------------------------
-- | Outputs a message to the browser console with @console.log@ when the
-- result is forced, then returns the second argument. The browser
-- analogue of 'Debug.Trace.trace'.
trace
  :: ToMisoString s
  => s
  -- ^ Message to log
  -> a
  -- ^ Value to return
  -> a
trace = traceTo consoleLog
-----------------------------------------------------------------------------
-- | Like 'trace', but returns the message itself:
-- @'traceId' x = 'trace' x x@.
traceId :: ToMisoString s => s -> s
traceId = traceWith id
-----------------------------------------------------------------------------
-- | Traces the result of applying a function to a value, then returns the
-- original value. Useful for logging a projection of a larger structure
-- while leaving the structure untouched.
traceWith
  :: ToMisoString s
  => (a -> s)
  -- ^ Function producing the message from the value
  -> a
  -- ^ Value to trace and return
  -> a
traceWith f a = trace (f a) a
-----------------------------------------------------------------------------
-- | Like 'trace', but accepts any 'Show'-able value as the message. The
-- browser analogue of 'Debug.Trace.traceShow'.
traceShow
  :: Show a
  => a
  -- ^ Value to log
  -> b
  -- ^ Value to return
  -> b
traceShow = trace . show
-----------------------------------------------------------------------------
-- | Shows and traces a value, then returns it. Convenient to wrap around
-- any sub-expression you want to inspect without restructuring the code.
traceShowId :: Show a => a -> a
traceShowId = traceWith show
-----------------------------------------------------------------------------
-- | Traces the 'show'-n result of applying a function to a value, then
-- returns the original value.
traceShowWith
  :: Show b
  => (a -> b)
  -- ^ Function producing the value to show from the value
  -> a
  -- ^ Value to trace and return
  -> a
traceShowWith f = traceWith (show . f)
-----------------------------------------------------------------------------
-- | Traces a message in an 'Applicative' context, such as miso's
-- 'Miso.Effect.Effect' monad or 'IO'. The browser analogue of
-- 'Debug.Trace.traceM'.
traceM :: (ToMisoString s, Applicative f) => s -> f ()
traceM s = trace s $ pure ()
-----------------------------------------------------------------------------
-- | Like 'traceM', but accepts any 'Show'-able value. Useful for logging
-- every action that flows through an update function.
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = traceM . show
-----------------------------------------------------------------------------
-- | Like 'trace', but logs with @console.error@, which browsers render
-- prominently (typically in red, with an expandable stack trace).
traceError
  :: ToMisoString s
  => s
  -- ^ Message to log
  -> a
  -- ^ Value to return
  -> a
traceError = traceTo consoleError
-----------------------------------------------------------------------------
-- | Like 'traceId', but logs with @console.error@.
traceErrorId :: ToMisoString s => s -> s
traceErrorId = traceErrorWith id
-----------------------------------------------------------------------------
-- | Like 'traceWith', but logs with @console.error@.
traceErrorWith
  :: ToMisoString s
  => (a -> s)
  -- ^ Function producing the message from the value
  -> a
  -- ^ Value to trace and return
  -> a
traceErrorWith f a = traceError (f a) a
-----------------------------------------------------------------------------
-- | Like 'traceShow', but logs with @console.error@.
traceErrorShow
  :: Show a
  => a
  -- ^ Value to log
  -> b
  -- ^ Value to return
  -> b
traceErrorShow = traceError . show
-----------------------------------------------------------------------------
-- | Like 'traceShowId', but logs with @console.error@.
traceErrorShowId :: Show a => a -> a
traceErrorShowId = traceErrorWith show
-----------------------------------------------------------------------------
-- | Like 'traceShowWith', but logs with @console.error@.
traceErrorShowWith
  :: Show b
  => (a -> b)
  -- ^ Function producing the value to show from the value
  -> a
  -- ^ Value to trace and return
  -> a
traceErrorShowWith f = traceErrorWith (show . f)
-----------------------------------------------------------------------------
-- | Like 'traceM', but logs with @console.error@.
traceErrorM :: (ToMisoString s, Applicative f) => s -> f ()
traceErrorM s = traceError s $ pure ()
-----------------------------------------------------------------------------
-- | Like 'traceShowM', but logs with @console.error@.
traceErrorShowM :: (Show a, Applicative f) => a -> f ()
traceErrorShowM = traceErrorM . show
-----------------------------------------------------------------------------
-- | Like 'trace', but logs with @console.warn@, which browsers render as
-- a warning (typically in yellow) and can be filtered by severity.
traceWarn
  :: ToMisoString s
  => s
  -- ^ Message to log
  -> a
  -- ^ Value to return
  -> a
traceWarn = traceTo consoleWarn
-----------------------------------------------------------------------------
-- | Like 'traceId', but logs with @console.warn@.
traceWarnId :: ToMisoString s => s -> s
traceWarnId = traceWarnWith id
-----------------------------------------------------------------------------
-- | Like 'traceWith', but logs with @console.warn@.
traceWarnWith
  :: ToMisoString s
  => (a -> s)
  -- ^ Function producing the message from the value
  -> a
  -- ^ Value to trace and return
  -> a
traceWarnWith f a = traceWarn (f a) a
-----------------------------------------------------------------------------
-- | Like 'traceShow', but logs with @console.warn@.
traceWarnShow
  :: Show a
  => a
  -- ^ Value to log
  -> b
  -- ^ Value to return
  -> b
traceWarnShow = traceWarn . show
-----------------------------------------------------------------------------
-- | Like 'traceShowId', but logs with @console.warn@.
traceWarnShowId :: Show a => a -> a
traceWarnShowId = traceWarnWith show
-----------------------------------------------------------------------------
-- | Like 'traceShowWith', but logs with @console.warn@.
traceWarnShowWith
  :: Show b
  => (a -> b)
  -- ^ Function producing the value to show from the value
  -> a
  -- ^ Value to trace and return
  -> a
traceWarnShowWith f = traceWarnWith (show . f)
-----------------------------------------------------------------------------
-- | Like 'traceM', but logs with @console.warn@.
traceWarnM :: (ToMisoString s, Applicative f) => s -> f ()
traceWarnM s = traceWarn s $ pure ()
-----------------------------------------------------------------------------
-- | Like 'traceShowM', but logs with @console.warn@.
traceWarnShowM :: (Show a, Applicative f) => a -> f ()
traceWarnShowM = traceWarnM . show
-----------------------------------------------------------------------------
-- | The generalized tracing combinator underlying this module: traces via
-- the given console function from "Miso.FFI". Every other function here
-- is defined in terms of it.
traceTo
  :: ToMisoString s
  => (MisoString -> IO ())
  -- ^ Console function to log with, e.g. 'consoleLog'
  -> s
  -- ^ Message to log
  -> a
  -- ^ Value to return
  -> a
traceTo f s a = unsafePerformIO $ do
  f (toMisoString s)
  pure a
-----------------------------------------------------------------------------
