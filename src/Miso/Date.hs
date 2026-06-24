-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Date
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Date" is a Haskell wrapper around the JavaScript
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date Date>
-- object. A 'Date' value lives in JavaScript memory and represents a single
-- point in time. All operations run in 'IO' and call through to the
-- underlying JS object.
--
-- Import qualified to avoid clashing with 'Prelude':
--
-- @
-- import qualified "Miso.Date" as D
-- @
--
-- = Quick start
--
-- @
-- import qualified "Miso.Date" as D
--
-- example :: IO ()
-- example = do
--   now  <- D.'new'
--   iso  <- D.'toISOString' now     -- e.g. \"2026-06-23T12:00:00.000Z\"
--   year <- D.'getFullYear' now     -- e.g. 2026
--   ms   <- D.'valueOf' now         -- milliseconds since Unix epoch
--   pure ()
-- @
--
-- = API groups
--
-- * __Construction__: 'new'
-- * __Conversion__ (strings): 'toDateString', 'toISOString', 'toJSON',
--   'toLocaleDateString', 'toLocaleString', 'toLocaleTimeString',
--   'toString', 'toTimeString', 'toUTCString'
-- * __Conversion__ (numeric): 'valueOf' (ms since epoch)
-- * __Local getters__: 'getDate', 'getDay', 'getFullYear', 'getHours',
--   'getMilliseconds', 'getMinutes', 'getMonth', 'getSeconds',
--   'getTime', 'getTimezoneOffset'
-- * __UTC getters__: 'getUTCDate', 'getUTCDay', 'getUTCFullYear',
--   'getUTCHours', 'getUTCMilliseconds', 'getUTCMinutes',
--   'getUTCMonth', 'getUTCSeconds'
-- * __Local setters__: 'setDate', 'setFullYear', 'setHours',
--   'setMilliseconds', 'setMinutes', 'setMonth', 'setSeconds', 'setTime'
-- * __UTC setters__: 'setUTCDate', 'setUTCFullYear', 'setUTCHours',
--   'setUTCMilliseconds', 'setUTCMinutes', 'setUTCMonth', 'setUTCSeconds'
--
-- __Note__: JavaScript months are __0-indexed__ (January = 0, December = 11).
-- All getter and setter functions in this module follow that convention.
-- Setter functions return the new timestamp as milliseconds since the Unix
-- epoch (a 'Double'), mirroring the JavaScript return value.
--
-- = See also
--
-- * "Miso.DSL" — 'Miso.DSL.JSVal' and marshaling used internally
-----------------------------------------------------------------------------
module Miso.Date
  ( -- * Type
    Date
    -- * Construction
  , new
    -- * Conversion
  , toDateString
  , toISOString
  , toJSON
  , toLocaleDateString
  , toLocaleString
  , toLocaleTimeString
  , toString
  , toTimeString
  , toUTCString
  , valueOf
    -- * Getters
  , getDate
  , getDay
  , getFullYear
  , getHours
  , getMilliseconds
  , getMinutes
  , getMonth
  , getSeconds
  , getTime
  , getTimezoneOffset
  , getUTCDate
  , getUTCDay
  , getUTCFullYear
  , getUTCHours
  , getUTCMilliseconds
  , getUTCMinutes
  , getUTCMonth
  , getUTCSeconds
    -- * Setters
  , setDate
  , setFullYear
  , setHours
  , setMilliseconds
  , setMinutes
  , setMonth
  , setSeconds
  , setTime
  , setUTCDate
  , setUTCFullYear
  , setUTCHours
  , setUTCMilliseconds
  , setUTCMinutes
  , setUTCMonth
  , setUTCSeconds
  ) where
-----------------------------------------------------------------------------
import           Data.Maybe (catMaybes)
-----------------------------------------------------------------------------
import           Miso.DSL (jsg, JSVal, ToJSVal, FromJSVal, ToObject)
import qualified Miso.DSL as DSL
import           Miso.FFI (callFunction)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
newtype Date = Date JSVal deriving (FromJSVal, ToJSVal, ToObject, Eq)
-----------------------------------------------------------------------------
-- | Constructs a new JS [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) in t'IO'.
--
new :: IO Date
new = Date <$> DSL.new (jsg "Date") ([] :: [JSVal])
-----------------------------------------------------------------------------
call0 :: FromJSVal a => Date -> MisoString -> IO a
call0 (Date d) name = DSL.fromJSValUnchecked =<< callFunction d name ([] :: [JSVal])
-----------------------------------------------------------------------------
callArgs :: FromJSVal a => Date -> MisoString -> [JSVal] -> IO a
callArgs (Date d) name args = DSL.fromJSValUnchecked =<< callFunction d name args
-----------------------------------------------------------------------------
-- | Returns a human-readable date string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString>
--
toDateString :: Date -> IO MisoString
toDateString date = call0 date "toDateString"
-----------------------------------------------------------------------------
-- | Returns an ISO 8601 string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString>
--
toISOString :: Date -> IO MisoString
toISOString date = call0 date "toISOString"
-----------------------------------------------------------------------------
-- | Returns the JSON representation of the date.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toJSON>
--
toJSON :: Date -> IO MisoString
toJSON date = call0 date "toJSON"
-----------------------------------------------------------------------------
-- | Returns a locale-sensitive date string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString>
--
toLocaleDateString :: Date -> IO MisoString
toLocaleDateString date = call0 date "toLocaleDateString"
-----------------------------------------------------------------------------
-- | Returns a locale-sensitive date and time string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString>
--
toLocaleString :: Date -> IO MisoString
toLocaleString date = call0 date "toLocaleString"
-----------------------------------------------------------------------------
-- | Returns a locale-sensitive time string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString>
--
toLocaleTimeString :: Date -> IO MisoString
toLocaleTimeString date = call0 date "toLocaleTimeString"
-----------------------------------------------------------------------------
-- | Returns the full date string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString>
--
toString :: Date -> IO MisoString
toString date = call0 date "toString"
-----------------------------------------------------------------------------
-- | Returns a time string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toTimeString>
--
toTimeString :: Date -> IO MisoString
toTimeString date = call0 date "toTimeString"
-----------------------------------------------------------------------------
-- | Returns a UTC string.
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toUTCString>
--
toUTCString :: Date -> IO MisoString
toUTCString date = call0 date "toUTCString"
-----------------------------------------------------------------------------
-- | Returns the primitive value (milliseconds since epoch).
--
-- <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/valueOf>
--
valueOf :: Date -> IO Double
valueOf date = call0 date "valueOf"
-----------------------------------------------------------------------------
-- | Returns the day of the month.
--
getDate :: Date -> IO Int
getDate date = call0 date "getDate"
-----------------------------------------------------------------------------
-- | Returns the day of the week.
--
getDay :: Date -> IO Int
getDay date = call0 date "getDay"
-----------------------------------------------------------------------------
-- | Returns the full year.
--
getFullYear :: Date -> IO Int
getFullYear date = call0 date "getFullYear"
-----------------------------------------------------------------------------
-- | Returns the hour.
--
getHours :: Date -> IO Int
getHours date = call0 date "getHours"
-----------------------------------------------------------------------------
-- | Returns the milliseconds.
--
getMilliseconds :: Date -> IO Int
getMilliseconds date = call0 date "getMilliseconds"
-----------------------------------------------------------------------------
-- | Returns the minutes.
--
getMinutes :: Date -> IO Int
getMinutes date = call0 date "getMinutes"
-----------------------------------------------------------------------------
-- | Returns the month (0-11).
--
getMonth :: Date -> IO Int
getMonth date = call0 date "getMonth"
-----------------------------------------------------------------------------
-- | Returns the seconds.
--
getSeconds :: Date -> IO Int
getSeconds date = call0 date "getSeconds"
-----------------------------------------------------------------------------
-- | Returns milliseconds since epoch.
--
getTime :: Date -> IO Double
getTime date = call0 date "getTime"
-----------------------------------------------------------------------------
-- | Returns the time zone offset in minutes.
--
getTimezoneOffset :: Date -> IO Int
getTimezoneOffset date = call0 date "getTimezoneOffset"
-----------------------------------------------------------------------------
-- | Returns the UTC day of the month.
--
getUTCDate :: Date -> IO Int
getUTCDate date = call0 date "getUTCDate"
-----------------------------------------------------------------------------
-- | Returns the UTC day of the week.
--
getUTCDay :: Date -> IO Int
getUTCDay date = call0 date "getUTCDay"
-----------------------------------------------------------------------------
-- | Returns the UTC full year.
--
getUTCFullYear :: Date -> IO Int
getUTCFullYear date = call0 date "getUTCFullYear"
-----------------------------------------------------------------------------
-- | Returns the UTC hour.
--
getUTCHours :: Date -> IO Int
getUTCHours date = call0 date "getUTCHours"
-----------------------------------------------------------------------------
-- | Returns the UTC milliseconds.
--
getUTCMilliseconds :: Date -> IO Int
getUTCMilliseconds date = call0 date "getUTCMilliseconds"
-----------------------------------------------------------------------------
-- | Returns the UTC minutes.
--
getUTCMinutes :: Date -> IO Int
getUTCMinutes date = call0 date "getUTCMinutes"
-----------------------------------------------------------------------------
-- | Returns the UTC month (0-11).
--
getUTCMonth :: Date -> IO Int
getUTCMonth date = call0 date "getUTCMonth"
-----------------------------------------------------------------------------
-- | Returns the UTC seconds.
--
getUTCSeconds :: Date -> IO Int
getUTCSeconds date = call0 date "getUTCSeconds"
-----------------------------------------------------------------------------
-- | Sets the day of the month.
--
setDate
  :: Int
  -- ^ Day of the month (1–31)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setDate day (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setDate" [day]
-----------------------------------------------------------------------------
-- | Sets the full year, with optional month and day.
--
setFullYear
  :: Int
  -- ^ Full year (e.g. 2026)
  -> Maybe Int
  -- ^ Optional month (0-indexed: 0 = January)
  -> Maybe Int
  -- ^ Optional day of the month (1–31)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setFullYear year month day (Date d) = do
  y <- DSL.toJSVal year
  m <- traverse DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setFullYear" (catMaybes [Just y, m, d'])
-----------------------------------------------------------------------------
-- | Sets the hour, with optional minutes, seconds, and milliseconds.
--
setHours
  :: Int
  -- ^ Hour (0–23)
  -> Maybe Int
  -- ^ Optional minutes (0–59)
  -> Maybe Int
  -- ^ Optional seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setHours hours minutes seconds millis (Date d) = do
  h <- DSL.toJSVal hours
  m <- traverse DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setHours" (catMaybes [Just h, m, s, ms])
-----------------------------------------------------------------------------
-- | Sets the milliseconds.
--
setMilliseconds
  :: Int
  -- ^ Milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setMilliseconds ms (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setMilliseconds" [ms]
-----------------------------------------------------------------------------
-- | Sets the minutes, with optional seconds and milliseconds.
--
setMinutes
  :: Int
  -- ^ Minutes (0–59)
  -> Maybe Int
  -- ^ Optional seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setMinutes minutes seconds millis (Date d) = do
  m <- DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setMinutes" (catMaybes [Just m, s, ms])
-----------------------------------------------------------------------------
-- | Sets the month, with optional day of the month.
--
setMonth
  :: Int
  -- ^ Month (0-indexed: 0 = January, 11 = December)
  -> Maybe Int
  -- ^ Optional day of the month (1–31)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setMonth month day (Date d) = do
  m <- DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setMonth" (catMaybes [Just m, d'])
-----------------------------------------------------------------------------
-- | Sets the seconds, with optional milliseconds.
--
setSeconds
  :: Int
  -- ^ Seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setSeconds seconds millis (Date d) = do
  s <- DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setSeconds" (catMaybes [Just s, ms])
-----------------------------------------------------------------------------
-- | Sets the time in milliseconds since epoch.
--
setTime
  :: Double
  -- ^ Milliseconds since the Unix epoch (1 January 1970 00:00:00 UTC)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setTime time (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setTime" [time]
-----------------------------------------------------------------------------
-- | Sets the UTC day of the month.
--
setUTCDate
  :: Int
  -- ^ Day of the month in UTC (1–31)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCDate day (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setUTCDate" [day]
-----------------------------------------------------------------------------
-- | Sets the UTC full year, with optional month and day.
--
setUTCFullYear
  :: Int
  -- ^ Full year in UTC (e.g. 2026)
  -> Maybe Int
  -- ^ Optional month in UTC (0-indexed)
  -> Maybe Int
  -- ^ Optional day in UTC (1–31)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCFullYear year month day (Date d) = do
  y <- DSL.toJSVal year
  m <- traverse DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setUTCFullYear" (catMaybes [Just y, m, d'])
-----------------------------------------------------------------------------
-- | Sets the UTC hour, with optional minutes, seconds, and milliseconds.
--
setUTCHours
  :: Int
  -- ^ Hour in UTC (0–23)
  -> Maybe Int
  -- ^ Optional minutes (0–59)
  -> Maybe Int
  -- ^ Optional seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCHours hours minutes seconds millis (Date d) = do
  h <- DSL.toJSVal hours
  m <- traverse DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setUTCHours" (catMaybes [Just h, m, s, ms])
-----------------------------------------------------------------------------
-- | Sets the UTC milliseconds.
--
setUTCMilliseconds
  :: Int
  -- ^ Milliseconds in UTC (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCMilliseconds ms (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setUTCMilliseconds" [ms]
-----------------------------------------------------------------------------
-- | Sets the UTC minutes, with optional seconds and milliseconds.
--
setUTCMinutes
  :: Int
  -- ^ Minutes in UTC (0–59)
  -> Maybe Int
  -- ^ Optional seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCMinutes minutes seconds millis (Date d) = do
  m <- DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setUTCMinutes" (catMaybes [Just m, s, ms])
-----------------------------------------------------------------------------
-- | Sets the UTC month, with optional day of the month.
--
setUTCMonth
  :: Int
  -- ^ Month in UTC (0-indexed: 0 = January)
  -> Maybe Int
  -- ^ Optional day in UTC (1–31)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCMonth month day (Date d) = do
  m <- DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setUTCMonth" (catMaybes [Just m, d'])
-----------------------------------------------------------------------------
-- | Sets the UTC seconds, with optional milliseconds.
--
setUTCSeconds
  :: Int
  -- ^ Seconds in UTC (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date object to modify
  -> IO Double
setUTCSeconds seconds millis (Date d) = do
  s <- DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setUTCSeconds" (catMaybes [Just s, ms])
-----------------------------------------------------------------------------
