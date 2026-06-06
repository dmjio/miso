-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Date
-- Copyright   :  (C) 2016-2026 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mutable 'Date' data structure in 'IO'.
--
-- A JavaScript [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date). This is a convenience for manipulating JavaScript data structures from Haskell.
--
-- We recommend using this module qualified.
--
-- > import qualified Miso.Date as D
--
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
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate>
--
-- Sets the day of the month.
setDate
  :: Int
  -- ^ Day of the month (1–31)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setDate day (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setDate" [day]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setFullYear>
--
-- Sets the full year, with optional month and day.
setFullYear
  :: Int
  -- ^ Four-digit year
  -> Maybe Int
  -- ^ Optional month (0–11)
  -> Maybe Int
  -- ^ Optional day of the month (1–31)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setFullYear year month day (Date d) = do
  y <- DSL.toJSVal year
  m <- traverse DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setFullYear" (catMaybes [Just y, m, d'])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setHours>
--
-- Sets the hour, with optional minutes, seconds, and milliseconds.
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
  -- ^ Date to mutate
  -> IO Double
setHours hours minutes seconds millis (Date d) = do
  h <- DSL.toJSVal hours
  m <- traverse DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setHours" (catMaybes [Just h, m, s, ms])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMilliseconds>
--
-- Sets the milliseconds.
setMilliseconds
  :: Int
  -- ^ Milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setMilliseconds ms (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setMilliseconds" [ms]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMinutes>
--
-- Sets the minutes, with optional seconds and milliseconds.
setMinutes
  :: Int
  -- ^ Minutes (0–59)
  -> Maybe Int
  -- ^ Optional seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setMinutes minutes seconds millis (Date d) = do
  m <- DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setMinutes" (catMaybes [Just m, s, ms])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMonth>
--
-- Sets the month, with optional day of the month.
setMonth
  :: Int
  -- ^ Month (0–11)
  -> Maybe Int
  -- ^ Optional day of the month (1–31)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setMonth month day (Date d) = do
  m <- DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setMonth" (catMaybes [Just m, d'])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setSeconds>
--
-- Sets the seconds, with optional milliseconds.
setSeconds
  :: Int
  -- ^ Seconds (0–59)
  -> Maybe Int
  -- ^ Optional milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setSeconds seconds millis (Date d) = do
  s <- DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setSeconds" (catMaybes [Just s, ms])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setTime>
--
-- Sets the time in milliseconds since epoch.
setTime
  :: Double
  -- ^ Milliseconds since Unix epoch
  -> Date
  -- ^ Date to mutate
  -> IO Double
setTime time (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setTime" [time]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCDate>
--
-- Sets the UTC day of the month.
setUTCDate
  :: Int
  -- ^ UTC day of the month (1–31)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCDate day (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setUTCDate" [day]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCFullYear>
--
-- Sets the UTC full year, with optional month and day.
setUTCFullYear
  :: Int
  -- ^ Four-digit UTC year
  -> Maybe Int
  -- ^ Optional UTC month (0–11)
  -> Maybe Int
  -- ^ Optional UTC day of the month (1–31)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCFullYear year month day (Date d) = do
  y <- DSL.toJSVal year
  m <- traverse DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setUTCFullYear" (catMaybes [Just y, m, d'])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCHours>
--
-- Sets the UTC hour, with optional minutes, seconds, and milliseconds.
setUTCHours
  :: Int
  -- ^ UTC hour (0–23)
  -> Maybe Int
  -- ^ Optional UTC minutes (0–59)
  -> Maybe Int
  -- ^ Optional UTC seconds (0–59)
  -> Maybe Int
  -- ^ Optional UTC milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCHours hours minutes seconds millis (Date d) = do
  h <- DSL.toJSVal hours
  m <- traverse DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setUTCHours" (catMaybes [Just h, m, s, ms])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMilliseconds>
--
-- Sets the UTC milliseconds.
setUTCMilliseconds
  :: Int
  -- ^ UTC milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCMilliseconds ms (Date d) = DSL.fromJSValUnchecked =<< callFunction d "setUTCMilliseconds" [ms]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMinutes>
--
-- Sets the UTC minutes, with optional seconds and milliseconds.
setUTCMinutes
  :: Int
  -- ^ UTC minutes (0–59)
  -> Maybe Int
  -- ^ Optional UTC seconds (0–59)
  -> Maybe Int
  -- ^ Optional UTC milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCMinutes minutes seconds millis (Date d) = do
  m <- DSL.toJSVal minutes
  s <- traverse DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setUTCMinutes" (catMaybes [Just m, s, ms])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMonth>
--
-- Sets the UTC month, with optional day of the month.
setUTCMonth
  :: Int
  -- ^ UTC month (0–11)
  -> Maybe Int
  -- ^ Optional UTC day of the month (1–31)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCMonth month day (Date d) = do
  m <- DSL.toJSVal month
  d' <- traverse DSL.toJSVal day
  callArgs (Date d) "setUTCMonth" (catMaybes [Just m, d'])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCSeconds>
--
-- Sets the UTC seconds, with optional milliseconds.
setUTCSeconds
  :: Int
  -- ^ UTC seconds (0–59)
  -> Maybe Int
  -- ^ Optional UTC milliseconds (0–999)
  -> Date
  -- ^ Date to mutate
  -> IO Double
setUTCSeconds seconds millis (Date d) = do
  s <- DSL.toJSVal seconds
  ms <- traverse DSL.toJSVal millis
  callArgs (Date d) "setUTCSeconds" (catMaybes [Just s, ms])
-----------------------------------------------------------------------------
