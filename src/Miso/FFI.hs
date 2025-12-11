-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Foreign Function Interface (FFI) utilities for interacting with JavaScript.
--
----------------------------------------------------------------------------
module Miso.FFI
  ( -- ** Object
    set
    -- ** Performance
  , now
    -- ** Logging
  , consoleLog
  , consoleLog'
  , consoleError
  , consoleWarn
    -- ** DOM
  , getElementById
  , focus
  , blur
  , select
  , setSelectionRange
  , alert
  , getProperty
  , callFunction
  , castJSVal
    -- ** Styles
  , addStyle
  , addStyleSheet
    -- ** Callbacks
  , syncCallback
  , syncCallback1
  , asyncCallback
  , asyncCallback1
  , asyncCallback2
    -- ** Drawing
  , flush
  , setDrawingContext
    -- ** JSON
  , jsonStringify
  , jsonParse
    -- ** Window
  , windowInnerWidth
  , windowInnerHeight
  , reload
    -- ** Image
  , Image (..)
  , newImage
    -- ** Date
  , Date (..)
  , newDate
  , toLocaleString
  , getSeconds
  , getMilliseconds
    -- ** 'Miso.Types.Component'
  , getParentComponentId
  , getComponentId
    -- ** DOM Traversal
  , nextSibling
  , previousSibling
    -- ** Element
  , click
    -- ** File Input
  , files
    -- ** Navigator
  , isOnLine
    -- ** ArrayBuffer
  , ArrayBuffer (..)
    -- ** Blob
  , Blob (..)
    -- ** Uint8Array
  , Uint8Array (..)
    -- ** FormData
  , FormData (..)
    -- ** File
  , File (..)
    -- ** URLSearchParams
  , URLSearchParams (..)
    -- ** FileReader
  , FileReader (..)
  , newFileReader
    -- ** Fetch API
  , fetch
  , Response (..)
    -- ** Event
  , addEventListener
  , removeEventListener
  , dispatchEvent
  , newEvent
  , newCustomEvent
  , Event (..)
  , eventPreventDefault
  , eventStopPropagation
  ) where
-----------------------------------------------------------------------------
import           Miso.FFI.Internal
-----------------------------------------------------------------------------
