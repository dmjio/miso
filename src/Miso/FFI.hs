-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Public re-export surface for Miso's JavaScript FFI layer.
--
-- This module re-exports the stable public API from "Miso.FFI.Internal".
-- Prefer importing this module over @Internal@ in application code and
-- library extensions.
--
-- Exports are grouped by concern:
--
-- * __DOM manipulation__: 'getElementById', 'focus', 'blur', 'removeChild', …
-- * __Logging__: 'consoleLog', 'consoleError', 'consoleWarn'
-- * __Callbacks__: 'syncCallback', 'asyncCallback', 'asyncCallback2', …
-- * __Canvas\/Drawing__: 'flush', 'setDrawingContext'
-- * __Browser APIs__: 'fetch', 'addEventListener', 'dispatchEvent', …
-- * __JS types__: 'ArrayBuffer', 'Blob', 'FormData', 'Uint8Array', 'File', …
--
-- For inline JavaScript via quasi-quotation, see "Miso.FFI.QQ".
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
  , removeChild
    -- ** Styles
  , addStyle
  , addStyleSheet
    -- * JS
  , addSrc
  , addScript
  , addScriptImportMap
    -- ** Callbacks
  , syncCallback
  , syncCallback1
  , asyncCallback
  , asyncCallback1
  , asyncCallback2
    -- ** Drawing
  , flush
  , setDrawingContext
    -- ** Window
  , windowInnerWidth
  , windowInnerHeight
  , locationReload
    -- ** Image
  , Image (..)
  , newImage
    -- ** Date
  , Date (..)
  , newDate
  , toLocaleString
  , getSeconds
  , getMilliseconds
    -- ** DOM Traversal
  , nextSibling
  , previousSibling
    -- ** Element
  , click
  , setValue
    -- ** File Input
  , files
    -- ** Navigator
  , isOnLine
    -- ** Lynx
  , onBTS
  , onMTS
  , getThreads
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
    -- ** Inline JS
  , inline
    -- ** Scroll
  , scrollIntoView
    -- ** Fullscreen
  , requestFullscreen
    -- ** SplitMix32
  , splitmix32
    -- ** Math.random()
  , mathRandom
  ) where
-----------------------------------------------------------------------------
import           Miso.FFI.Internal
-----------------------------------------------------------------------------
