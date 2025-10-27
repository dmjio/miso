-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Module for re-exporting internal FFI functions for public consumption
--
----------------------------------------------------------------------------
module Miso.FFI
  ( -- ** Functions
    set
  , now
  , consoleLog
  , consoleLog'
  , consoleError
  , consoleWarn
  , getElementById
  , focus
  , blur
  , alert
  , reload
  , addStyle
  , addStyleSheet
  , addEventListener
  , syncCallback
  , syncCallback1
  , asyncCallback
  , asyncCallback1
  , asyncCallback2
  , flush
  , setDrawingContext
  , jsonStringify
  , jsonParse
  , windowInnerWidth
  , windowInnerHeight
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
  -- ** DOM
  , nextSibling
  , previousSibling
  -- ** Element
  , click
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
  ) where
-----------------------------------------------------------------------------
import           Miso.FFI.Internal
-----------------------------------------------------------------------------
