-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
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
  -- ** Component
  , getParentComponentId
  , getComponentId
  -- ** DOM
  , nextSibling
  , previousSibling
  -- ** Element
  , click
  , files
  -- ** Re-exports
  , JSVal
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
  ) where
-----------------------------------------------------------------------------
import           Miso.FFI.Internal
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle (JSVal)
-----------------------------------------------------------------------------
