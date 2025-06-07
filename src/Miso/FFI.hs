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
  ( -- * Functions
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
  , syncCallback
  , syncCallback1
  , asyncCallback
  -- ** Image
  , Image (..)
  , newImage
  ) where
-----------------------------------------------------------------------------
import           Miso.FFI.Internal
-----------------------------------------------------------------------------
