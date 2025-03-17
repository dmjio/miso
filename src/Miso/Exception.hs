-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Exception
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Exception
  ( -- * Exceptions
    MisoException (..)
  ) where
----------------------------------------------------------------------------
import Control.Exception
import Data.Typeable
----------------------------------------------------------------------------
import Miso.String (MisoString)
----------------------------------------------------------------------------
-- | The @MisoException@ type is used to catch @Component@-related mounting errors.
--
-- The two mounting errors that can occur during the lifetime of a miso application are
--
-- * Not Mounted Exception
--
-- This occurs if a user tries to call @sample myComponent@ when @myComponent@ is currently
-- not mounted onto the DOM.
--
-- * Already Mounted Exception
--
-- It is a requirement that all @Component@ be named uniquely
-- (this is to avoid runaway recursion during mounting).
-- If we detect a @Component@ is attempting to be mounted twice
-- this exception will be raised.
--
-- Other exceptions can arise, but its up to the user to handle them in
-- the `update` function. All unhandled exceptions are caught in the event loop
-- and logged to the console with /console.error()/
--
data MisoException
  = NotMountedException MisoString
  | AlreadyMountedException MisoString
  deriving (Show, Eq, Typeable)
----------------------------------------------------------------------------
instance Exception MisoException
----------------------------------------------------------------------------
