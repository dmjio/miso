-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
  ( -- ** Types
    MisoException (..)
    -- ** Functions
  , exception
  ) where
----------------------------------------------------------------------------
import           Control.Exception
import           Language.Javascript.JSaddle
----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
import qualified Miso.FFI as FFI
----------------------------------------------------------------------------
-- | The @MisoException@ type is used to catch @Component@-related mounting errors.
--
-- The two mounting errors that can occur during the lifetime of a miso application are
--
-- * Already Mounted Exception
--
-- It is a requirement that all @Component@ be named uniquely
-- (this is to avoid runaway recursion during mounting).
-- If we detect a @Component@ is attempting to be mounted twice
-- this exception will be raised.
--
-- Other exceptions can arise, but its up to the user to handle them in
-- the @update@ function. All unhandled exceptions are caught in the event loop
-- and logged to the console with /console.error()/
--
data MisoException
  = AlreadyMountedException MisoString
  -- ^ Thrown when a @Component@ is attempted to be mounted twice.
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance Exception MisoException
----------------------------------------------------------------------------
-- | Exception handler
--
-- Used to catch @Component@ mounting exceptions
--
-- > action `catch` exception
exception :: SomeException -> JSM JSVal
exception ex
  | Just (AlreadyMountedException name) <- fromException ex = do
      FFI.consoleError ("AlreadyMountedException: Component \"" <> name <> "\" is already")
      pure jsNull
  | otherwise = do
      FFI.consoleError ("UnknownException: " <> ms ex)
      pure jsNull
----------------------------------------------------------------------------
