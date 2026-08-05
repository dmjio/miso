-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- import Miso
-- import Miso.Native
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ m =
--   vfrag_
--   [ view_ [ onTap Increment ] [ text_ [] [ "+" ] ]
--   , text_ [] [ text $ ms (show m) ]
--   , view_ [ onTap Decrement ] [ text_ [] [ "-" ] ]
--   ]
-- @
--
-- More information on how to use miso is available on GitHub
--
-- <http://github.com/dmjio/miso>
--
----------------------------------------------------------------------------
module Miso.Native
   ( -- * Entrypoint
     native
   , nativeWithContext
     -- * Element
   , module Miso.Native.Element
     -- * FFI
   , module Miso.Native.FFI
     -- * Event
   , module Miso.Native.Event
   ) where
-----------------------------------------------------------------------------
import Miso.Runtime (initComponent)
import Miso.Types (Events, SomeStaticComponent(..), SomeComponent(..), Hydrate(..))
import Miso.JSON (ToJSON, FromJSON)
-----------------------------------------------------------------------------
import Miso.Native.Element
import Miso.Native.FFI
import Miso.Native.Event
-----------------------------------------------------------------------------
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
-----------------------------------------------------------------------------
-- | The native drawing context is already selected per-thread by the runtime
-- (@ts\/miso-native.ts@ picks @bts@ or @mts@ from @__BACKGROUND__@), so there
-- is no renderer to register — we start the app directly.
--
-- @
-- {-# LANGUAGE StaticPointers #-}
--
-- import Miso
-- import Miso.Native
--
-- main :: IO ()
-- main = native nativeEvents (static (mount_ app))
-- @
--
native
  :: Events
  -> StaticPtr (SomeStaticComponent () ())
  -> IO ()
native events ptr =
  case deRefStaticPtr ptr of
    SomeStaticComponent mk -> case mk () of
      SomeComponent key props_ vcomp_ ->
        initComponent events Draw False () vcomp_
          key props_ (Just (staticKey ptr))
-----------------------------------------------------------------------------
-- | Like 'native', but the user can specify a global 'context' object.
--
-- @
-- {-# LANGUAGE StaticPointers #-}
--
-- import Miso
-- import Miso.Native
--
-- main :: IO ()
-- main = nativeWithContext nativeEvents () (static (mount_ app))
-- @
--
nativeWithContext
  :: (ToJSON context, FromJSON context, Eq context)
  => Events
  -> context
  -> StaticPtr (SomeStaticComponent () context)
  -> IO ()
nativeWithContext events context ptr =
  case deRefStaticPtr ptr of
    SomeStaticComponent mk -> case mk () of
      SomeComponent key props_ vcomp_ ->
        initComponent events Draw False context vcomp_
          key props_ (Just (staticKey ptr))
-----------------------------------------------------------------------------
