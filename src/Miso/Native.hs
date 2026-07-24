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
--   view_
--   []
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
     -- * Element
   , module Miso.Native.Element
     -- * FFI
   , module Miso.Native.FFI
     -- * Event
   , module Miso.Native.Event
   ) where
-----------------------------------------------------------------------------
import Miso (startApp)
import Miso.Types (Events, SomeComponent)
-----------------------------------------------------------------------------
import Miso.Native.Element
import Miso.Native.FFI
import Miso.Native.Event
-----------------------------------------------------------------------------
import GHC.StaticPtr (StaticPtr)
-----------------------------------------------------------------------------
-- | The native drawing context is already selected per-thread by the runtime
-- (@ts\/miso-native.ts@ picks @bts@ or @mts@ from @__BACKGROUND__@), so there
-- is no renderer to register — we start the app directly.
native
  :: Events
  -> StaticPtr (SomeComponent ())
  -> IO ()
native events ptr = startApp events ptr
-----------------------------------------------------------------------------
