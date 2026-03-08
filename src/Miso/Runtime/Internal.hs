-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Runtime.Internal
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This is an internal module not meant for consumption during application
-- development. This is primarily meant to give user's access to the
-- global @component@ state for testing. Use at your own risk. You have been warned.
--
-- We currently consume this module for the miso integration testing package @miso-tests@.
--
-- Alterations to these global variables *will* break your application. Again, do not
-- use these in your main application, only use with the miso testing framework to test
-- your application.
--
----------------------------------------------------------------------------
module Miso.Runtime.Internal
  ( components
  , componentIds
  , rootComponentId
  , ComponentState(..)
#ifdef WASM
  , evalFile
#endif
  ) where
----------------------------------------------------------------------------
import Miso.Runtime (components, ComponentState(..), componentIds, rootComponentId)
----------------------------------------------------------------------------
#ifdef WASM
-----------------------------------------------------------------------------
-- | Like 'eval', but read the JS code to evaluate from a file.
evalFile
  :: FilePath
  -- ^ Path to JS file that will be converted into an FFI declaration.
  -> TH.Q TH.Exp
evalFile path = eval_ =<< TH.runIO (readFile path)
  where
    eval_ :: String -> TH.Q TH.Exp
    eval_ chunk = [| $(Miso.DSL.TH.evalTH chunk []) :: IO () |]
-----------------------------------------------------------------------------
#endif
