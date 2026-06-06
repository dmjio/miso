-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Diff
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Functions and helpers for Virtual DOM diffing.
--
----------------------------------------------------------------------------
module Miso.Diff
  ( diff
  , mountElement
  ) where
-----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Types
import           Miso.DSL
-----------------------------------------------------------------------------
-- | Computes the diff between two virtual DOM trees and applies the resulting
-- patch to the given mount-point element. Passing 'Nothing' for the current
-- tree performs a full mount; 'Nothing' for the new tree unmounts.
diff
  :: Maybe VTree
  -- ^ Current (old) virtual DOM tree, or 'Nothing' for initial mount
  -> Maybe VTree
  -- ^ New virtual DOM tree to patch to, or 'Nothing' to unmount
  -> JSVal
  -- ^ The DOM element that serves as the patch target
  -> IO ()
diff current new_ mountEl =
  case (current, new_) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) -> do
      FFI.diff current' new' mountEl
      FFI.flush
    (Nothing, Just (VTree new')) -> do
      FFI.diff (Object jsNull) new' mountEl
      FFI.flush
    (Just (VTree current'), Nothing) -> do
      FFI.diff current' (Object jsNull) mountEl
      FFI.flush
-----------------------------------------------------------------------------
-- | Resolves the configured mount-point name to an actual DOM element.
-- Returns @\<body\>@ when the name is @"body"@, otherwise calls @getElementById@.
mountElement
  :: MisoString
  -- ^ Mount-point name: @"body"@ for the document body, or an element ID
  -> IO JSVal
mountElement = \case
  "body" -> FFI.getBody
  e -> FFI.getElementById e
-----------------------------------------------------------------------------
