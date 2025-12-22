-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Diff
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
-- | diffing / patching a given element
diff :: Maybe VTree -> Maybe VTree -> JSVal -> IO ()
diff current new_ mountEl =
  case (current, new_) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) -> do
      FFI.diff current' new' mountEl
      FFI.flush
    (Nothing, Just (VTree new')) -> do
      null_ <- jsNull
      FFI.diff (Object null_) new' mountEl
      FFI.flush
    (Just (VTree current'), Nothing) -> do
      null_ <- jsNull
      FFI.diff current' (Object null_) mountEl
      FFI.flush
-----------------------------------------------------------------------------
-- | return the configured mountPoint element or the body
mountElement :: MisoString -> IO JSVal
mountElement = \case
  "body" -> FFI.getBody
  e -> FFI.getElementById e
-----------------------------------------------------------------------------
