-----------------------------------------------------------------------------
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
import           Language.Javascript.JSaddle (JSVal, Object(..), jsNull)
-----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Types
-----------------------------------------------------------------------------
-- | diffing / patching a given element
diff :: Maybe VTree -> Maybe VTree -> JSVal -> JSM ()
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
-- | return the configured mountPoint element or the body
mountElement :: MisoString -> JSM JSVal
mountElement "body" = FFI.getBody
mountElement e = FFI.getElementById e
-----------------------------------------------------------------------------
