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
----------------------------------------------------------------------------
module Miso.Diff
  ( diff
  , mountElement
  ) where
-----------------------------------------------------------------------------
import           GHCJS.Foreign.Internal hiding (Object)
import           GHCJS.Types
import           JavaScript.Object.Internal
import qualified Miso.FFI as FFI
import           Miso.FFI (JSM)
import           Miso.Html.Types
import           Miso.String
-----------------------------------------------------------------------------
-- | diffing / patching a given element
diff :: JSVal -> Maybe VTree -> Maybe VTree -> JSM ()
diff mountEl current new =
  case (current, new) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) ->
      FFI.diff current' new' mountEl
    (Nothing, Just (VTree new')) -> do
      FFI.diff (Object jsNull) new' mountEl
    (Just (VTree current'), Nothing) ->
      FFI.diff current' (Object jsNull) mountEl
-----------------------------------------------------------------------------
-- | return the configured mountPoint element or the body
mountElement :: MisoString -> JSM JSVal
mountElement "body" = FFI.getBody
mountElement e = FFI.getElementById e
-----------------------------------------------------------------------------
