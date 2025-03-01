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
module Miso.Diff ( diff
                 , mountElement
                 ) where

import GHCJS.Foreign.Internal     hiding (Object)
import GHCJS.Types
import JavaScript.Object.Internal
import Miso.Html.Types
import Miso.FFI
import Miso.String

-- | diffing / patching a given element
diff :: JSVal -> Maybe VTree -> Maybe VTree -> JSM ()
diff mountEl current new = do
  doc <- getDoc
  case (current, new) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) ->
      diff' current' new' mountEl doc
    (Nothing, Just (VTree new')) -> do
      diff' (Object jsNull) new' mountEl doc
    (Just (VTree current'), Nothing) ->
      diff' current' (Object jsNull) mountEl doc

-- | return the configured mountPoint element or the body
mountElement :: MisoString -> JSM JSVal
mountElement "body" = getBody
mountElement e = getElementById e
