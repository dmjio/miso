-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Diff
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
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

-- | Entry point for diffing / patching algorithm
diff :: Maybe MisoString -> Maybe VTree -> Maybe VTree -> JSM ()
diff mayElem current new =
  case mayElem of
    Nothing -> do
      body <- getBody
      diffElement body current new
    Just elemId -> do
      e <- getElementById elemId
      diffElement e current new

-- | diffing / patching a given element
diffElement :: JSVal -> Maybe VTree -> Maybe VTree -> JSM ()
diffElement mountEl current new = do
  doc <- getDoc
  case (current, new) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) -> do
      diff' current' new' mountEl doc
    (Nothing, Just (VTree new')) -> do
      diff' (Object jsNull) new' mountEl doc
    (Just (VTree current'), Nothing) -> do
      diff' current' (Object jsNull) mountEl doc

-- | return the configured mountPoint element or the body
mountElement :: Maybe MisoString -> JSM JSVal
mountElement mayMp =
  case mayMp of
    Nothing -> getBody
    Just eid -> getElementById eid
