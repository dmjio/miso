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
import JavaScript.Object
import JavaScript.Object.Internal
import Miso.Html.Internal

-- | Entry point for diffing / patching algorithm
diff :: Maybe JSString -> Maybe VTree -> Maybe VTree -> IO ()
diff mayElem current new =
  case mayElem of
    Nothing -> do
      body <- getBody
      diffElement body current new
    Just elemId -> do
      e <- getElementById elemId
      diffElement e current new

-- | diffing / patching a given element
diffElement :: JSVal -> Maybe VTree -> Maybe VTree -> IO ()
diffElement mountEl current new = do
  case (current, new) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) -> do
      diff' current' new' mountEl
    (Nothing, Just (VTree new')) -> do
      diff' (Object jsNull) new' mountEl
    (Just (VTree current'), Nothing) -> do
      diff' current' (Object jsNull) mountEl

-- | return the configured mountPoint element or the body
mountElement :: Maybe JSString -> IO JSVal
mountElement mayMp =
  case mayMp of
    Nothing -> getBody
    Just eid -> getElementById eid

foreign import javascript unsafe "$r = document.body;"
  getBody :: IO JSVal

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "diff($1, $2, $3);"
  diff'
    :: Object -- ^ current object
    -> Object -- ^ new object
    -> JSVal  -- ^ parent node
    -> IO ()
