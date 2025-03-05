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

import           Miso.FFI hiding (diff)
import qualified Miso.FFI as FFI
import           Miso.Html.Types
import           Miso.String

-- | Diffing / patching a given element
diff :: JSVal -> Maybe VTree -> Maybe VTree -> IO ()
diff mount current new = do
  doc <- getDoc
  case (current, new) of
    (Nothing, Nothing) ->
      pure ()
    (Just (VTree c), Just (VTree n)) ->
      FFI.diff c n mount doc
    (Nothing, Just (VTree n)) ->
      FFI.diff (JSObject jsNull) n mount doc
    (Just (VTree c), Nothing) ->
      FFI.diff c (JSObject jsNull) mount doc

-- | return the configured mountPoint element or the body
mountElement :: MisoString -> IO JSVal
mountElement "body" = getBody
mountElement e = getElementById e
