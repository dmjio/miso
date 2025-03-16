-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.SSE
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.SSE
  ( EventSource(..)
  , data'
  , new
  , addEventListener
  ) where
-----------------------------------------------------------------------------
import qualified Language.Javascript.JSaddle as JSaddle
import           Language.Javascript.JSaddle hiding (new)
-----------------------------------------------------------------------------
import qualified Miso.FFI as FFI
import           Miso.String
-----------------------------------------------------------------------------
newtype EventSource = EventSource JSVal
-----------------------------------------------------------------------------
data' :: JSVal -> JSM JSVal
data' v = v ! ("data" :: JSString)
-----------------------------------------------------------------------------
new :: MisoString -> JSM EventSource
new url = EventSource <$> JSaddle.new (jsg ("EventSource" :: JSString)) [url]
-----------------------------------------------------------------------------
addEventListener :: EventSource -> MisoString -> (JSVal -> JSM ()) -> JSM ()
addEventListener (EventSource s) = FFI.addEventListener s
-----------------------------------------------------------------------------
