-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.History
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.History
  ( getWindowLocationHref
  , go
  , back
  , forward
  , pushState
  , replaceState
  ) where

import Control.Monad
import GHCJS.Types
import Language.Javascript.JSaddle
import Miso.String

getWindowLocationHref :: JSM MisoString
getWindowLocationHref = do
  href <- fromJSVal =<< jsg "window" ! "location" ! "href"
  case join href of
    Nothing -> pure mempty
    Just uri -> pure uri

getHistory :: JSM JSVal
getHistory = jsg "window" ! "history"

go :: Int -> JSM ()
go i = do
  _ <- getHistory # "go" $ [i]
  pure ()

back :: JSM ()
back = do
  _ <- getHistory # "back" $ ()
  pure ()

forward :: JSM ()
forward = do
  _ <- getHistory # "forward" $ ()
  pure ()

pushState :: MisoString -> JSM ()
pushState url = do
  _ <- getHistory # "pushState" $ (jsNull, jsNull, url)
  pure ()

replaceState :: MisoString -> JSM ()
replaceState url = do
  _ <- getHistory # "replaceState" $ (jsNull, jsNull, url)
  pure ()
