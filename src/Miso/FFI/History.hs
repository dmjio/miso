-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.History
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.History
  ( 
  ) where

-- import Control.Monad
-- import GHCJS.Types
-- import Language.Javascript.JSaddle
-- import Miso.String
-- import Miso.FFI

-- getWindowLocationHref :: IO MisoString
-- getWindowLocationHref = undefined -- do
  -- href <- fromJSVal =<< jsg "window" ! "location" ! "href"
  -- case join href of
  --   Nothing -> pure mempty
  --   Just uri -> pure uri

-- go :: Int -> IO ()
-- go i = undefined -- do
  -- _ <- getHistory # "go" $ [i]
  -- pure ()

-- back :: IO ()
-- back = undefined -- do
  -- _ <- getHistory # "back" $ ()
  -- pure ()

-- forward :: IO ()
-- forward = undefined -- do
  -- _ <- getHistory # "forward" $ ()
  -- pure ()

-- pushState :: MisoString -> IO ()
-- pushState url = undefined -- do
  -- _ <- getHistory # "pushState" $ (jsNull, jsNull, url)
  -- pure ()

-- replaceState :: MisoString -> IO ()
-- replaceState url = undefined -- do
  -- _ <- getHistory # "replaceState" $ (jsNull, jsNull, url)
  -- pure ()
