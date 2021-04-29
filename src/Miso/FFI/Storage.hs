-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.Storage
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.Storage
  ( Storage
  , localStorage
  , sessionStorage
  , getItem
  , removeItem
  , setItem
  , length
  , clear
  ) where

import GHCJS.Types
import Prelude hiding (length)

import Language.Javascript.JSaddle ((!), (#), JSM, fromJSValUnchecked, jsg)
import Miso.String hiding (length)

newtype Storage = Storage JSVal

localStorage :: JSM Storage
localStorage = Storage <$> (jsg "window" ! "localStorage")

sessionStorage :: JSM Storage
sessionStorage = Storage <$> (jsg "window" ! "sessionStorage")

getItem :: Storage -> MisoString -> JSM JSVal
getItem (Storage s) key =
  s # "getItem" $ [key]

removeItem :: Storage -> MisoString -> JSM ()
removeItem (Storage s) key = do
  _ <- s # "removeItem" $ [key]
  pure ()

setItem :: Storage -> MisoString -> MisoString -> JSM ()
setItem (Storage s) key val = do
  _ <- s # "setItem" $ (key, val)
  pure ()

length :: Storage -> JSM Int
length (Storage s) = fromJSValUnchecked =<< s ! "length"

clear :: Storage -> JSM ()
clear (Storage s) = do
  _ <- s # "clear" $ ()
  pure ()
