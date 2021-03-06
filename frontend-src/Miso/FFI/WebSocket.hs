{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.WebSocket
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.WebSocket
  ( Socket(..)
  , create
  , socketState
  , send
  , close
  , addEventListener
  , data'
  , wasClean
  , code
  , reason
  ) where

import           GHCJS.Types

import           Language.Javascript.JSaddle hiding (create)

import           Miso.FFI (JSM)
import qualified Miso.FFI as FFI
import           Miso.String
import           Miso.WebSocket

newtype Socket = Socket JSVal

create :: MisoString -> JSVal -> JSM Socket
create url protocols = Socket <$> new (jsg ("WebSocket" :: JSString)) (url, protocols)

socketState :: Socket -> JSM Int
socketState (Socket s) = fromJSValUnchecked =<< s ! ("readyState" :: JSString)

send :: Socket -> MisoString -> JSM ()
send (Socket s) msg = do
  _ <- s # ("send" :: JSString) $ [msg]
  pure ()

close :: Socket -> JSM ()
close (Socket s) = do
  _ <- s # ("close" :: JSString) $ ([] :: [JSString])
  pure ()

addEventListener :: Socket -> MisoString -> (JSVal -> JSM ()) -> JSM ()
addEventListener (Socket s) name cb = do
  FFI.addEventListener s name cb

data' :: JSVal -> JSM JSVal
data' v = v ! ("data" :: JSString)

wasClean :: JSVal -> JSM WasClean
wasClean v = WasClean <$> (fromJSValUnchecked =<< v ! ("wasClean" :: JSString))

code :: JSVal -> JSM Int
code v = fromJSValUnchecked =<< v ! ("code" :: JSString)

reason :: JSVal -> JSM Reason
reason v = Reason <$> (fromJSValUnchecked =<< v ! ("reason" :: JSString))
