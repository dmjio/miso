{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.WebSocket
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.WebSocket
  ( 
  ) where

-- import           Miso.WebSocket

-- send :: Socket -> MisoString -> IO ()
-- send (Socket s) msg = undefined -- do
  -- _ <- s # ("send" :: JSString) $ [msg]
  -- pure ()

-- close :: Socket -> IO ()
-- close (Socket s) = undefined -- do
  -- _ <- s # ("close" :: JSString) $ ([] :: [JSString])
  -- pure ()

-- addEventListener :: Socket -> MisoString -> (JSVal -> IO ()) -> IO ()
-- addEventListener (Socket s) name cb = undefined -- do
--  FFI.addEventListener s name cb

-- data' :: JSVal -> IO JSVal
-- data' v = undefined -- v ! ("data" :: JSString)

-- wasClean :: JSVal -> IO WasClean
-- wasClean v = undefined -- WasClean <$> (fromJSValUnchecked =<< v ! ("wasClean" :: JSString))


-- code :: JSVal -> IO Int
-- code v = undefined -- fromJSValUnchecked =<< v ! ("code" :: JSString)

-- reason :: JSVal -> IO Reason
-- reason v = undefined -- Reason <$> (fromJSValUnchecked =<< v ! ("reason" :: JSString))
