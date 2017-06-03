{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
   ( windowAddEventListener
   , windowRemoveEventListener
   , windowInnerHeight
   , windowInnerWidth
   , now
   , consoleLog
   , stringify
   , parse
   , copyDOMIntoVTree
   , delegator
   , item
   ) where

import           Data.Aeson                 hiding (Object)
import           Data.IORef
import           Data.JSString
import qualified Data.Map                   as M
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object.Internal
import           Miso.Html.Internal

-- | Adds event listener to window
foreign import javascript unsafe "window.addEventListener($1, $2);"
  windowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

-- | Removes event listener from window
foreign import javascript unsafe "window.removeEventListener($1, $2);"
  windowRemoveEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

-- | Retrieves inner height
foreign import javascript unsafe "$r = window.innerHeight;"
  windowInnerHeight :: IO Int

-- | Retrieves outer height
foreign import javascript unsafe "$r = window.innerWidth;"
  windowInnerWidth :: IO Int

-- | Retrieve high performance time stamp
foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

-- | Console-logging
foreign import javascript unsafe "console.log($1);"
  consoleLog :: JSVal -> IO ()

-- | Converts a JS object into a JSON string
foreign import javascript unsafe "$r = JSON.stringify($1);"
  stringify' :: JSVal -> IO JSString

foreign import javascript unsafe "$r = JSON.parse($1);"
  parse' :: JSVal -> IO JSVal

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> IO JSString
{-# INLINE stringify #-}
stringify j = stringify' =<< toJSVal (toJSON j)

-- | Parses a JSString
parse :: FromJSON json => JSVal -> IO (Either String json)
{-# INLINE parse #-}
parse jval = do
  Just val <- fromJSVal =<< parse' jval
  pure $ case fromJSON val of
    Success x -> Right x
    Error y -> Left y

-- | Indexing into a JS object
foreign import javascript unsafe "$r = $1[$2];"
  item :: JSVal -> JSString -> IO JSVal

-- -- | Function application
-- foreign import javascript unsafe "$r = $1[$2].apply($1, $3);"
--   applyFunction :: JSVal -> JSString -> JSArray -> IO JSVal

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree :: VTree -> IO ()

foreign import javascript unsafe "delegate($1, $2);"
  delegateEvent
     :: JSVal                     -- ^ Events
     -> Callback (IO JSVal)       -- ^ Virtual DOM callback
     -> IO ()

-- | Entry point for event delegation
delegator
  :: IORef VTree
  -> M.Map JSString Bool
  -> IO ()
delegator vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (Object val) <- readIORef vtreeRef
    pure val
  delegateEvent evts getVTreeFromRef
