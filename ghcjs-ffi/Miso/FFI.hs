{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
   ( JSM
   , forkJSM
   , asyncCallback
   , asyncCallback1
   , callbackToJSVal
   , objectToJSVal
   , ghcjsPure
   , syncPoint

   , addEventListener
   , windowAddEventListener

   , windowInnerHeight
   , windowInnerWidth

   , eventPreventDefault
   , eventStopPropagation

   , now
   , consoleLog
   , stringify
   , parse
   , clearBody
   , objectToJSON
   , set
   , getBody
   , getDoc
   , getElementById
   , diff'

   , integralToJSString
   , realFloatToJSString
   , jsStringToDouble

   , delegateEvent

   , copyDOMIntoVTree

   , swapCallbacks
   , releaseCallbacks
   , registerCallback

   , focus
   , blur
   , scrollIntoView
   , alert
   ) where

import           Control.Concurrent
import           Data.Aeson hiding (Object)
import           Data.JSString
import           Data.JSString.Int
import           Data.JSString.RealFloat
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import qualified JavaScript.Object.Internal as OI

-- | When compiled without the `jsaddle` Cabal flag, this is just a
-- type synonym for `IO`. When the `jsaddle` flag is enabled, this
-- resolves to the `JSM` type defined in `jsaddle`.
type JSM = IO

forkJSM :: JSM () -> JSM ()
forkJSM a = () <$ forkIO a

callbackToJSVal :: Callback a -> JSM JSVal
callbackToJSVal = pure . jsval

objectToJSVal :: OI.Object -> JSM JSVal
objectToJSVal = pure . jsval

ghcjsPure :: a -> JSM a
ghcjsPure = pure

syncPoint :: JSM ()
syncPoint = pure ()

-- | Set property on object
set :: ToJSVal v => JSString -> v -> OI.Object -> IO ()
set k v obj = toJSVal v >>= \x -> OI.setProp k x obj

-- | Adds event listener to window
foreign import javascript unsafe "$1.addEventListener($2, $3);"
  addEventListener' :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()

addEventListener :: JSVal -> JSString -> (JSVal -> IO ()) -> IO ()
addEventListener self name cb = addEventListener' self name =<< asyncCallback1 cb

windowAddEventListener :: JSString -> (JSVal -> IO ()) -> IO ()
windowAddEventListener name cb = do
  win <- getWindow
  addEventListener win name cb

foreign import javascript unsafe "$1.stopPropagation();"
    eventStopPropagation :: JSVal -> IO ()

foreign import javascript unsafe "$1.preventDefault();"
    eventPreventDefault :: JSVal -> IO ()


-- | Window object
foreign import javascript unsafe "$r = window;"
  getWindow :: IO JSVal


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
parse :: FromJSON json => JSVal -> IO json
{-# INLINE parse #-}
parse jval = do
  k <- parse' jval
  Just val <- fromJSVal k
  case fromJSON val of
    Success x -> pure x
    Error y -> error y

-- | Clear the document body. This is particularly useful to avoid
-- creating multiple copies of your app when running in GHCJSi.
foreign import javascript unsafe "document.body.innerHTML = '';"
  clearBody :: IO ()


foreign import javascript unsafe "$r = objectToJSON($1,$2);"
  objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> IO JSVal

foreign import javascript unsafe "$r = document.body;"
  getBody :: IO JSVal

foreign import javascript unsafe "$r = document;"
  getDoc :: IO JSVal

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "diff($1, $2, $3, $4);"
  diff'
    :: OI.Object -- ^ current object
    -> OI.Object -- ^ new object
    -> JSVal  -- ^ parent node
    -> JSVal  -- ^ document
    -> IO ()

integralToJSString :: Integral a => a -> JSString
integralToJSString = decimal

realFloatToJSString :: RealFloat a => a -> JSString
realFloatToJSString = realFloat

foreign import javascript unsafe "$r = Number($1);"
  jsStringToDouble :: JSString -> Double

delegateEvent :: JSVal -> JSVal -> IO JSVal -> IO ()
delegateEvent mountPoint events getVTree = do
  cb' <- syncCallback1 ThrowWouldBlock $ \continuation -> do
    res <- getVTree
    callFunction continuation res
  delegateEvent' mountPoint events cb'

foreign import javascript unsafe "delegate($1, $2, $3);"
  delegateEvent'
     :: JSVal               -- ^ mountPoint element
     -> JSVal               -- ^ Events
     -> Callback (JSVal -> IO ()) -- ^ Virtual DOM callback
     -> IO ()

foreign import javascript unsafe "$1($2);"
  callFunction :: JSVal -> JSVal -> IO ()

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree :: JSVal -> IO ()

-- | Pins down the current callbacks for clearing later
foreign import javascript unsafe "swapCallbacks();"
  swapCallbacks :: IO ()

-- | Releases callbacks registered by the virtual DOM.
foreign import javascript unsafe "releaseCallbacks();"
  releaseCallbacks :: IO ()

foreign import javascript unsafe "registerCallback($1);"
  registerCallback :: JSVal -> IO ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
foreign import javascript unsafe "callFocus($1);"
  focus :: JSString -> JSM ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
foreign import javascript unsafe "callBlur($1);"
  blur :: JSString -> JSM ()

-- | Calls @document.getElementById(id).scrollIntoView()@
foreign import javascript unsafe
  "document.getElementById($1).scrollIntoView();"
  scrollIntoView :: JSString -> IO ()

-- | Calls the @alert()@ function.
foreign import javascript unsafe "alert($1);"
  alert :: JSString -> JSM ()
