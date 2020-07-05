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
   , consoleLogJSVal
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
import           Data.Aeson                 hiding (Object)
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

-- | Run given `JSM` action asynchronously, in a separate thread.
forkJSM :: JSM () -> JSM ()
forkJSM a = () <$ forkIO a

callbackToJSVal :: Callback a -> JSM JSVal
callbackToJSVal = pure . jsval

objectToJSVal :: OI.Object -> JSM JSVal
objectToJSVal = pure . jsval

ghcjsPure :: a -> JSM a
ghcjsPure = pure

-- | Forces execution of pending asyncronous code
syncPoint :: JSM ()
syncPoint = pure ()

-- | Set property on object
set :: ToJSVal v => JSString -> v -> OI.Object -> IO ()
set "class" v obj = toJSVal v >>= appendClass obj
set k v obj = toJSVal v >>= \x -> OI.setProp k x obj

-- | Only used for 'class', guaranteed to be a MisoString
foreign import javascript unsafe "if ('class' in $1) { $1['class'] += ' ' + $2; } else { $1['class'] = $2; }"
  appendClass :: OI.Object -> JSVal -> IO ()

foreign import javascript unsafe "$1.addEventListener($2, $3);"
  addEventListener' :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()

-- | Register an event listener on given target.
addEventListener :: JSVal            -- ^ Event target on which we want to register event listener
                 -> JSString         -- ^ Type of event to listen to (e.g. "click")
                 -> (JSVal -> IO ()) -- ^ Callback which will be called when the event occurs. The event is passed as a parameter to it.
                 -> IO ()
addEventListener self name cb = addEventListener' self name =<< asyncCallback1 cb

-- | Registers an event listener on window
windowAddEventListener :: JSString           -- ^ Type of event to listen to (e.g. "click")
                       -> (JSVal -> IO ())  -- ^ Callback which will be called when the event occurs, the event will be passed to it as a parameter.
                       -> IO ()
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


-- | Retrieves the height (in pixels) of the browser window viewport including, if rendered, the horizontal scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerHeight>
foreign import javascript unsafe "$r = window['innerHeight'];"
  windowInnerHeight :: IO Int

-- | Retrieves the width (in pixels) of the browser window viewport including, if rendered, the vertical scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth>
foreign import javascript unsafe "$r = window['innerWidth'];"
  windowInnerWidth :: IO Int

-- | Retrieve high resolution time stamp
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Performance/now>
foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
foreign import javascript unsafe "console.log($1);"
  consoleLog :: JSString -> IO ()

foreign import javascript unsafe "console.log($1);"
  consoleLogJSVal :: JSVal -> IO ()

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


foreign import javascript unsafe "$r = window['objectToJSON']($1,$2);"
  objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> IO JSVal

-- | Retrieves a reference to the document body.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/body>
foreign import javascript unsafe "$r = document.body;"
  getBody :: IO JSVal

-- | Retrieves a reference to the document.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document>
foreign import javascript unsafe "$r = document;"
  getDoc :: IO JSVal

-- | Returns an Element object representing the element whose id property matches the specified string.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById>
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

foreign import javascript unsafe "window['delegate']($1, $2, $3);"
  delegateEvent'
     :: JSVal               -- ^ mountPoint element
     -> JSVal               -- ^ Events
     -> Callback (JSVal -> IO ()) -- ^ Virtual DOM callback
     -> IO ()

foreign import javascript unsafe "$1($2);"
  callFunction :: JSVal -> JSVal -> IO ()

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
foreign import javascript unsafe "window['copyDOMIntoVTree']($1, $2, $3);"
  copyDOMIntoVTree
    :: Bool  -- ^ Display debugging information when pre-rendering
    -> JSVal -- ^ mountPoint element of the isomorphic app
    -> JSVal -- ^ VDom object
    -> IO ()

-- | Pins down the current callbacks for clearing later
foreign import javascript unsafe "window['swapCallbacks']();"
  swapCallbacks :: IO ()

-- | Releases callbacks registered by the virtual DOM.
foreign import javascript unsafe "window['releaseCallbacks']();"
  releaseCallbacks :: IO ()

foreign import javascript unsafe "window['registerCallback']($1);"
  registerCallback :: JSVal -> IO ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
foreign import javascript unsafe "window['callFocus']($1);"
  focus :: JSString -> JSM ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
foreign import javascript unsafe "window['callBlur']($1);"
  blur :: JSString -> JSM ()

-- | Calls @document.getElementById(id).scrollIntoView()@
foreign import javascript unsafe
  "document.getElementById($1)['scrollIntoView']();"
  scrollIntoView :: JSString -> IO ()

-- | Calls the @alert()@ function.
foreign import javascript unsafe "alert($1);"
  alert :: JSString -> JSM ()
