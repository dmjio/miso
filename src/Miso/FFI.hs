{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Control.Monad.IO.Class
import           Data.Aeson hiding (Object)
import qualified Data.JSString as JSS
import           GHCJS.Marshal
import           GHCJS.Types
import qualified JavaScript.Object.Internal as OI
#ifdef __GHCJS__
import           Language.Javascript.JSaddle hiding (obj, val)
#else
import           Language.Javascript.JSaddle hiding (Success, obj, val)
#endif
import           Miso.String hiding (elem)

-- | Run given `JSM` action asynchronously, in a separate thread.
forkJSM :: JSM () -> JSM ()
forkJSM a = do
  ctx <- askJSM
  _ <- liftIO (forkIO (runJSM a ctx))
  pure ()

-- | Creates an asynchronous callback function
asyncCallback :: JSM () -> JSM Function
asyncCallback a = asyncFunction (\_ _ _ -> a)

-- | Creates an asynchronous callback function with a single argument
asyncCallback1 :: (JSVal -> JSM ()) -> JSM Function
asyncCallback1 f = asyncFunction (\_ _ [x] -> f x)

-- | Convert a Callback into a JSVal
callbackToJSVal :: Function -> JSM JSVal
callbackToJSVal = toJSVal

-- | Convert an Object into a JSVal
objectToJSVal :: Object -> JSM JSVal
objectToJSVal = toJSVal

-- | Set property on object
set :: ToJSVal v => MisoString -> v -> OI.Object -> JSM ()
set (unpack -> "class") v obj = do
  classSet <- ((JSS.pack "class") `Prelude.elem`) <$> listProps obj
  if classSet
    then do
      classStr <- fromJSValUnchecked =<< getProp (JSS.pack "class") obj
      vStr <- fromJSValUnchecked =<< toJSVal v
      v' <- toJSVal (classStr <> JSS.pack " " <> vStr)
      setProp (JSS.pack "class") v' obj
    else do
      v' <- toJSVal v
      setProp (JSS.pack "class") v' obj
set k v obj = do
  v' <- toJSVal v
  setProp (fromMisoString k) v' obj

-- | Register an event listener on given target.
addEventListener :: JSVal             -- ^ Event target on which we want to register event listener
                 -> MisoString        -- ^ Type of event to listen to (e.g. "click")
                 -> (JSVal -> JSM ()) -- ^ Callback which will be called when the event occurs, the event will be passed to it as a parameter.
                 -> JSM ()
addEventListener self name cb = do
  _ <- self # "addEventListener" $ (name, asyncFunction (\_ _ [a] -> cb a))
  pure ()

-- | Registers an event listener on window
windowAddEventListener :: MisoString         -- ^ Type of event to listen to (e.g. "click")
                       -> (JSVal -> JSM ())  -- ^ Callback which will be called when the event occurs, the event will be passed to it as a parameter.
                       -> JSM ()
windowAddEventListener name cb = do
  win <- jsg "window"
  addEventListener win name cb

-- | Stop propagation of events
eventStopPropagation :: JSVal -> JSM ()
eventStopPropagation e = do
  _ <- e # "stopPropagation" $ ()
  pure ()

-- | Prevent default event behavior
eventPreventDefault :: JSVal -> JSM ()
eventPreventDefault e = do
  _ <- e # "preventDefault" $ ()
  pure ()

-- | Retrieves the height (in pixels) of the browser window viewport including, if rendered, the horizontal scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerHeight>
windowInnerHeight :: JSM Int
windowInnerHeight =
  fromJSValUnchecked =<< jsg "window" ! "innerHeight"

-- | Retrieves the width (in pixels) of the browser window viewport including, if rendered, the vertical scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth>
windowInnerWidth :: JSM Int
windowInnerWidth =
  fromJSValUnchecked =<< jsg "window" ! "innerWidth"

-- | Retrieve high resolution time stamp
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Performance/now>
now :: JSM Double
now = fromJSValUnchecked =<< (jsg "performance" # "now" $ ())

-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
consoleLog :: MisoString -> JSM ()
consoleLog v = do
  _ <- jsg "console" # "log" $ [toJSString v]
  pure ()

-- | Console-logging
consoleLogJSVal :: JSVal -> JSM ()
consoleLogJSVal v = do
  _ <- jsg "console" # "log" $ [v]
  pure ()

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> JSM MisoString
{-# INLINE stringify #-}
stringify j = do
  v <- toJSVal (toJSON j)
  fromJSValUnchecked =<< (jsg "JSON" # "stringify" $ [v])

-- | Parses a MisoString
parse :: FromJSON json => JSVal -> JSM json
{-# INLINE parse #-}
parse jval = do
  val <- fromJSValUnchecked =<< (jsg "JSON" # "parse" $ [jval])
  case fromJSON val of
    Success x -> pure x
    Error y -> error y

-- | Clear the document body. This is particularly useful to avoid
-- creating multiple copies of your app when running in GHCJSi.
clearBody :: JSM ()
clearBody =
  (jsg "document" ! "body"  <# "innerHtml") [""]

-- | Convert a JavaScript object to JSON
objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> JSM JSVal
objectToJSON = jsg2 "objectToJSON"

-- | Retrieves a reference to document body.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/body>
getBody :: JSM JSVal
getBody = jsg "document" ! "body"

-- | Retrieves a reference to the document.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document>
getDoc :: JSM JSVal
getDoc = jsg "document"

-- | Returns an Element object representing the element whose id property matches the specified string.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById>
getElementById :: MisoString -> JSM JSVal
getElementById e = getDoc # "getElementById" $ [e]

-- | Diff two virtual DOMs
diff'
    :: OI.Object -- ^ current object
    -> OI.Object -- ^ new object
    -> JSVal  -- ^ parent node
    -> JSVal -- ^ document
    -> JSM ()
diff' a b c d = () <$ jsg4 "diff" a b c d

-- | Helper function for converting Integral types to JavaScript strings
integralToJSString :: Integral a => a -> MisoString
integralToJSString = pack . show . toInteger

-- | Helper function for converting RealFloat types to JavaScript strings
realFloatToJSString :: RealFloat a => a -> MisoString
realFloatToJSString x = (pack . show) (realToFrac x :: Double)

-- | Helper function for converting RealFloat types to JavaScript strings
jsStringToDouble :: MisoString -> Double
jsStringToDouble = read . unpack

-- | Initialize event delegation from a mount point.
delegateEvent :: JSVal -> JSVal -> JSM JSVal -> JSM ()
delegateEvent mountPoint events getVTree = do
  cb' <- function $ \_ _ [continuation] -> do
    res <- getVTree
    _ <- call continuation global res
    pure ()
  delegateEvent' mountPoint events cb'

-- | Call 'delegateEvent' JavaScript function
delegateEvent' :: JSVal -> JSVal -> Function -> JSM ()
delegateEvent' mountPoint events cb = () <$ jsg3 "delegate" mountPoint events cb

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
copyDOMIntoVTree :: Bool -> JSVal -> JSVal -> JSM ()
copyDOMIntoVTree logLevel mountPoint a = () <$ jsg3 "copyDOMIntoVTree" logLevel mountPoint a

-- TODO For now, we do not free callbacks when compiling with JSaddle

-- | Pins down the current callbacks for clearing later
swapCallbacks :: JSM ()
swapCallbacks = pure ()

-- | Releases callbacks registered by the virtual DOM.
releaseCallbacks :: JSM ()
releaseCallbacks = pure ()

-- | Mock for callback registration
registerCallback :: JSVal -> JSM ()
registerCallback _ = pure ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
focus :: MisoString -> JSM ()
focus a = () <$ jsg1 "callFocus" a

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
blur :: MisoString -> JSM ()
blur a = () <$ jsg1 "callBlur" a

-- | Calls @document.getElementById(id).scrollIntoView()@
scrollIntoView :: MisoString -> JSM ()
scrollIntoView elId = do
  el <- jsg "document" # "getElementById" $ [elId]
  _ <- el # "scrollIntoView" $ ()
  pure ()

-- | Calls the @alert()@ function.
alert :: MisoString -> JSM ()
alert a = () <$ jsg1 "alert" a
