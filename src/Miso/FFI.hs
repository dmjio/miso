{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
   ( JSM
   , forkJSM
   , syncCallback
   , syncCallback1
   , asyncCallback
   , asyncCallback1
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
   , undelegateEvent
   , copyDOMIntoVTree
   , focus
   , blur
   , scrollIntoView
   , alert
   , getComponent
   , setBodyComponent
   ) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson hiding (Object)
import qualified Data.Aeson as A
import qualified Data.JSString as JSS
import           Language.Javascript.JSaddle hiding (obj, val)
import           Prelude hiding ((!!))

import           Miso.String

-- | Run given `JSM` action asynchronously, in a separate thread.
forkJSM :: JSM () -> JSM ThreadId
forkJSM a = do
  ctx <- askJSM
  liftIO (forkIO (runJSM a ctx))

-- | Creates a synchronous callback function (no return value)
syncCallback :: JSM () -> JSM Function
syncCallback a = function (\_ _ _ -> a)

-- | Creates an asynchronous callback function
asyncCallback :: JSM () -> JSM Function
asyncCallback a = asyncFunction (\_ _ _ -> a)

-- | Creates an asynchronous callback function with a single argument
asyncCallback1 :: (JSVal -> JSM ()) -> JSM Function
asyncCallback1 f = asyncFunction handle
  where
    handle _ _ []    = error "asyncCallback1: no args, impossible"
    handle _ _ (x:_) = f x

syncCallback1 :: (JSVal -> JSM ()) -> JSM Function
syncCallback1 f = function handle
  where
    handle _ _ []    = error "ssyncCallback1: no args, impossible"
    handle _ _ (x:_) = f x

-- | Set property on object
set :: ToJSVal v => MisoString -> v -> Object -> JSM ()
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
    A.Success x -> pure x
    A.Error y -> error y

-- | Convert a JavaScript object to JSON
objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> JSM JSVal
objectToJSON = jsg2 "objectToJSON"

-- | Retrieves the component id
getComponent :: MisoString -> JSM JSVal
getComponent name = nodeList !! 0
  where
    nodeList
      = jsg "document"
      # "querySelectorAll"
      $ [ "[data-component-id='" <> fromMisoString name <> "']" ]

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
    :: Object -- ^ current object
    -> Object -- ^ new object
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
delegateEvent mountPoint events getVTree =
  delegate mountPoint events =<< function handler
    where
      handler _ _ [] = error "delegate: no args - impossible state"
      handler _ _ (continuation : _) =
        void (call continuation global =<< getVTree)

-- | deinitialize event delegation from a mount point.
undelegateEvent :: JSVal -> JSVal -> JSM JSVal -> JSM ()
undelegateEvent mountPoint events getVTree =
  undelegate mountPoint events =<< function handler
    where
      handler _ _ [] = error "undelegate: no args - impossible state"
      handler _ _ (continuation : _) =
        void (call continuation global =<< getVTree)

-- | Call 'delegateEvent' JavaScript function
delegate :: JSVal -> JSVal -> Function -> JSM ()
delegate mountPoint events cb = () <$ jsg3 "delegate" mountPoint events cb

undelegate :: JSVal -> JSVal -> Function -> JSM ()
undelegate mountPoint events cb = () <$ jsg3 "undelegate" mountPoint events cb

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
copyDOMIntoVTree :: Bool -> JSVal -> JSVal -> JSM ()
copyDOMIntoVTree logLevel mountPoint vtree = void $ do
  doc <- getDoc
  jsg4 "copyDOMIntoVTree" logLevel mountPoint vtree doc

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

setBodyComponent :: MisoString -> JSM ()
setBodyComponent x = void $ jsg "window" # "setBodyComponent" $ [x]
