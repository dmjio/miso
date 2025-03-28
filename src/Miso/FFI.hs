-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns        #-}
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
   , consoleWarn
   , consoleLog
   , consoleError
   , consoleLog'
   , jsonStringify
   , jsonParse
   , eventJSON
   , set
   , getBody
   , getDocument
   , getElementById
   , diff
   , integralToJSString
   , realFloatToJSString
   , jsStringToDouble
   , delegateEvent
   , undelegateEvent
   , hydrate
   , focus
   , blur
   , scrollIntoView
   , alert
   , reload
   , getComponent
   , setBodyComponent
   ) where
-----------------------------------------------------------------------------
import           Control.Concurrent (ThreadId, forkIO)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson hiding (Object)
import qualified Data.Aeson as A
import qualified Data.JSString as JSS
import           Language.Javascript.JSaddle
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.String
-----------------------------------------------------------------------------
-- | Run given `JSM` action asynchronously, in a separate thread.
forkJSM :: JSM () -> JSM ThreadId
forkJSM a = do
  ctx <- askJSM
  liftIO (forkIO (runJSM a ctx))
-----------------------------------------------------------------------------
-- | Creates a synchronous callback function (no return value)
syncCallback :: JSM () -> JSM Function
syncCallback a = function (\_ _ _ -> a)
-----------------------------------------------------------------------------
-- | Creates an asynchronous callback function
asyncCallback :: JSM () -> JSM Function
asyncCallback a = asyncFunction (\_ _ _ -> a)
-----------------------------------------------------------------------------
-- | Creates an asynchronous callback function with a single argument
asyncCallback1 :: (JSVal -> JSM ()) -> JSM Function
asyncCallback1 f = asyncFunction handle
  where
    handle _ _ []    = error "asyncCallback1: no args, impossible"
    handle _ _ (x:_) = f x
-----------------------------------------------------------------------------
syncCallback1 :: (JSVal -> JSM ()) -> JSM Function
syncCallback1 f = function handle
  where
    handle _ _ []    = error "syncCallback1: no args, impossible"
    handle _ _ (x:_) = f x
-----------------------------------------------------------------------------
-- | Set property on object
set :: ToJSVal v => MisoString -> v -> Object -> JSM ()
set (unpack -> "class") v o = do
  classSet <- ((JSS.pack "class") `Prelude.elem`) <$> listProps o
  if classSet
    then do
      classStr <- fromJSValUnchecked =<< getProp (JSS.pack "class") o
      vStr <- fromJSValUnchecked =<< toJSVal v
      v' <- toJSVal (classStr <> JSS.pack " " <> vStr)
      setProp (JSS.pack "class") v' o
    else do
      v' <- toJSVal v
      setProp (JSS.pack "class") v' o
set k v o = do
  v' <- toJSVal v
  setProp (fromMisoString k) v' o
-----------------------------------------------------------------------------
-- | Register an event listener on given target.
addEventListener
  :: JSVal
  -- ^ Event target on which we want to register event listener
  -> MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> (JSVal -> JSM ())
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> JSM ()
addEventListener self name cb = do
  _ <- self # "addEventListener" $ (name, asyncFunction (\_ _ [a] -> cb a))
  pure ()
-----------------------------------------------------------------------------
-- | Registers an event listener on window
windowAddEventListener
  :: MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> (JSVal -> JSM ())
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> JSM ()
windowAddEventListener name cb = do
  win <- jsg "window"
  addEventListener win name cb
-----------------------------------------------------------------------------
-- | Stop propagation of events
eventStopPropagation :: JSVal -> JSM ()
eventStopPropagation e = do
  _ <- e # "stopPropagation" $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Prevent default event behavior
eventPreventDefault :: JSVal -> JSM ()
eventPreventDefault e = do
  _ <- e # "preventDefault" $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Retrieves the height (in pixels) of the browser window viewport including,
-- if rendered, the horizontal scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerHeight>
windowInnerHeight :: JSM Int
windowInnerHeight =
  fromJSValUnchecked =<< jsg "window" ! "innerHeight"
-----------------------------------------------------------------------------
-- | Retrieves the width (in pixels) of the browser window viewport including
-- if rendered, the vertical scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth>
windowInnerWidth :: JSM Int
windowInnerWidth =
  fromJSValUnchecked =<< jsg "window" ! "innerWidth"
-----------------------------------------------------------------------------
-- | Retrieve high resolution time stamp
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Performance/now>
now :: JSM Double
now = fromJSValUnchecked =<< (jsg "performance" # "now" $ ())
-----------------------------------------------------------------------------
-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
--
-- Console logging of JavaScript strings.
consoleLog :: MisoString -> JSM ()
consoleLog v = do
  _ <- jsg "console" # "log" $ [toJSString v]
  pure ()
-----------------------------------------------------------------------------
-- | Outputs a warning message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/warn>
--
-- Console logging of JavaScript strings.
consoleWarn :: MisoString -> JSM ()
consoleWarn v = do
  _ <- jsg "console" # "warn" $ [toJSString v]
  pure ()
-----------------------------------------------------------------------------
-- | Outputs an error message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/error>
--
-- Console logging of JavaScript strings.
consoleError :: MisoString -> JSM ()
consoleError v = do
  _ <- jsg "console" # "error" $ [toJSString v]
  pure ()
-----------------------------------------------------------------------------
-- | Console-logging of JSVal
consoleLog' :: JSVal -> JSM ()
consoleLog' v = do
  _ <- jsg "console" # "log" $ [v]
  pure ()
-----------------------------------------------------------------------------
-- | Encodes a Haskell object as a JSON string by way of a JavaScript object
jsonStringify :: ToJSON json => json -> JSM MisoString
{-# INLINE jsonStringify #-}
jsonStringify j = do
  v <- toJSVal (toJSON j)
  fromJSValUnchecked =<< (jsg "JSON" # "stringify" $ [v])
-----------------------------------------------------------------------------
-- | Parses a MisoString
jsonParse :: FromJSON json => JSVal -> JSM json
{-# INLINE jsonParse #-}
jsonParse jval = do
  v <- fromJSValUnchecked =<< (jsg "JSON" # "parse" $ [jval])
  case fromJSON v of
    A.Success x -> pure x
    A.Error y -> error y
-----------------------------------------------------------------------------
-- | Convert a JavaScript object to JSON
-- JSONified representation of events
eventJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> JSM JSVal
eventJSON x y = do
  moduleExports <- jsg "module" ! "exports"
  moduleExports # "eventJSON" $ [x,y]
-----------------------------------------------------------------------------
-- | Retrieves the component id
getComponent :: MisoString -> JSM JSVal
getComponent name = nodeList !! 0
  where
    nodeList
      = jsg "document"
      # "querySelectorAll"
      $ [ "[data-component-id='" <> fromMisoString name <> "']" ]
-----------------------------------------------------------------------------
-- | Retrieves a reference to document body.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/body>
getBody :: JSM JSVal
getBody = jsg "document" ! "body"
-----------------------------------------------------------------------------
-- | Retrieves a reference to the document.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document>
getDocument :: JSM JSVal
getDocument = jsg "document"
-----------------------------------------------------------------------------
-- | Returns an Element object representing the element whose id property matches
-- the specified string.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById>
getElementById :: MisoString -> JSM JSVal
getElementById e = getDocument # "getElementById" $ [e]
-----------------------------------------------------------------------------
-- | Diff two virtual DOMs
diff
  :: Object
  -- ^ current object
  -> Object
  -- ^ new object
  -> JSVal
  -- ^ parent node
  -> JSM ()
diff (Object a) (Object b) c = do
  moduleExports <- jsg "module" ! "exports"
  void $ moduleExports # "diff" $ [a,b,c]
-----------------------------------------------------------------------------
-- | Helper function for converting Integral types to JavaScript strings
integralToJSString :: Integral a => a -> MisoString
integralToJSString = pack . show . toInteger
-----------------------------------------------------------------------------
-- | Helper function for converting RealFloat types to JavaScript strings
realFloatToJSString :: RealFloat a => a -> MisoString
realFloatToJSString x = (pack . show) (realToFrac x :: Double)
-----------------------------------------------------------------------------
-- | Helper function for converting RealFloat types to JavaScript strings
jsStringToDouble :: MisoString -> Double
jsStringToDouble = read . unpack
-----------------------------------------------------------------------------
-- | Initialize event delegation from a mount point.
delegateEvent :: JSVal -> JSVal -> Bool -> JSM JSVal -> JSM ()
delegateEvent mountPoint events debug getVTree =
  delegate mountPoint events debug =<< function handler
    where
      handler _ _ [] = error "delegate: no args - impossible state"
      handler _ _ (continuation : _) =
        void (call continuation global =<< getVTree)
-----------------------------------------------------------------------------
-- | Deinitialize event delegation from a mount point.
undelegateEvent :: JSVal -> JSVal -> Bool -> JSM JSVal -> JSM ()
undelegateEvent mountPoint events debug getVTree =
  undelegate mountPoint events debug =<< function handler
    where
      handler _ _ [] = error "undelegate: no args - impossible state"
      handler _ _ (continuation : _) =
        void (call continuation global =<< getVTree)
-----------------------------------------------------------------------------
-- | Call 'delegateEvent' JavaScript function
delegate :: JSVal -> JSVal -> Bool -> Function -> JSM ()
delegate mountPoint events debug callback = do
  d <- toJSVal debug
  cb <- toJSVal callback
  moduleExports <- jsg "module" ! "exports"
  void $ moduleExports # "delegate" $ [mountPoint,events,cb,d]
-----------------------------------------------------------------------------
undelegate :: JSVal -> JSVal -> Bool -> Function -> JSM ()
undelegate mountPoint events debug callback = do
  d <- toJSVal debug
  cb <- toJSVal callback
  moduleExports <- jsg "module" ! "exports"
  void $ moduleExports # "undelegate" $ [mountPoint,events,cb,d]
-----------------------------------------------------------------------------
-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
--
-- <https://en.wikipedia.org/wiki/Hydration_(web_development)>
-- 
hydrate :: Bool -> JSVal -> JSVal -> JSM ()
hydrate logLevel mountPoint vtree = void $ do
  ll <- toJSVal logLevel
  moduleExports <- jsg "module" ! "exports"
  void $ moduleExports # "hydrate" $ [ll, mountPoint, vtree]
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
focus :: MisoString -> JSM ()
focus x = do
  moduleExports <- jsg "module" ! "exports"
  el <- toJSVal x
  delay <- toJSVal (50 :: Int)
  void $ moduleExports # "callFocus" $ [el,delay]
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
blur :: MisoString -> JSM ()
blur x = do
  moduleExports <- jsg "module" ! "exports"
  el <- toJSVal x
  delay <- toJSVal (50 :: Int)
  void $ moduleExports # "callBlur" $ [el,delay]
-----------------------------------------------------------------------------
-- | Calls @document.getElementById(id).scrollIntoView()@
scrollIntoView :: MisoString -> JSM ()
scrollIntoView elId = do
  el <- jsg "document" # "getElementById" $ [elId]
  _ <- el # "scrollIntoView" $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Calls the @alert()@ function.
alert :: MisoString -> JSM ()
alert a = () <$ jsg1 "alert" a
-----------------------------------------------------------------------------
-- | Calls the @location.reload()@ function.
reload :: JSM ()
reload = void $ jsg "location" # "reload" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Sets the body with data-component-id
setBodyComponent :: MisoString -> JSM ()
setBodyComponent name = do
  component <- toJSVal name
  moduleExports <- jsg "module" ! "exports"
  void $ moduleExports # "setBodyComponent" $ [component]
-----------------------------------------------------------------------------
