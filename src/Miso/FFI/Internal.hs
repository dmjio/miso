-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.Internal
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal FFI functions for browser / device interaction.
--
----------------------------------------------------------------------------
module Miso.FFI.Internal
   ( JSM
   -- * Concurrency
   , forkJSM
   -- * Callbacks
   , syncCallback
   , syncCallback1
   , syncCallback2
   , asyncCallback
   , asyncCallback1
   , asyncCallback2
   , ghcjsPure
   -- * JSAddle
   , syncPoint
   -- * Events
   , addEventListener
   , removeEventListener
   , eventPreventDefault
   , eventStopPropagation
   -- * Window
   , windowAddEventListener
   , windowRemoveEventListener
   , windowInnerHeight
   , windowInnerWidth
   -- * Performance
   , now
   -- * Console
   , consoleWarn
   , consoleLog
   , consoleError
   , consoleLog'
   -- * JSON
   , jsonStringify
   , jsonParse
   , eventJSON
   -- * Object
   , set
   -- * DOM
   , getBody
   , getDocument
   , getContext
   , getElementById
   , diff
   , nextSibling
   , previousSibling
   -- * Conversions
   , integralToJSString
   , realFloatToJSString
   , jsStringToDouble
   -- * Events
   , delegateEvent
   , undelegateEvent
   -- * Isomorphic
   , hydrate
   -- * Misc.
   , focus
   , blur
   , scrollIntoView
   , alert
   , reload
   -- * CSS
   , addStyle
   , addStyleSheet
   -- * JS
   , addSrc
   , addScript
   , addScriptImportMap
   -- * XHR
   , fetch
   , CONTENT_TYPE(..)
   , shouldSync
   -- * Drawing
   , setDrawingContext
   , flush
   -- * Image
   , Image (..)
   , newImage
   -- * Date
   , Date (..)
   , newDate
   , toLocaleString
   -- * Utils
   , getMilliseconds
   , getSeconds
   -- * 'Miso.Types.Component'
   , getParentComponentId
   , getComponentId
   -- * Element
   , files
   , click
   -- * WebSocket
   , websocketConnect
   , websocketClose
   , websocketSend
   -- * SSE
   , eventSourceConnect
   , eventSourceClose
   -- * Blob
   , Blob (..)
   -- * FormData
   , FormData (..)
   -- * URLSearchParams
   , URLSearchParams (..)
   -- * File
   , File (..)
   -- * Uint8Array
   , Uint8Array (..)
   -- * ArrayBuffer
   , ArrayBuffer (..)
   -- * Navigator
   , geolocation
   , copyClipboard
   , getUserMedia
   , isOnLine
   -- * FileReader
   , FileReader (..)
   , newFileReader
   -- * fetch API
   , Response (..)
   ) where
-----------------------------------------------------------------------------
import           Control.Monad (foldM)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Control.Concurrent (ThreadId, forkIO)
import           Control.Monad (void, forM_, (<=<), when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson hiding (Object)
import qualified Data.Aeson as A
import qualified Data.JSString as JSS
#ifdef GHCJS_BOTH
import           Language.Javascript.JSaddle
#else
import           Language.Javascript.JSaddle hiding (Success)
#endif
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.String
----------------------------------------------------------------------------
-- | Run given t'JSM' action asynchronously, in a separate thread.
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
-- | Creates an asynchronous callback function with two arguments
asyncCallback2 :: (JSVal -> JSVal -> JSM ()) -> JSM Function
asyncCallback2 f = asyncFunction handle
  where
    handle _ _ []    = error "asyncCallback2: no args, impossible"
    handle _ _ [_]   = error "asyncCallback2: 1 arg, impossible"
    handle _ _ (x:y:_) = f x y
-----------------------------------------------------------------------------
-- | Creates a synchronous callback function with one argument
syncCallback1 :: (JSVal -> JSM ()) -> JSM Function
syncCallback1 f = function handle
  where
    handle _ _ []    = error "syncCallback1: no args, impossible"
    handle _ _ (x:_) = f x
-----------------------------------------------------------------------------
-- | Creates a synchronous callback function with two arguments
syncCallback2 :: (JSVal -> JSVal -> JSM ()) -> JSM Function
syncCallback2 f = function handle
  where
    handle _ _ []    = error "syncCallback2: no args, impossible"
    handle _ _ [_]   = error "syncCallback2: 1 arg, impossible"
    handle _ _ (x:y:_) = f x y
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
  -> JSM Function
addEventListener self name cb = do
  cb_ <- asyncFunction handle
  void $ self # "addEventListener" $ (name, cb_)
  pure cb_
    where
      handle _ _ []    = error "addEventListener: no args, impossible"
      handle _ _ (x:_) = cb x
-----------------------------------------------------------------------------
-- | Register an event listener on given target.
removeEventListener
  :: JSVal
  -- ^ Event target on which we want to register event listener
  -> MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> Function
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> JSM ()
removeEventListener self name cb =
  void $ self # "removeEventListener" $ (name, cb)
-----------------------------------------------------------------------------
-- | Registers an event listener on window
windowRemoveEventListener
  :: MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> Function
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> JSM ()
windowRemoveEventListener name cb = do
  win <- jsg "window"
  removeEventListener win name cb
-----------------------------------------------------------------------------
-- | Registers an event listener on window
windowAddEventListener
  :: MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> (JSVal -> JSM ())
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> JSM Function
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
jsonStringify :: ToJSON json => json -> JSM JSVal
{-# INLINE jsonStringify #-}
jsonStringify j = do
  v <- toJSVal (toJSON j)
  jsg "JSON" # "stringify" $ [v]
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
  moduleMiso <- jsg "miso"
  moduleMiso # "eventJSON" $ [x,y]
-----------------------------------------------------------------------------
-- | Retrieves a reference to document body.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/body>
getBody :: JSM JSVal
getBody = do
  ctx <- getContext
  ctx # "getRoot" $ ()
-----------------------------------------------------------------------------
-- | Retrieves a reference to the document.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document>
getDocument :: JSM JSVal
getDocument = jsg "document"
-----------------------------------------------------------------------------
-- | Retrieves a reference to the context.
--
-- This is a miso specific construct used to provide an identical interface
-- for both native (iOS / Android, etc.) and browser environments.
--
getContext :: JSM JSVal
getContext = jsg "miso" ! "context"
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
  moduleMiso <- jsg "miso"
  context <- getContext
  void $ moduleMiso # "diff" $ [a,b,c,context]
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
delegateEvent mountPoint events debug getVTree = do
  ctx <- getContext
  cb <- function handler
  delegate mountPoint events debug cb ctx
    where
      handler _ _ [] = error "delegate: no args - impossible state"
      handler _ _ (continuation : _) =
        void (call continuation global =<< getVTree)
-----------------------------------------------------------------------------
-- | Deinitialize event delegation from a mount point.
undelegateEvent :: JSVal -> JSVal -> Bool -> JSM JSVal -> JSM ()
undelegateEvent mountPoint events debug getVTree = do
  ctx <- getContext
  cb <- function handler
  undelegate mountPoint events debug cb ctx
    where
      handler _ _ [] = error "undelegate: no args - impossible state"
      handler _ _ (continuation : _) =
        void (call continuation global =<< getVTree)
-----------------------------------------------------------------------------
-- | Call 'delegateEvent' JavaScript function
delegate :: JSVal -> JSVal -> Bool -> Function -> JSVal -> JSM ()
delegate mountPoint events debug callback ctx = do
  d <- toJSVal debug
  cb <- toJSVal callback
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "delegate" $ [mountPoint,events,cb,d,ctx]
-----------------------------------------------------------------------------
undelegate :: JSVal -> JSVal -> Bool -> Function -> JSVal -> JSM ()
undelegate mountPoint events debug callback ctx = do
  d <- toJSVal debug
  cb <- toJSVal callback
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "undelegate" $ [mountPoint,events,cb,d,ctx]
-----------------------------------------------------------------------------
-- | Copies DOM pointers into virtual dom entry point into isomorphic javascript
--
-- See [hydration](https://en.wikipedia.org/wiki/Hydration_(web_development))
--
hydrate :: Bool -> JSVal -> JSVal -> JSM ()
hydrate logLevel mountPoint vtree = void $ do
  ll <- toJSVal logLevel
  context <- getContext
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "hydrate" $ [ll, mountPoint, vtree, context]
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
focus :: MisoString -> JSM ()
focus x = do
  moduleMiso <- jsg "miso"
  el <- toJSVal x
  delay <- toJSVal (50 :: Int)
  void $ moduleMiso # "callFocus" $ [el,delay]
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
blur :: MisoString -> JSM ()
blur x = do
  moduleMiso <- jsg "miso"
  el <- toJSVal x
  delay <- toJSVal (50 :: Int)
  void $ moduleMiso # "callBlur" $ [el,delay]
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
-- | Appends a 'Miso.Html.Element.style_' element containing CSS to 'Miso.Html.Element.head_'
--
-- > addStyle "body { background-color: green; }"
--
-- > <head><style>body { background-color: green; }</style></head>
--
addStyle :: MisoString -> JSM JSVal
addStyle css = do
  style <- jsg "document" # "createElement" $ ["style"]
  (style <# "innerHTML") css
  jsg "document" ! "head" # "appendChild" $ [style]
-----------------------------------------------------------------------------
-- | Appends a 'Miso.Html.Element.script_' element containing JS to 'Miso.Html.Element.head_'
--
-- > addScript False "function () { alert('hi'); }"
--
addScript :: Bool -> MisoString -> JSM JSVal
addScript useModule js_ = do
  script <- jsg "document" # "createElement" $ ["script"]
  when useModule $ (script <# "type") "module"
  (script <# "innerHTML") js_
  jsg "document" ! "head" # "appendChild" $ [script]
-----------------------------------------------------------------------------
-- | Appends a 'Miso.Html.Element.script_' element containing a JS import map.
--
-- > addScript "{ \"import\" : { \"three\" : \"url\" } }"
--
addScriptImportMap :: MisoString -> JSM JSVal
addScriptImportMap impMap = do
  script <- jsg "document" # "createElement" $ ["script"]
  (script <# "type") "importmap"
  (script <# "innerHTML") impMap
  jsg "document" ! "head" # "appendChild" $ [script]
-----------------------------------------------------------------------------
-- | Appends a \<script\> element to 'Miso.Html.Element.head_'
--
-- > addSrc "https://example.com/script.js"
--
addSrc :: MisoString -> JSM JSVal
addSrc url = do
  link <- jsg "document" # "createElement" $ ["script"]
  _ <- link # "setAttribute" $ ["src", fromMisoString url]
  jsg "document" ! "head" # "appendChild" $ [link]
-----------------------------------------------------------------------------
-- | Appends a StyleSheet 'Miso.Html.Element.link_' element to 'Miso.Html.Element.head_'
-- The 'Miso.Html.Element.link_' tag will contain a URL to a CSS file.
--
-- > addStyleSheet "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css"
--
-- > <head><link href="https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css" ref="stylesheet"></head>
--
addStyleSheet :: MisoString -> JSM JSVal
addStyleSheet url = do
  link <- jsg "document" # "createElement" $ ["link"]
  _ <- link # "setAttribute" $ ["rel","stylesheet"]
  _ <- link # "setAttribute" $ ["href", fromMisoString url]
  jsg "document" ! "head" # "appendChild" $ [link]
-----------------------------------------------------------------------------
-- | Retrieve JSON via Fetch API
--
-- Basic GET of JSON using Fetch API, will be expanded upon.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API>
--
fetch
  :: (FromJSVal success, FromJSVal error)
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ method
  -> Maybe JSVal
  -- ^ body
  -> [(MisoString, MisoString)]
  -- ^ headers
  -> (Response success -> JSM ())
  -- ^ successful callback
  -> (Response error -> JSM ())
  -- ^ errorful callback
  -> CONTENT_TYPE
  -- ^ content type
  -> JSM ()
fetch url method maybeBody requestHeaders successful errorful type_ = do
  successful_ <- toJSVal =<< asyncCallback1 (successful <=< fromJSValUnchecked)
  errorful_ <- toJSVal =<< asyncCallback1 (errorful <=< fromJSValUnchecked)
  moduleMiso <- jsg "miso"
  url_ <- toJSVal url
  method_ <- toJSVal method
  body_ <- toJSVal maybeBody
  Object headers_ <- do
    o <- create
    forM_ requestHeaders $ \(k,v) -> set k v o
    pure o
  typ <- toJSVal type_
  void $ moduleMiso # "fetchCore" $
    [ url_
    , method_
    , body_
    , headers_
    , successful_
    , errorful_
    , typ
    ]
-----------------------------------------------------------------------------
-- | List of possible content types that are available for use with the fetch API
data CONTENT_TYPE
  = JSON
  | ARRAY_BUFFER
  | TEXT
  | BLOB
  | BYTES
  | FORM_DATA
  | NONE
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal CONTENT_TYPE where
  toJSVal = \case
    JSON ->
      toJSVal "json"
    ARRAY_BUFFER ->
      toJSVal "arrayBuffer"
    TEXT ->
      toJSVal "text"
    BLOB ->
      toJSVal "blob"
    BYTES ->
      toJSVal "bytes"
    FORM_DATA ->
      toJSVal "formData"
    NONE ->
      toJSVal "none"
-----------------------------------------------------------------------------
-- | shouldSync
--
-- Used to set whether or not the current VNode should enter the @syncChildren@
-- function during diffing. The criteria for entrance is that all children
-- have a populated 'Miso.Property.key_' node. We can determine this property more efficiently
-- at tree construction time rather than dynamic detection during diffing.
--
shouldSync :: JSVal -> JSM Bool
shouldSync vnode = do
  returnValue <- jsg "miso" # "shouldSync" $ [vnode]
  fromJSValUnchecked returnValue
-----------------------------------------------------------------------------
-- | Flush is used to force a draw of the render tree. This is currently
-- only used when targeting platforms other than the browser (like mobile).
flush :: JSM ()
flush = do
  context <- jsg "miso" ! "context"
  void $ context # "flush" $ ([] :: [JSVal])
-----------------------------------------------------------------------------
newtype Image = Image JSVal
  deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
instance FromJSVal Image where
  fromJSVal = pure . pure . Image
-----------------------------------------------------------------------------
-- | Smart constructor for building a t'Image' w/ 'Miso.Html.Property.src_' 'Miso.Types.Attribute'.
newImage :: MisoString -> JSM Image
newImage url = do
  img <- new (jsg "Image") ([] :: [MisoString])
  img <# "src" $ url
  pure (Image img)
-----------------------------------------------------------------------------
-- | Used to select a drawing context. Users can override the default DOM renderer
-- by implementing their own Context, and exporting it to the global scope. This
-- opens the door to different rendering engines, ala [miso-lynx](https://github.com/haskell-miso/miso-lynx).
setDrawingContext :: MisoString -> JSM ()
setDrawingContext rendererName =
  void $ jsg "miso" # "setDrawingContext" $ [rendererName]
-----------------------------------------------------------------------------
-- | The [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) type
newtype Date = Date JSVal
  deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
-- | Smart constructor for a t'Date'
newDate :: JSM Date
newDate = Date <$> new (jsg "Date") ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Date conversion function to produce a locale
toLocaleString :: Date -> JSM MisoString
toLocaleString date = fromJSValUnchecked =<< do
  date # "toLocaleString" $ ()
-----------------------------------------------------------------------------
-- | Retrieves current milliseconds from t'Date'
getMilliseconds :: Date -> JSM Double
getMilliseconds date =
  fromJSValUnchecked =<< do
    date # "getMilliseconds" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Retrieves current seconds from t'Date'
getSeconds :: Date -> JSM Double
getSeconds date =
  fromJSValUnchecked =<< do
    date # "getSeconds" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Climb the tree, get the parent.
getParentComponentId :: JSVal -> JSM (Maybe Int)
getParentComponentId domRef =
  fromJSVal =<< do
    jsg "miso" # "getParentComponentId" $ [domRef]
-----------------------------------------------------------------------------
-- | Get access to the 'Miso.Effect.ComponentId'
-- N.B. you * must * call this on the DOMRef, otherwise, problems.
-- For use in 'Miso.Event.onMounted', etc.
getComponentId :: JSVal -> JSM Int
getComponentId vtree = fromJSValUnchecked =<< vtree ! "componentId"
-----------------------------------------------------------------------------
-- | Fetch next sibling DOM node
--
-- @since 1.9.0.0
nextSibling :: JSVal -> JSM JSVal
nextSibling domRef = domRef ! "nextSibling"
-----------------------------------------------------------------------------
-- | Fetch previous sibling DOM node
--
-- @since 1.9.0.0
previousSibling :: JSVal -> JSM JSVal
previousSibling domRef = domRef ! "previousSibling"
-----------------------------------------------------------------------------
-- | When working with /<input>/ of type="file", this is useful for
-- extracting out the selected files.
--
-- @
--   update (InputClicked inputElement) = withSink $ \sink -> do
--      files_ <- files inputElement
--      forM_ files_ $ \file -> sink (Upload file)
--   update (Upload file) = do
--      fetch "https://localhost:8080/upload" "POST" (Just file) []
--        Successful Errorful
-- @
--
-- @since 1.9.0.0
files :: JSVal -> JSM [JSVal]
files domRef = fromJSValUnchecked =<< domRef ! "files"
-----------------------------------------------------------------------------
-- | Simulates a click event
--
-- > button & click ()
--
-- @since 1.9.0.0
click :: () -> JSVal -> JSM ()
click () domRef = void $ domRef # "click" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Get Camera on user's device
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia>
--
getUserMedia
  :: Bool
  -- ^ video
  -> Bool
  -- ^ audio
  -> (JSVal -> JSM ())
  -- ^ successful
  -> (JSVal -> JSM ())
  -- ^ errorful
  -> JSM ()
getUserMedia video audio successful errorful = do
  params <- create
  set (ms "video") video params
  set (ms "audio") audio params
  devices <- jsg "navigator" ! "mediaDevices"
  promise <- devices # "getUserMedia" $ [params]
  successfulCallback <- asyncCallback1 successful
  void $ promise # "then" $ [successfulCallback]
  errorfulCallback <- asyncCallback1 errorful
  void $ promise # "catch" $ [errorfulCallback]
-----------------------------------------------------------------------------
-- | Copy clipboard
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia>
--
copyClipboard
  :: MisoString
  -- ^ Text to copy
  -> JSM ()
  -- ^ successful
  -> (JSVal -> JSM ())
  -- ^ errorful
  -> JSM ()
copyClipboard txt successful errorful = do
  clipboard <- jsg "navigator" ! "clipboard"
  promise <- clipboard # "writeText" $ [txt]
  successfulCallback <- asyncCallback successful
  void $ promise # "then" $ [successfulCallback]
  errorfulCallback <- asyncCallback1 errorful
  void $ promise # "catch" $ [errorfulCallback]
-----------------------------------------------------------------------------
-- | Establishes a @WebSocket@ connection
websocketConnect
  :: MisoString
  -> JSM ()
  -> (JSVal -> JSM ())
  -> Maybe (JSVal -> JSM ())
  -> Maybe (JSVal -> JSM ())
  -> Maybe (JSVal -> JSM ())
  -> Maybe (JSVal -> JSM ())
  -> (JSVal -> JSM ())
  -> Bool
  -> JSM JSVal
websocketConnect
  url onOpen onClose
  onMessageText onMessageJSON
  onMessageBLOB onMessageArrayBuffer
  onError textOnly = do
    url_ <- toJSVal url
    onOpen_ <- toJSVal =<< asyncCallback onOpen
    onClose_ <- toJSVal =<< asyncCallback1 onClose
    onMessageText_ <- withMaybe onMessageText
    onMessageJSON_ <- withMaybe onMessageJSON
    onMessageBLOB_ <- withMaybe onMessageBLOB
    onMessageArrayBuffer_ <- withMaybe onMessageArrayBuffer
    onError_ <- toJSVal =<< asyncCallback1 onError
    textOnly_ <- toJSVal textOnly
    jsg "miso" # "websocketConnect" $
      [ url_
      , onOpen_
      , onClose_
      , onMessageText_
      , onMessageJSON_
      , onMessageBLOB_
      , onMessageArrayBuffer_
      , onError_
      , textOnly_
      ]
  where
    withMaybe Nothing = pure jsNull
    withMaybe (Just f) = toJSVal =<< asyncCallback1 f
-----------------------------------------------------------------------------
websocketClose :: JSVal -> JSM ()
websocketClose websocket = void $ do
  jsg "miso" # "websocketClose" $ [websocket]
-----------------------------------------------------------------------------
websocketSend :: JSVal -> JSVal -> JSM ()
websocketSend websocket message = void $ do
  jsg "miso" # "websocketSend" $ [websocket, message]
-----------------------------------------------------------------------------
eventSourceConnect
  :: MisoString
  -> JSM ()
  -> Maybe (JSVal -> JSM ())
  -> Maybe (JSVal -> JSM ())
  -> (JSVal -> JSM ())
  -> Bool
  -> JSM JSVal
eventSourceConnect url onOpen onMessageText onMessageJSON onError textOnly = do
  onOpen_ <- asyncCallback onOpen
  onMessageText_ <- withMaybe onMessageText
  onMessageJSON_ <- withMaybe onMessageJSON
  onError_ <- asyncCallback1 onError
  textOnly_ <- toJSVal textOnly
  jsg "miso" # "eventSourceConnect" $
    (url, onOpen_, onMessageText_, onMessageJSON_, onError_, textOnly_)
    where
      withMaybe Nothing = pure jsNull
      withMaybe (Just f) = toJSVal =<< asyncCallback1 f
-----------------------------------------------------------------------------
eventSourceClose :: JSVal -> JSM ()
eventSourceClose eventSource = void $ do
  jsg "miso" # "eventSourceClose" $ [eventSource]
-----------------------------------------------------------------------------
-- | Navigator function to query the current online status of the user's computer
--
-- See [navigator.onLine](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/onLine)
--
isOnLine :: JSM Bool
isOnLine = fromJSValUnchecked =<< jsg "navigator" ! "onLine"
-----------------------------------------------------------------------------
-- | [Blob](https://developer.mozilla.org/en-US/docs/Web/API/FormData)
newtype Blob = Blob JSVal
  deriving ToJSVal
-----------------------------------------------------------------------------
instance FromJSVal Blob where
  fromJSVal = pure . pure . Blob
-----------------------------------------------------------------------------
-- | [FormData](https://developer.mozilla.org/en-US/docs/Web/API/FormData)
newtype FormData = FormData JSVal
  deriving ToJSVal
-----------------------------------------------------------------------------
instance FromJSVal FormData where
  fromJSVal = pure . pure . FormData
-----------------------------------------------------------------------------
instance FromJSVal ArrayBuffer where
  fromJSVal = pure . pure . ArrayBuffer
-----------------------------------------------------------------------------
-- | [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/API/ArrayBuffer)
newtype ArrayBuffer = ArrayBuffer JSVal
  deriving ToJSVal
-----------------------------------------------------------------------------
geolocation :: (JSVal -> JSM ()) -> (JSVal -> JSM ()) -> JSM ()
geolocation successful errorful = do
  geo <- jsg "navigator" ! "geolocation"
  cb1 <- asyncCallback1 successful
  cb2 <- asyncCallback1 errorful
  void $ geo # "getCurrentPosition" $ (cb1, cb2)
-----------------------------------------------------------------------------
-- | [File](https://developer.mozilla.org/en-US/docs/Web/API/File)
newtype File = File JSVal
  deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
instance FromJSVal File where
  fromJSVal = pure . pure . File
-----------------------------------------------------------------------------
-- | [Uint8Array](https://developer.mozilla.org/en-US/docs/Web/API/Uint8Array)
newtype Uint8Array = Uint8Array JSVal
  deriving ToJSVal
-----------------------------------------------------------------------------
instance FromJSVal Uint8Array where
  fromJSVal = pure . pure . Uint8Array
-----------------------------------------------------------------------------
-- | [FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
newtype FileReader = FileReader JSVal
  deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
instance FromJSVal FileReader where
  fromJSVal = pure . pure . FileReader
-----------------------------------------------------------------------------
-- | [URLSearchParams](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams)
newtype URLSearchParams = URLSearchParams JSVal
  deriving (ToJSVal, MakeObject)
-----------------------------------------------------------------------------
instance FromJSVal URLSearchParams where
  fromJSVal = pure . pure . URLSearchParams
-----------------------------------------------------------------------------
-- | Smart constructor for building a t'FileReader'
newFileReader :: JSM FileReader
newFileReader = do
  reader <- new (jsg "FileReader") ([] :: [MisoString])
  pure (FileReader reader)
-----------------------------------------------------------------------------
-- | Type returned from a 'fetch' request 
data Response body
  = Response
  { status :: Maybe Int
    -- ^ HTTP status code
  , headers :: Map MisoString MisoString
    -- ^ Response headers 
  , errorMessage :: Maybe MisoString
    -- ^ Optional error message
  , body :: body
    -- ^ Response body
  }
-----------------------------------------------------------------------------
instance Functor Response where
  fmap f response@Response { body } = response { body = f body }
-----------------------------------------------------------------------------
instance FromJSVal body => FromJSVal (Response body) where
  fromJSVal o = do
    status_ <- fromJSVal =<< getProp (ms "status") (Object o)
    headers_ <- fromJSVal =<< getProp (ms "headers") (Object o)
    errorMessage_ <- fromJSVal =<< getProp (ms "error") (Object o)
    body_ <- fromJSVal =<< getProp (ms "body") (Object o)
    pure (Response <$> status_ <*> headers_ <*> errorMessage_ <*> body_)
-----------------------------------------------------------------------------
instance FromJSVal (Map MisoString MisoString) where
  fromJSVal o = pure <$> do foldM populate M.empty =<< listProps (Object o)
    where
      populate m k = do
        v <- fromJSValUnchecked =<< getProp k (Object o)
        pure (M.insert k v m)
-----------------------------------------------------------------------------
