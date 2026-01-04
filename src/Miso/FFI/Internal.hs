-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
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
   ( -- * Callbacks
     syncCallback
   , syncCallback1
   , syncCallback2
   , asyncCallback
   , asyncCallback1
   , asyncCallback2
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
   , setValue
   -- * DOM
   , getBody
   , getDocument
   , getDrawingContext
   , getHydrationContext
   , getEventContext
   , getElementById
  , removeChild
  , getHead
   , diff
   , nextSibling
   , previousSibling
   , getProperty
   , callFunction
   , castJSVal
   -- * Events
   , delegateEvent
   , undelegateEvent
   , dispatchEvent
   , newEvent
   , newCustomEvent
   -- * Isomorphic
   , hydrate
   -- * Misc.
   , focus
   , blur
   , select
   , setSelectionRange
   , scrollIntoView
   , alert
   , locationReload
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
   -- * Fetch API
   , Response (..)
   -- * Event
   , Event (..)
   -- * Class
   , populateClass
   , updateRef
   -- * Inline JS
   , inline
   ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Control.Monad (void, foldM, forM_, (<=<), when)
import           Data.Aeson hiding (Object, ToArgs)
import qualified Data.Aeson as A
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.String
import           Miso.Effect (DOMRef)
-----------------------------------------------------------------------------
-- | Set property on object
set :: ToJSVal v => MisoString -> v -> Object -> IO ()
set k v o = do
  v' <- toJSVal v
  setProp (fromMisoString k) v' o
-----------------------------------------------------------------------------
-- | Get a property of a 'JSVal'
--
-- Example usage:
--
-- > Just (value :: String) <- fromJSVal =<< getProperty domRef "value"
getProperty :: JSVal -> MisoString -> IO JSVal
getProperty = (!)
-----------------------------------------------------------------------------
-- | Calls a function on a 'JSVal'
--
-- Example usage:
-- 
-- > callFunction domRef "focus" ()
-- > callFunction domRef "setSelectionRange" (0, 3, "none")
callFunction :: (ToArgs args) => JSVal -> MisoString -> args -> IO JSVal
callFunction = (#)
-----------------------------------------------------------------------------
-- | Marshalling of 'JSVal', useful for 'getProperty'
castJSVal :: (FromJSVal a) => JSVal -> IO (Maybe a)
castJSVal = fromJSVal
-----------------------------------------------------------------------------
-- | Register an event listener on given target.
addEventListener
  :: JSVal
  -- ^ Event target on which we want to register event listener
  -> MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> (JSVal -> IO ())
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> IO Function
addEventListener self name cb = do
  cb_ <- Function <$> asyncCallback1 cb
  void $ self # "addEventListener" $ (name, cb_)
  pure cb_
-----------------------------------------------------------------------------
-- | Removes an event listener from given target.
removeEventListener
  :: JSVal
  -- ^ Event target from which we want to remove event listener
  -> MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> Function
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> IO ()
removeEventListener self name cb =
  void $ self # "removeEventListener" $ (name, cb)
-----------------------------------------------------------------------------
-- | Removes an event listener from window
windowRemoveEventListener
  :: MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> Function
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> IO ()
windowRemoveEventListener name cb = do
  win <- jsg "window"
  removeEventListener win name cb
-----------------------------------------------------------------------------
-- | Registers an event listener on window
windowAddEventListener
  :: MisoString
  -- ^ Type of event to listen to (e.g. "click")
  -> (JSVal -> IO ())
  -- ^ Callback which will be called when the event occurs,
  -- the event will be passed to it as a parameter.
  -> IO Function
windowAddEventListener name cb = do
  win <- jsg "window"
  addEventListener win name cb
-----------------------------------------------------------------------------
-- | Stop propagation of events
eventStopPropagation :: JSVal -> IO ()
eventStopPropagation e = do
  _ <- e # "stopPropagation" $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Prevent default event behavior
eventPreventDefault :: JSVal -> IO ()
eventPreventDefault e = do
  _ <- e # "preventDefault" $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Retrieves the height (in pixels) of the browser window viewport including,
-- if rendered, the horizontal scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerHeight>
windowInnerHeight :: IO Int
windowInnerHeight = fromJSValUnchecked =<< jsg "window" ! "innerHeight"
-----------------------------------------------------------------------------
-- | Retrieves the width (in pixels) of the browser window viewport including
-- if rendered, the vertical scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth>
windowInnerWidth :: IO Int
windowInnerWidth =
  fromJSValUnchecked =<< jsg "window" ! "innerWidth"
-----------------------------------------------------------------------------
-- | Retrieve high resolution time stamp
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Performance/now>
now :: IO Double
now = fromJSValUnchecked =<< (jsg "performance" # "now" $ ())
-----------------------------------------------------------------------------
-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
--
-- Console logging of JavaScript strings.
consoleLog :: MisoString -> IO ()
consoleLog v = do
  _ <- jsg "console" # "log" $ [ms v]
  pure ()
-----------------------------------------------------------------------------
-- | Outputs a warning message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/warn>
--
-- Console logging of JavaScript strings.
consoleWarn :: MisoString -> IO ()
consoleWarn v = do
  _ <- jsg "console" # "warn" $ [ms v]
  pure ()
-----------------------------------------------------------------------------
-- | Outputs an error message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/error>
--
-- Console logging of JavaScript strings.
consoleError :: MisoString -> IO ()
consoleError v = do
  _ <- jsg "console" # "error" $ [ms v]
  pure ()
-----------------------------------------------------------------------------
-- | Console-logging of JSVal
consoleLog' :: ToArgs a => a -> IO ()
consoleLog' args' = do
  args <- toArgs args'
  _ <- jsg "console" # "log" $ args
  pure ()
-----------------------------------------------------------------------------
-- | Encodes a Haskell object as a JSON string by way of a JavaScript object
jsonStringify :: ToJSON json => json -> IO JSVal
{-# INLINE jsonStringify #-}
jsonStringify j = do
  v <- toJSVal (toJSON j)
  jsg "JSON" # "stringify" $ [v]
-----------------------------------------------------------------------------
-- | Parses a JavaScript value into a Haskell type using JSON conversion
jsonParse :: FromJSON json => JSVal -> IO json
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
    -> IO JSVal
eventJSON x y = do
  moduleMiso <- jsg "miso"
  moduleMiso # "eventJSON" $ [x,y]
-----------------------------------------------------------------------------
-- | Used to update the JavaScript reference post-diff.
updateRef
    :: ToJSVal val
    => val
    -> val
    -> IO ()
updateRef jsval1 jsval2 = do
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "updateRef" $ (jsval1, jsval2)
-----------------------------------------------------------------------------
-- | Convenience function to write inline javascript.
--
-- This function takes as arguments a JavaScript object and makes the
-- keys available in the function body.
--
-- @
--
--  data Person = Person { name :: MisoString, age :: Int }
--    deriving stock (Generic)
--    deriving anyclass (ToJSVal, ToObject)
--
-- logNameGetAge :: Person -> IO Int
-- logNameGetAge = inline
--   """
--   console.log('name', name);
--   return age;
--   """
--
-- @
--
inline
    :: (FromJSVal return, ToObject object)
    => MisoString
    -> object
    -> IO return
inline code o = do
  moduleMiso <- jsg "miso"
  Object obj <- toObject o
  fromJSValUnchecked =<< do
    moduleMiso # "inline" $ (code, obj)
-----------------------------------------------------------------------------
-- | Populate the 'Miso.Html.Property.classList' Set on the virtual DOM.
populateClass
    :: JSVal
    -- ^ Node
    -> [MisoString]
    -- ^ classes
    -> IO ()
populateClass domRef classes = do
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "populateClass" $ (domRef, classes)
-----------------------------------------------------------------------------
-- | Retrieves a reference to document body.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/body>
getBody :: IO JSVal
getBody = do
  ctx <- getDrawingContext
  ctx # "getRoot" $ ()
-----------------------------------------------------------------------------
-- | Retrieves a reference to the document.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document>
getDocument :: IO JSVal
getDocument = jsg "document"
-----------------------------------------------------------------------------
-- | Retrieves a reference to the drawing context.
--
-- This is a miso specific construct used to provide an identical interface
-- for both native (iOS / Android, etc.) and browser environments.
--
getDrawingContext :: IO JSVal
getDrawingContext = jsg "miso" ! "drawingContext"
-----------------------------------------------------------------------------
-- | Retrieves a reference to the event context.
--
-- This is a miso specific construct used to provide an identical interface
-- for both native (iOS / Android, etc.) and browser environments.
--
getEventContext :: IO JSVal
getEventContext = jsg "miso" ! "eventContext"
-----------------------------------------------------------------------------
-- | Retrieves a reference to the hydration context.
--
-- This is a miso specific construct used to provide an identical interface
-- for both native (iOS / Android, etc.) and browser environments.
--
getHydrationContext :: IO JSVal
getHydrationContext = jsg "miso" ! "hydrationContext"
-----------------------------------------------------------------------------
-- | Returns an Element object representing the element whose id property matches
-- the specified string.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById>
getElementById :: MisoString -> IO JSVal
getElementById e = getDocument # "getElementById" $ [e]
-----------------------------------------------------------------------------
-- | Retrieves a reference to the renderer's "head" mount.
--
-- Calls @miso.drawingContext.getHead()@.
--
-- Note: custom renderers should implement this method.
--
-- @since 1.9.0.0
getHead :: IO DOMRef
getHead = do
  context <- getDrawingContext
  context # "getHead" $ ()
-----------------------------------------------------------------------------
-- | Removes a child node from a parent node.
--
-- Calls @miso.drawingContext.removeChild(parent, child)@.
--
-- @since 1.9.0.0
removeChild :: DOMRef -> DOMRef -> IO ()
removeChild parent child = do
  context <- getDrawingContext
  void $ context # "removeChild" $ (parent, child)
-----------------------------------------------------------------------------
-- | Diff two virtual DOMs
diff
  :: Object
  -- ^ current object
  -> Object
  -- ^ new object
  -> JSVal
  -- ^ parent node
  -> IO ()
diff (Object a) (Object b) c = do
  moduleMiso <- jsg "miso"
  context <- getDrawingContext
  void $ moduleMiso # "diff" $ [a,b,c,context]
-----------------------------------------------------------------------------
-- | Initialize event delegation from a mount point.
delegateEvent :: JSVal -> JSVal -> Bool -> IO JSVal -> IO ()
delegateEvent mountPoint events debug getVTree = do
  ctx <- getEventContext
  cb <- asyncCallback1 $ \continuation -> void (call continuation global =<< getVTree)
  delegate mountPoint events debug (Function cb) ctx
-----------------------------------------------------------------------------
-- | Deinitialize event delegation from a mount point.
undelegateEvent :: JSVal -> JSVal -> Bool -> IO JSVal -> IO ()
undelegateEvent mountPoint events debug getVTree = do
  ctx <- getEventContext
  cb <- asyncCallback1 $ \continuation -> void (call continuation global =<< getVTree)
  undelegate mountPoint events debug (Function cb) ctx
-----------------------------------------------------------------------------
-- | Call 'delegateEvent' JavaScript function
delegate :: JSVal -> JSVal -> Bool -> Function -> JSVal -> IO ()
delegate mountPoint events debug callback ctx = do
  d <- toJSVal debug
  cb <- toJSVal callback
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "delegate" $ [mountPoint,events,cb,d,ctx]
-----------------------------------------------------------------------------
undelegate :: JSVal -> JSVal -> Bool -> Function -> JSVal -> IO ()
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
hydrate :: Bool -> JSVal -> JSVal -> IO JSVal
hydrate logLevel mountPoint vtree = do
  ll <- toJSVal logLevel
  drawingContext <- getDrawingContext
  hydrationContext <- getHydrationContext
  moduleMiso <- jsg "miso"
  moduleMiso # "hydrate" $ (ll, mountPoint, vtree, hydrationContext, drawingContext)
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
focus :: MisoString -> IO ()
focus x = void $ jsg "miso" # "callFocus" $ (x, 50 :: Int)
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
blur :: MisoString -> IO ()
blur x = void $ jsg "miso" # "callBlur" $ (x, 50 :: Int)
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.querySelector('#' + id).select()@.
select :: MisoString -> IO ()
select x = void $ jsg "miso" # "callSelect" $ (x, 50 :: Int)
-----------------------------------------------------------------------------
-- | Fails silently if the element is not found.
--
-- Analogous to @document.querySelector('#' + id).setSelectionRange(start, end, \'none\')@.
setSelectionRange :: MisoString -> Int -> Int -> IO ()
setSelectionRange x start end = void $ jsg "miso" # "callSetSelectionRange" $ (x, start, end, 50 :: Int)
-----------------------------------------------------------------------------
-- | Calls @document.getElementById(id).scrollIntoView()@
scrollIntoView :: MisoString -> IO ()
scrollIntoView elId = do
  el <- jsg "document" # "getElementById" $ [elId]
  _ <- el # "scrollIntoView" $ ()
  pure ()
-----------------------------------------------------------------------------
-- | Calls the @alert()@ function.
alert :: MisoString -> IO ()
alert a = () <$ jsg1 "alert" a
-----------------------------------------------------------------------------
-- | Calls the @location.reload()@ function.
locationReload :: IO ()
locationReload = void $ jsg "location" # "reload" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Appends a 'Miso.Html.Element.style_' element containing CSS to 'Miso.Html.Element.head_'
--
-- > addStyle "body { background-color: green; }"
--
-- > <head><style>body { background-color: green; }</style></head>
--
addStyle :: MisoString -> IO JSVal
addStyle css = do
  context <- getDrawingContext
  head_ <- getHead
  style <- context # "createElement" $ ["style" :: MisoString]
  setField style "innerHTML" css
  void $ context # "appendChild" $ (head_, style)
  pure style
-----------------------------------------------------------------------------
-- | Appends a 'Miso.Html.Element.script_' element containing JS to 'Miso.Html.Element.head_'
--
-- > addScript False "function () { alert('hi'); }"
--
addScript :: Bool -> MisoString -> IO JSVal
addScript useModule js_ = do
  context <- getDrawingContext
  head_ <- getHead
  script <- context # "createElement" $ ["script" :: MisoString]
  when useModule $ setField script "type" ("module" :: MisoString)
  setField script "innerHTML" js_
  void $ context # "appendChild" $ (head_, script)
  pure script
-----------------------------------------------------------------------------
-- | Sets the @.value@ property on a 'DOMRef'.
--
-- Useful for resetting the @value@ property on an input element.
--
-- @
--   setValue domRef ("" :: MisoString)
-- @
--
setValue :: DOMRef -> MisoString -> IO ()
setValue domRef value = setField domRef "value" value
-----------------------------------------------------------------------------
-- | Appends a 'Miso.Html.Element.script_' element containing a JS import map.
--
-- > addScript "{ \"import\" : { \"three\" : \"url\" } }"
--
addScriptImportMap :: MisoString -> IO JSVal
addScriptImportMap impMap = do
  context <- getDrawingContext
  head_ <- getHead
  script <- context # "createElement" $ ["script" :: MisoString]
  setField script "type" ("importmap" :: MisoString)
  setField script "innerHTML" impMap
  void $ context # "appendChild" $ (head_, script)
  pure script
-----------------------------------------------------------------------------
-- | Appends a \<script\> element to 'Miso.Html.Element.head_'
--
-- > addSrc "https://example.com/script.js"
--
addSrc :: MisoString -> IO JSVal
addSrc url = do
  context <- getDrawingContext
  head_ <- getHead
  link <- context # "createElement" $ ["script" :: MisoString]
  _ <- link # "setAttribute" $ ["src", url]
  void $ context # "appendChild" $ (head_, link)
  pure link
-----------------------------------------------------------------------------
-- | Appends a StyleSheet 'Miso.Html.Element.link_' element to 'Miso.Html.Element.head_'
-- The 'Miso.Html.Element.link_' tag will contain a URL to a CSS file.
--
-- > addStyleSheet "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css"
--
-- > <head><link href="https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css" ref="stylesheet"></head>
--
addStyleSheet :: MisoString -> IO JSVal
addStyleSheet url = do
  context <- getDrawingContext
  head_ <- getHead
  link <- context # "createElement" $ ["link" :: MisoString]
  _ <- link # "setAttribute" $ ["rel","stylesheet" :: MisoString]
  _ <- link # "setAttribute" $ ["href", url]
  void $ context # "appendChild" $ (head_, link)
  pure link
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
  -> (Response success -> IO ())
  -- ^ successful callback
  -> (Response error -> IO ())
  -- ^ errorful callback
  -> CONTENT_TYPE
  -- ^ content type
  -> IO ()
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
      toJSVal ("json" :: MisoString)
    ARRAY_BUFFER ->
      toJSVal ("arrayBuffer" :: MisoString)
    TEXT ->
      toJSVal ("text" :: MisoString)
    BLOB ->
      toJSVal ("blob" :: MisoString)
    BYTES ->
      toJSVal ("bytes" :: MisoString)
    FORM_DATA ->
      toJSVal ("formData" :: MisoString)
    NONE ->
      toJSVal ("none" :: MisoString)
-----------------------------------------------------------------------------
-- | Flush is used to force a draw of the render tree. This is currently
-- only used when targeting platforms other than the browser (like mobile).
flush :: IO ()
flush = do
  context <- getDrawingContext
  void $ context # "flush" $ ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Type that holds an [Image](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/img).
newtype Image = Image JSVal
  deriving (ToJSVal, ToObject)
-----------------------------------------------------------------------------
instance FromJSVal Image where
  fromJSVal = pure . pure . Image
-----------------------------------------------------------------------------
-- | Smart constructor for building a t'Image' w/ 'Miso.Html.Property.src_' 'Miso.Types.Attribute'.
newImage :: MisoString -> IO Image
newImage url = do
  img <- new (jsg "Image") ([] :: [MisoString])
  setField img "src" url
  pure (Image img)
-----------------------------------------------------------------------------
-- | Used to select a drawing context. Users can override the default DOM renderer
-- by implementing their own Context, and exporting it to the global scope. This
-- opens the door to different rendering engines, ala [miso-lynx](https://github.com/haskell-miso/miso-lynx).
setDrawingContext :: MisoString -> IO ()
setDrawingContext rendererName =
  void $ jsg "miso" # "setDrawingContext" $ [rendererName]
-----------------------------------------------------------------------------
-- | The [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) type
newtype Date = Date JSVal
  deriving (ToJSVal, ToObject)
-----------------------------------------------------------------------------
-- | Smart constructor for a t'Date'
newDate :: IO Date
newDate = Date <$> new (jsg "Date") ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Date conversion function to produce a locale
toLocaleString :: Date -> IO MisoString
toLocaleString date = fromJSValUnchecked =<< do
  date # "toLocaleString" $ ()
-----------------------------------------------------------------------------
-- | Retrieves current milliseconds from t'Date'
getMilliseconds :: Date -> IO Double
getMilliseconds date =
  fromJSValUnchecked =<< do
    date # "getMilliseconds" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Retrieves current seconds from t'Date'
getSeconds :: Date -> IO Double
getSeconds date =
  fromJSValUnchecked =<< do
    date # "getSeconds" $ ([] :: [MisoString])
-----------------------------------------------------------------------------
-- | Fetch next sibling DOM node
--
-- @since 1.9.0.0
nextSibling :: JSVal -> IO JSVal
nextSibling domRef = domRef ! "nextSibling"
-----------------------------------------------------------------------------
-- | Fetch previous sibling DOM node
--
-- @since 1.9.0.0
previousSibling :: JSVal -> IO JSVal
previousSibling domRef = domRef ! "previousSibling"
-----------------------------------------------------------------------------
-- | When working with @\<input type="file"\>@, this is useful for
-- extracting out the selected files.
--
-- @
--   update (InputClicked inputElement) = withSink $ \\sink -> do
--      files_ <- files inputElement
--      forM_ files_ $ \\file -> sink (Upload file)
--   update (Upload file) = do
--      fetch \"https://localhost:8080\/upload\" \"POST\" (Just file) []
--        Successful Errorful
-- @
--
-- @since 1.9.0.0
files :: JSVal -> IO [JSVal]
files domRef = fromJSValUnchecked =<< domRef ! "files"
-----------------------------------------------------------------------------
-- | Simulates a click event
--
-- > button & click ()
--
-- @since 1.9.0.0
click :: () -> JSVal -> IO ()
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
  -> (JSVal -> IO ())
  -- ^ successful
  -> (JSVal -> IO ())
  -- ^ errorful
  -> IO ()
getUserMedia video audio successful errorful = do
  params <- create
  set "video" video params
  set "audio" audio params
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
  -> IO ()
  -- ^ successful
  -> (JSVal -> IO ())
  -- ^ errorful
  -> IO ()
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
  -> IO ()
  -> (JSVal -> IO ())
  -> Maybe (JSVal -> IO ())
  -> Maybe (JSVal -> IO ())
  -> Maybe (JSVal -> IO ())
  -> Maybe (JSVal -> IO ())
  -> (JSVal -> IO ())
  -> Bool
  -> IO JSVal
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
    withMaybe (Just f) = asyncCallback1 f
-----------------------------------------------------------------------------
websocketClose :: JSVal -> IO ()
websocketClose websocket = void $ do
  jsg "miso" # "websocketClose" $ [websocket]
-----------------------------------------------------------------------------
websocketSend :: JSVal -> JSVal -> IO ()
websocketSend websocket message = void $ do
  jsg "miso" # "websocketSend" $ [websocket, message]
-----------------------------------------------------------------------------
eventSourceConnect
  :: MisoString
  -> IO ()
  -> Maybe (JSVal -> IO ())
  -> Maybe (JSVal -> IO ())
  -> (JSVal -> IO ())
  -> Bool
  -> IO JSVal
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
eventSourceClose :: JSVal -> IO ()
eventSourceClose eventSource = void $ do
  jsg "miso" # "eventSourceClose" $ [eventSource]
-----------------------------------------------------------------------------
-- | Navigator function to query the current online status of the user's computer
--
-- See [navigator.onLine](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/onLine)
--
isOnLine :: IO Bool
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
geolocation :: (JSVal -> IO ()) -> (JSVal -> IO ()) -> IO ()
geolocation successful errorful = do
  geo <- jsg "navigator" ! "geolocation"
  cb1 <- asyncCallback1 successful
  cb2 <- asyncCallback1 errorful
  void $ geo # "getCurrentPosition" $ (cb1, cb2)
-----------------------------------------------------------------------------
-- | [File](https://developer.mozilla.org/en-US/docs/Web/API/File)
newtype File = File JSVal
  deriving (ToJSVal, ToObject)
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
  deriving (ToJSVal, ToObject)
-----------------------------------------------------------------------------
instance FromJSVal FileReader where
  fromJSVal = pure . pure . FileReader
-----------------------------------------------------------------------------
-- | [URLSearchParams](https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams)
newtype URLSearchParams = URLSearchParams JSVal
  deriving (ToJSVal, ToObject)
-----------------------------------------------------------------------------
instance FromJSVal URLSearchParams where
  fromJSVal = pure . pure . URLSearchParams
-----------------------------------------------------------------------------
-- | Smart constructor for building a t'FileReader'
newFileReader :: IO FileReader
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
    status_ <- fromJSVal =<< getProp "status" (Object o)
    headers_ <- fromJSVal =<< getProp "headers" (Object o)
    errorMessage_ <- fromJSVal =<< getProp "error" (Object o)
    body_ <- fromJSVal =<< getProp "body" (Object o)
    pure (Response <$> status_ <*> headers_ <*> errorMessage_ <*> body_)
-----------------------------------------------------------------------------
instance FromJSVal (Map MisoString MisoString) where
  fromJSVal o = pure <$> do foldM populate M.empty =<< listProps (Object o)
    where
      populate m k = do
        v <- fromJSValUnchecked =<< getProp k (Object o)
        pure (M.insert k v m)
-----------------------------------------------------------------------------
-- | [Event](https://developer.mozilla.org/en-US/docs/Web/API/Event/Event)
newtype Event = Event JSVal
  deriving (ToJSVal)
-----------------------------------------------------------------------------
instance FromJSVal Event where
  fromJSVal = pure . Just . Event
-----------------------------------------------------------------------------
-- | Invokes [document.dispatchEvent](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/dispatchEvent)
--
-- @
--   update ChangeTheme = io_ $ do
--     themeEvent <- newEvent "basecoat:theme"
--     dispatchEvent themeEvent
-- @
--
dispatchEvent :: Event -> IO ()
dispatchEvent event = do
  doc <- getDocument
  _ <- doc # "dispatchEvent" $ [event]
  pure ()
-----------------------------------------------------------------------------
-- | Creates a new [Event](https://developer.mozilla.org/en-US/docs/Web/API/Event/Event)
--
-- @
--   update ChangeTheme = io_ $ do
--     themeEvent <- newEvent "basecoat:theme"
--     dispatchEvent themeEvent
-- @
--
newEvent :: ToArgs args => args -> IO Event
newEvent args = Event <$> new (jsg "Event") args
-----------------------------------------------------------------------------
-- | Creates a new [Event](https://developer.mozilla.org/en-US/docs/Web/API/Event/CustomEvent)
--
-- @
--   update ToggleSidebar = io_ $ do
--     themeEvent <- newCustomEvent "basecoat:sidebar"
--     dispatchEvent themeEvent
-- @
--
newCustomEvent :: ToArgs args => args -> IO Event
newCustomEvent args = Event <$> new (jsg "CustomEvent") args
-----------------------------------------------------------------------------
