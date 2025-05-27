-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.Internal
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.Internal
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
   , addStyle
   , addStyleSheet
   , fetchJSON
   , shouldSync
   , setComponent
   , Image (..)
   , newImage
   , Audio (..)
   , newAudio
   , play
   , volume
   , pause
   ) where
-----------------------------------------------------------------------------
import           Control.Concurrent (ThreadId, forkIO)
import           Control.Monad (void, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson hiding (Object)
import qualified Data.Aeson as A
import           Data.Maybe (isJust, fromMaybe)
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
  _ <- self # "addEventListener" $ (name, asyncFunction handle)
  pure ()
    where
      handle _ _ []    = error "addEventListener: no args, impossible"
      handle _ _ (x:_) = cb x
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
  moduleMiso <- jsg "miso"
  moduleMiso # "eventJSON" $ [x,y]
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
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "diff" $ [a,b,c]
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
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "delegate" $ [mountPoint,events,cb,d]
-----------------------------------------------------------------------------
undelegate :: JSVal -> JSVal -> Bool -> Function -> JSM ()
undelegate mountPoint events debug callback = do
  d <- toJSVal debug
  cb <- toJSVal callback
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "undelegate" $ [mountPoint,events,cb,d]
-----------------------------------------------------------------------------
-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
--
-- <https://en.wikipedia.org/wiki/Hydration_(web_development)>
--
hydrate :: Bool -> JSVal -> JSVal -> JSM ()
hydrate logLevel mountPoint vtree = void $ do
  ll <- toJSVal logLevel
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "hydrate" $ [ll, mountPoint, vtree]
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
-- | Sets the body with data-component-id
setBodyComponent :: MisoString -> JSM ()
setBodyComponent name = do
  component <- toJSVal name
  node <- jsg "document" ! "body"
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "setComponent" $ [node, component]
-----------------------------------------------------------------------------
-- | Sets @data-component-id@ on the node given by second argument to a value given by the first argument
setComponent :: MisoString -> JSVal -> JSM ()
setComponent name node = do
  component <- toJSVal name
  moduleMiso <- jsg "miso"
  void $ moduleMiso # "setComponent" $ [node, component]
-----------------------------------------------------------------------------
-- | Appends a 'style_' element containing CSS to 'head_'
--
-- > addStyle "body { background-color: green; }"
--
-- > <head><style>body { background-color: green; }</style></head>
--
addStyle :: MisoString -> JSM ()
addStyle css = do
  style <- jsg "document" # "createElement" $ ["style"]
  (style <# "innerHTML") css
  void $ jsg "document" ! "head" # "appendChild" $ [style]
-----------------------------------------------------------------------------
-- | Appends a StyleSheet 'link_' element to 'head_'
-- The 'link_' tag will contain a URL to a CSS file.
--
-- > addStyleSheet "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css"
--
-- > <head><link href="https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css" ref="stylesheet"></head>
--
addStyleSheet :: MisoString -> JSM ()
addStyleSheet url = do
  link <- jsg "document" # "createElement" $ ["link"]
  _ <- link # "setAttribute" $ ["rel","stylesheet"]
  _ <- link # "setAttribute" $ ["href", fromMisoString url]
  void $ jsg "document" ! "head" # "appendChild" $ [link]
-----------------------------------------------------------------------------
-- | Retrieve JSON via Fetch API
--
-- Basic GET of JSON using Fetch API, will be expanded upon.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API>
--
fetchJSON
  :: FromJSON action
  => MisoString
  -- ^ url
  -> MisoString
  -- ^ method
  -> Maybe MisoString
  -- ^ body
  -> [(MisoString,MisoString)]
  -- ^ headers
  -> (action -> JSM ())
  -- ^ successful callback
  -> (MisoString -> JSM ())
  -- ^ errorful callback
  -> JSM ()
fetchJSON url method maybeBody headers successful errorful = do
  successful_ <- toJSVal =<< do
    asyncCallback1 $ \jval ->
      fromJSON <$> fromJSValUnchecked jval >>= \case
        Error string ->
          error ("fetchJSON: " <> string <> ": decode failure")
        Success result -> do
          successful result
  errorful_ <- toJSVal =<< do
    asyncCallback1 $ \jval ->
       errorful =<< fromJSValUnchecked jval
  moduleMiso <- jsg "miso"
  url_ <- toJSVal url
  method_ <- toJSVal method
  body_ <- toJSVal maybeBody
  let jsonHeaders =
        [(ms "Content-Type", ms "application/json") | isJust maybeBody]
        <>
        [(ms "Accept", ms "application/json")]
  Object headers_ <- do
    o <- create
    forM_ (headers <> jsonHeaders) $ \(k,v) -> do
      set k v o
    pure o
  void $ moduleMiso # "fetchJSON" $ [url_, method_, body_, headers_, successful_, errorful_]
-----------------------------------------------------------------------------
-- | shouldSync
--
-- Used to set whether or not the current VNode should enter the 'syncChildren'
-- function during diffing. The criteria for entrance is that all children
-- have a populated 'key' node. We can determine this property more efficiently
-- at tree construction time rather than dynamic detection during diffing.
--
shouldSync :: JSVal -> JSM Bool
shouldSync vnode = do
  moduleMiso <- jsg "miso"
  fromJSValUnchecked =<< do
    moduleMiso # "shouldSync" $ [vnode]
-----------------------------------------------------------------------------
newtype Image = Image JSVal
  deriving (ToJSVal)
-----------------------------------------------------------------------------
-- | Smart constructor for building a 'Image' w/ 'src' attribute.
newImage :: MisoString -> JSM Image
newImage url = do
  img <- new (jsg "Image") ([] :: [MisoString])
  img <# "src" $ url
  pure (Image img)
-----------------------------------------------------------------------------
newtype Audio = Audio JSVal
  deriving (ToJSVal)
-----------------------------------------------------------------------------
newAudio :: MisoString -> JSM Audio
newAudio url = do
  a <- new (jsg "Audio") ([] :: [MisoString])
  o <- makeObject a
  set (ms "src") url o
  pure (Audio a)
-----------------------a------------------------------------------------------
play :: Audio -> JSM ()
play (Audio a) = void $ a # "play" $ ()
-----------------------------------------------------------------------------
volume :: Audio -> Double -> JSM ()
volume (Audio a) = a <# "volume"
-----------------------------------------------------------------------------
pause :: Audio -> JSM Bool
pause (Audio a) = do
  value <- a ! "paused"
  fromMaybe False <$> fromJSVal value
-----------------------------------------------------------------------------
