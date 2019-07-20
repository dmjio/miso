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
import           Control.Monad.IO.Class
import           Data.Aeson hiding (Object)
import           Data.JSString
import           GHCJS.Marshal
import           GHCJS.Types
import qualified JavaScript.Object.Internal as OI
import           Language.Javascript.JSaddle hiding (Success, obj, val)

forkJSM :: JSM () -> JSM ()
forkJSM a = do
  ctx <- askJSM
  _ <- liftIO (forkIO (runJSM a ctx))
  pure ()

asyncCallback :: JSM () -> JSM Function
asyncCallback a = asyncFunction (\_ _ _ -> a)

asyncCallback1 :: (JSVal -> JSM ()) -> JSM Function
asyncCallback1 f = asyncFunction (\_ _ [x] -> f x)

callbackToJSVal :: Function -> JSM JSVal
callbackToJSVal = toJSVal

objectToJSVal :: Object -> JSM JSVal
objectToJSVal = toJSVal

-- | Set property on object
set :: ToJSVal v => JSString -> v -> OI.Object -> JSM ()
set k v obj = do
  v' <- toJSVal v
  setProp k v' obj

addEventListener :: JSVal -> JSString -> (JSVal -> JSM ()) -> JSM ()
addEventListener self name cb = do
  _ <- self # "addEventListener" $ (name, asyncFunction (\_ _ [a] -> cb a))
  pure ()

-- | Adds event listener to window
windowAddEventListener :: JSString -> (JSVal -> JSM ()) -> JSM ()
windowAddEventListener name cb = do
  win <- jsg "window"
  addEventListener win name cb

eventStopPropagation :: JSVal -> JSM ()
eventStopPropagation e = do
  _ <- e # "stopPropagation" $ ()
  pure ()

eventPreventDefault :: JSVal -> JSM ()
eventPreventDefault e = do
  _ <- e # "preventDefault" $ ()
  pure ()

-- | Retrieves inner height
windowInnerHeight :: JSM Int
windowInnerHeight =
  fromJSValUnchecked =<< jsg "window" ! "innerHeight"

-- | Retrieves outer height
windowInnerWidth :: JSM Int
windowInnerWidth =
  fromJSValUnchecked =<< jsg "window" ! "innerWidth"

-- | Retrieve high performance time stamp
now :: JSM Double
now = fromJSValUnchecked =<< (jsg "performance" # "now" $ ())

-- | Console-logging
consoleLog :: JSVal -> JSM ()
consoleLog v = do
  _ <- jsg "console" # "log" $ [v]
  pure ()

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> JSM JSString
{-# INLINE stringify #-}
stringify j = do
  v <- toJSVal (toJSON j)
  fromJSValUnchecked =<< (jsg "JSON" # "stringify" $ [v])

-- | Parses a JSString
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

objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> JSM JSVal
objectToJSON = jsg2 "objectToJSON"

getBody :: JSM JSVal
getBody = jsg "document" ! "body"

getDoc :: JSM JSVal
getDoc = jsg "document"

getElementById :: JSString -> JSM JSVal
getElementById e = getDoc # "getElementById" $ [e]

diff'
    :: OI.Object -- ^ current object
    -> OI.Object -- ^ new object
    -> JSVal  -- ^ parent node
    -> JSVal -- ^ document
    -> JSM ()
diff' a b c d = () <$ jsg4 "diff" a b c d

integralToJSString :: Integral a => a -> JSString
integralToJSString = pack . show . toInteger

realFloatToJSString :: RealFloat a => a -> JSString
realFloatToJSString x = (pack . show) (realToFrac x :: Double)

jsStringToDouble :: JSString -> Double
jsStringToDouble = read . unpack

delegateEvent :: JSVal -> JSVal -> JSM JSVal -> JSM ()
delegateEvent mountPoint events getVTree = do
  cb' <- function $ \_ _ [continuation] -> do
    res <- getVTree
    _ <- call continuation global res
    pure ()
  delegateEvent' mountPoint events cb'

delegateEvent' :: JSVal -> JSVal -> Function -> JSM ()
delegateEvent' mountPoint events cb = () <$ jsg3 "delegate" mountPoint events cb

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
copyDOMIntoVTree :: JSVal -> JSVal -> JSM ()
copyDOMIntoVTree mountPoint a = () <$ jsg2 "copyDOMIntoVTree" mountPoint a

-- TODO For now, we do not free callbacks when compiling with JSaddle

-- | Pins down the current callbacks for clearing later
swapCallbacks :: JSM ()
swapCallbacks = pure ()

-- | Releases callbacks registered by the virtual DOM.
releaseCallbacks :: JSM ()
releaseCallbacks = pure ()

registerCallback :: JSVal -> JSM ()
registerCallback _ = pure ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
focus :: JSString -> JSM ()
focus a = () <$ jsg1 "callFocus" a

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
blur :: JSString -> JSM ()
blur a = () <$ jsg1 "callBlur" a

-- | Calls @document.getElementById(id).scrollIntoView()@
scrollIntoView :: JSString -> JSM ()
scrollIntoView elId = do
  el <- jsg "document" # "getElementById" $ [elId]
  _ <- el # "scollIntoView" $ ()
  pure ()

-- | Calls the @alert()@ function.
alert :: JSString -> JSM ()
alert a = () <$ jsg1 "alert" a
