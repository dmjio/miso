-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.View.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.View.Method
  ( -- *** Methods
    boundingClientRect
  , takeScreenshot
  , requestAccessibilityFocus
  -- *** Types
  , Rect (..)
  , BoundingClientRect (..)
  , TakeScreenshot (..)
  -- *** Smart constructors
  , defaultBoundingClientRect
  , defaultTakeScreenshot
  ) where
-----------------------------------------------------------------------------
import Miso
import Miso.Native.FFI
-----------------------------------------------------------------------------
-- | Result of calling `getClientBoundingRect`
data Rect
  = Rect
  { x,y :: Double
  , width, height :: Double
  , top, bottom :: Double
  , right, left :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal Rect where
  fromJSVal = \rect -> do
    let readProp = \name ->
          fromJSValUnchecked =<<
            rect ! (name :: MisoString)
    x      <- readProp "x"
    y      <- readProp "y"
    height <- readProp "height"
    width  <- readProp "width"
    top    <- readProp "top"
    right  <- readProp "right"
    left   <- readProp "left"
    bottom <- readProp "bottom"
    pure $ Just Rect {..}
-----------------------------------------------------------------------------
data BoundingClientRect
  = BoundingClientRect
  { androidEnableTransformProps :: Bool
  -- ^ Specifies whether to consider the transform attribute
  -- when calculating the position on Android. The default value is 'False'
  , relativeTo :: Maybe JSVal
  -- ^ Specify the reference node, relative to LynxView by default.
  }
-----------------------------------------------------------------------------
instance ToJSVal BoundingClientRect where
  toJSVal BoundingClientRect {..} = do
    o <- create
    set "androidEnableTransformProps" androidEnableTransformProps o
    set "relativeTo" relativeTo o
    toJSVal o
-----------------------------------------------------------------------------
-- | Smart constructor for constructing 'boundingClientRect'
defaultBoundingClientRect :: BoundingClientRect
defaultBoundingClientRect
  = BoundingClientRect
  { androidEnableTransformProps = True
  , relativeTo = Nothing
  }
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#boundingclientrect
--
-- The front end can execute 'boundingClientRect' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success Rect
--   | Failure MisoString
--   | GetRect
--
-- update :: Action -> Effect props model Action
-- update GetRect =
--   boundingClientRect defaultBoundingClientRect "#box" Success Failure
-- update (Succes Rect {..}) =
--   consoleLog "Successfuly got Rect"
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call getClientBoundingRect: " <> errorMsg)
--
-- @
--
boundingClientRect
  :: MisoString
  -> BoundingClientRect
  -> (Rect -> action)
  -> (MisoString -> action)
  -> Effect context props model action
boundingClientRect = invokeExec "boundingClientRect"
-----------------------------------------------------------------------------
data TakeScreenshot
  = TakeScreenshot
  { format :: MisoString
  -- ^ e.g. Specify the image format, supports jpeg and png, the default is jpeg
  , scale :: Double
  -- ^ e.g. Specify the image quality, 0 < scale <= 1, the default is 1,
  -- the smaller the value, the blurrier and smaller the size.
  }
-----------------------------------------------------------------------------
instance ToJSVal TakeScreenshot where
  toJSVal TakeScreenshot {..} = do
    o <- create
    set "format" format o
    set "scale" scale o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#takescreenshot
--
-- The front end can execute 'takeScreenshot' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success Image
--   | Failure MisoString
--   | GetScreenshot
--
-- update :: Action -> Effect props model Action
-- update GetScreenshot = takeScreenshot
--   defaultTakeScreenshot "#my-view" Success Failure
-- update (Succes image) =
--   consoleLog "Successfuly got image"
--   consoleLog' image
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call takeScreenshot: " <> errorMsg)
--
-- @
--
takeScreenshot
  :: MisoString
  -> TakeScreenshot
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
takeScreenshot = invokeExec "takeScreenshot"
-----------------------------------------------------------------------------
-- | Smart constructor for calling 'TakeScreenshot'
defaultTakeScreenshot :: TakeScreenshot
defaultTakeScreenshot
  = TakeScreenshot
  { scale = 0.5
  , format = ".png"
  }
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#requestaccessibilityfocus
--
-- The front end can execute 'requestAccessiblityFocus' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success
--   | Failure MisoString
--   | GetFocus
--
-- update :: Action -> Effect props model Action
-- update GetFocus = requestAccessibilityFocus "#my-view" Success Failure
-- update Success = consoleLog "Successfuly got focus"
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call requestAccessibilityFocus: " <> errorMsg)
--
-- @
--
requestAccessibilityFocus
  :: MisoString
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
requestAccessibilityFocus selector =
  invokeExec "requestAccessibilityFocus" selector ()
-----------------------------------------------------------------------------
