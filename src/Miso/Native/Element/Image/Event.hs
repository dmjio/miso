-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Image.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Image.Event
  ( -- *** Events
    onLoad
  , onLoadWith
  , onError
  , onErrorWith
  , onStartPlay
  , onStartPlayWith
  , onCurrentLoopComplete
  , onCurrentLoopCompleteWith
  , onFinalLoopComplete
  , onFinalLoopCompleteWith
  -- *** Decoder
  , imageLoadDecoder
  , imageErrorDecoder
  -- *** Types
  , ImageErrorEvent (..)
  , ImageLoadEvent (..)
  -- *** Event Map
  , imageEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (EventHandler, DOMRef)
-----------------------------------------------------------------------------
imageEvents :: Events
imageEvents
  = backgroundEvents
  [ ("load", BUBBLE)
  , ("error", BUBBLE)
  , ("startplay", BUBBLE)
  , ("currentloopcomplete", BUBBLE)
  , ("finalloopcomplete", BUBBLE)
  ]
----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#bindload
--
-- Triggered when the image request succeeds, outputting the image's width and height.
--
-- @
--
-- data Action = HandleImageLoad ImageLoadEvent
--
-- view :: context -> props -> Model -> View context Action
-- view model = image_ "url" [ onLoad HandleImageLoad ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleImageLoad ImageLoadEvent {..}) = do
--   io_ (consoleLog "image load event received")
--
-- @
--
onLoad :: (ImageLoadEvent -> action) -> EventHandler action
onLoad action = on "load" imageLoadDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#binderror
--
-- Triggered when the image request fails, outputting the error message and code.
--
-- @
--
-- data Action = HandleImageError ImageErrorEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = image_ "url" [ onError HandleImageError ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleImageError ImageErrorEvent {..}) = do
--   io_ (consoleLog "image error event received")
--
-- @
--
onError :: (ImageErrorEvent -> action) -> EventHandler action
onError action = on "error" imageErrorDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#bindstartplay
--
-- Triggered when the animated image starts playing.
--
-- @
--
-- data Action = HandleStartPlay
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = image_ "url" [ onStartPlay HandleStartPlay ]
--
-- @
--
onStartPlay :: action -> EventHandler action
onStartPlay action = on "startplay" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#bindcurrentloopcomplete
--
-- Triggered when one loop of the animated image finishes playing.
--
-- @
--
-- data Action = HandleCurrentLoopComplete
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = image_ "url" [ onCurrentLoopComplete HandleCurrentLoopComplete ]
--
-- @
--
onCurrentLoopComplete :: action -> EventHandler action
onCurrentLoopComplete action = on "currentloopcomplete" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#bindfinalloopcomplete
--
-- Triggered when the animated image finishes playing all 'loopCount_' loops.
--
-- @
--
-- data Action = HandleFinalLoopComplete
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = image_ "url" [ onFinalLoopComplete HandleFinalLoopComplete ]
--
-- @
--
onFinalLoopComplete :: action -> EventHandler action
onFinalLoopComplete action = on "finalloopcomplete" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | Callback when an 'image_' fails to load
data ImageErrorEvent
  = ImageErrorEvent
  { errorMessage :: MisoString
    -- ^ error message
  , errorCode :: Int
    -- ^ error code
  , lynxCategorizedCode :: Int
    -- ^ lynx specific error code
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Callback when an 'image_' succeeds in loading
data ImageLoadEvent
  = ImageLoadEvent
  { imageWidth :: Int
    -- ^ 'image_' width
  , imageHeight :: Int
    -- ^ 'image_' height
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
imageLoadDecoder :: Decoder ImageLoadEvent
imageLoadDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      ImageLoadEvent
        <$> o .: "width"
        <*> o .: "height"
-----------------------------------------------------------------------------
imageErrorDecoder :: Decoder ImageErrorEvent
imageErrorDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      ImageErrorEvent
        <$> o .: "errMsg"
        <*> o .: "error_code"
        <*> o .: "lynx_categorized_code"
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onLoad', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLoadWith :: (ImageLoadEvent -> DOMRef -> action) -> EventHandler action
onLoadWith action = on "load" imageLoadDecoder action
-----------------------------------------------------------------------------
-- | Like 'onError', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onErrorWith :: (ImageErrorEvent -> DOMRef -> action) -> EventHandler action
onErrorWith action = on "error" imageErrorDecoder action
-----------------------------------------------------------------------------
-- | Like 'onStartPlay', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onStartPlayWith :: (DOMRef -> action) -> EventHandler action
onStartPlayWith action = on "startplay" emptyDecoder (\() ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onCurrentLoopComplete', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onCurrentLoopCompleteWith :: (DOMRef -> action) -> EventHandler action
onCurrentLoopCompleteWith action = on "currentloopcomplete" emptyDecoder (\() ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onFinalLoopComplete', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onFinalLoopCompleteWith :: (DOMRef -> action) -> EventHandler action
onFinalLoopCompleteWith action = on "finalloopcomplete" emptyDecoder (\() ref -> action ref)
-----------------------------------------------------------------------------
