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
  , onLoadMain
  , onLoadMainWith
  , onError
  , onErrorWith
  , onErrorMain
  , onErrorMainWith
  , onStartPlay
  , onStartPlayWith
  , onStartPlayMain
  , onStartPlayMainWith
  , onCurrentLoopComplete
  , onCurrentLoopCompleteWith
  , onCurrentLoopCompleteMain
  , onCurrentLoopCompleteMainWith
  , onFinalLoopComplete
  , onFinalLoopCompleteWith
  , onFinalLoopCompleteMain
  , onFinalLoopCompleteMainWith
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
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
imageEvents :: Events
imageEvents
  = M.fromList
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
onLoad :: (ImageLoadEvent -> action) -> Attribute model action
onLoad action = on "load" imageLoadDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLoad', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleImageLoad ImageLoadEvent
--
-- view_ [ event (static (onLoadMain HandleImageLoad)) ] [ "some view" ]
-- @
--
onLoadMain :: (ImageLoadEvent -> action) -> EventHandler model action
onLoadMain action = onMain "load" imageLoadDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLoadMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleImageLoad ImageLoadEvent Model DOMRef
--
-- view_ [ event (static (onLoadMainWith HandleImageLoad)) ] [ "some view" ]
-- @
--
onLoadMainWith :: (ImageLoadEvent -> model -> DOMRef -> action) -> EventHandler model action
onLoadMainWith action = onMain "load" imageLoadDecoder action
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
onError :: (ImageErrorEvent -> action) -> Attribute model action
onError action = on "error" imageErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onError', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleImageError ImageErrorEvent
--
-- view_ [ event (static (onErrorMain HandleImageError)) ] [ "some view" ]
-- @
--
onErrorMain :: (ImageErrorEvent -> action) -> EventHandler model action
onErrorMain action = onMain "error" imageErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onErrorMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleImageError ImageErrorEvent Model DOMRef
--
-- view_ [ event (static (onErrorMainWith HandleImageError)) ] [ "some view" ]
-- @
--
onErrorMainWith :: (ImageErrorEvent -> model -> DOMRef -> action) -> EventHandler model action
onErrorMainWith action = onMain "error" imageErrorDecoder action
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
onStartPlay :: action -> Attribute model action
onStartPlay action = on "startplay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onStartPlay', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleStartPlay
--
-- view_ [ event (static (onStartPlayMain HandleStartPlay)) ] [ "some view" ]
-- @
--
onStartPlayMain :: action -> EventHandler model action
onStartPlayMain action = onMain "startplay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onStartPlayMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleStartPlay Model DOMRef
--
-- view_ [ event (static (onStartPlayMainWith HandleStartPlay)) ] [ "some view" ]
-- @
--
onStartPlayMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onStartPlayMainWith action = onMain "startplay" emptyDecoder (\() m ref -> action m ref)
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
onCurrentLoopComplete :: action -> Attribute model action
onCurrentLoopComplete action = on "currentloopcomplete" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onCurrentLoopComplete', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleCurrentLoopComplete
--
-- view_ [ event (static (onCurrentLoopCompleteMain HandleCurrentLoopComplete)) ] [ "some view" ]
-- @
--
onCurrentLoopCompleteMain :: action -> EventHandler model action
onCurrentLoopCompleteMain action = onMain "currentloopcomplete" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onCurrentLoopCompleteMain', but the handler also receives read-only
-- access to the @model@ and the target element's 'DOMRef' (for imperative MTS
-- mutation).
--
-- @
-- data Action = HandleCurrentLoopComplete Model DOMRef
--
-- view_ [ event (static (onCurrentLoopCompleteMainWith HandleCurrentLoopComplete)) ] [ "some view" ]
-- @
--
onCurrentLoopCompleteMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onCurrentLoopCompleteMainWith action = onMain "currentloopcomplete" emptyDecoder (\() m ref -> action m ref)
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
onFinalLoopComplete :: action -> Attribute model action
onFinalLoopComplete action = on "finalloopcomplete" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onFinalLoopComplete', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleFinalLoopComplete
--
-- view_ [ event (static (onFinalLoopCompleteMain HandleFinalLoopComplete)) ] [ "some view" ]
-- @
--
onFinalLoopCompleteMain :: action -> EventHandler model action
onFinalLoopCompleteMain action = onMain "finalloopcomplete" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onFinalLoopCompleteMain', but the handler also receives read-only
-- access to the @model@ and the target element's 'DOMRef' (for imperative MTS
-- mutation).
--
-- @
-- data Action = HandleFinalLoopComplete Model DOMRef
--
-- view_ [ event (static (onFinalLoopCompleteMainWith HandleFinalLoopComplete)) ] [ "some view" ]
-- @
--
onFinalLoopCompleteMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onFinalLoopCompleteMainWith action = onMain "finalloopcomplete" emptyDecoder (\() m ref -> action m ref)
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
onLoadWith :: (ImageLoadEvent -> DOMRef -> action) -> Attribute model action
onLoadWith action = on "load" imageLoadDecoder $ \x _ domRef -> action x domRef
-----------------------------------------------------------------------------
-- | Like 'onError', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onErrorWith :: (ImageErrorEvent -> DOMRef -> action) -> Attribute model action
onErrorWith action = on "error" imageErrorDecoder $ \x _ domRef -> action x domRef
-----------------------------------------------------------------------------
-- | Like 'onStartPlay', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onStartPlayWith :: (DOMRef -> action) -> Attribute model action
onStartPlayWith action = on "startplay" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onCurrentLoopComplete', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onCurrentLoopCompleteWith :: (DOMRef -> action) -> Attribute model action
onCurrentLoopCompleteWith action = on "currentloopcomplete" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onFinalLoopComplete', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onFinalLoopCompleteWith :: (DOMRef -> action) -> Attribute model action
onFinalLoopCompleteWith action = on "finalloopcomplete" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
