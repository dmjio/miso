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
  , onError
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
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
imageEvents :: Events
imageEvents
  = M.fromList
  [ ("load", BUBBLE)
  , ("error", BUBBLE)
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
-- view :: Model -> View Model Action
-- view model = image_ "url" [ onLoad HandleImageLoad ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleImageLoad ImageLoadEvent {..}) = do
--   io_ (consoleLog "image load event received")
--
-- @
--
onLoad :: (ImageLoadEvent -> action) -> Attribute action
onLoad action = on "bindload" imageLoadDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/image.html#binderror
--
-- Triggered when the image request fails, outputting the error message and code.
--
-- @
--
-- data Action = HandleImageError ImageErrorEvent
--
-- view :: Model -> View Model Action
-- view model = image_ "url" [ onError HandleImageError ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleImageError ImageErrorEvent {..}) = do
--   io_ (consoleLog "image error event received")
--
-- @
--
onError :: (ImageErrorEvent -> action) -> Attribute action
onError action = on "binderror" imageErrorDecoder (\e _ -> action e)
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
