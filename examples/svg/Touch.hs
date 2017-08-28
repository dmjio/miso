{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Touch where

import Control.Monad
import Data.Aeson.Types
import qualified Data.Vector as V
import Debug.Trace
import GHCJS.Marshal
import Miso

data Touch = Touch
  { identifier :: Int
  , screen :: (Int, Int)
  , client :: (Int, Int)
  , page :: (Int, Int)
  } deriving (Eq, Show)

instance FromJSON Touch where
  parseJSON =
    withObject "touch" $ \o -> do
      identifier <- o .: "identifier"
      screen <- (,) <$> o .: "screenX" <*> o .: "screenY"
      client <- (,) <$> o .: "clientX" <*> o .: "clientY"
      page <- (,) <$> o .: "pageX" <*> o .: "pageY"
      return Touch {..}

data TouchEvent =
  TouchEvent Touch
  deriving (Eq, Show)

instance FromJSON TouchEvent where
  parseJSON obj = do
    (x:_) <- parseJSON obj
    return $ TouchEvent x

touchDecoder :: Decoder TouchEvent
touchDecoder = Decoder {..}
  where
    decodeAt = ["changedTouches", "0"]
    decoder = parseJSON

onTouchMove :: (TouchEvent -> action) -> Attribute action
onTouchMove = on "touchmove" touchDecoder

onTouchStart = on "touchstart" touchDecoder
