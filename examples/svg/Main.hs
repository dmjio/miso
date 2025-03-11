{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Arrow
import           Data.Aeson.Types
import qualified Data.Map as M

import           Miso
import           Miso.String (MisoString, pack, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ startApp App {..}
  where
    initialAction = Id
    model         = emptyModel
    update        = updateModel
    view          = viewModel
    events        = M.insert (pack "mousemove") False $
                    M.insert (pack "touchstart") False $
                    M.insert (pack "touchmove") False defaultEvents
    subs          = [ mouseSub HandleMouse ]
    logLevel      = Off
    mountPoint    = Nothing

trunc :: (Double, Double) -> (Int, Int)
trunc = truncate *** truncate

emptyModel :: Model
emptyModel = Model (0,0)

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleTouch (TouchEvent touch)) model =
  model <# do
    return $ HandleMouse $ trunc . page $ touch
updateModel (HandleMouse newCoords) model =
  noEff model { mouseCoords = newCoords }
updateModel Id model = noEff model

data Action
  = HandleMouse (Int, Int)
  | HandleTouch TouchEvent
  | Id

newtype Model
  = Model
  { mouseCoords  :: (Int, Int)
  } deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel (Model (x,y)) =
  div_ [ ] [
    svg_ [ style_ $ M.fromList [ ("border-style", "solid")
                               , ("height", "700px")
                               , ("width", "100%")
                               ]
         , onTouchMove HandleTouch
       ] [
     g_ [] [
     ellipse_ [ cx_ $ ms x
              , cy_ $ ms y
              , style_ svgStyle
              , rx_ "100"
              , ry_ "100"
              ] [ ]
     ]
     , text_ [ x_ $ ms x
             , y_ $ ms y
             ] [ text $ ms $ show (x,y) ]
   ]
 ]

svgStyle :: M.Map MisoString MisoString
svgStyle =
  M.fromList [
      ("fill", "yellow")
    , ("stroke", "purple")
    , ("stroke-width", "2")
    ]

data Touch = Touch
  { identifier :: Int
  , screen :: (Double, Double)
  , client :: (Double, Double)
  , page :: (Double, Double)
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
    ((x:_):_) <- parseJSON obj
    return $ TouchEvent x

touchDecoder :: Decoder TouchEvent
touchDecoder = Decoder {..}
  where
    decodeAt = DecodeTargets [["changedTouches"], ["targetTouches"], ["touches"]]
    decoder = parseJSON

onTouchMove :: (TouchEvent -> action) -> Attribute action
onTouchMove = on "touchmove" touchDecoder

onTouchStart :: (TouchEvent -> action) -> Attribute action
onTouchStart = on "touchstart" touchDecoder
