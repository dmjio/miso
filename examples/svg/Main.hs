{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad.State
import qualified Data.Map as M

import           Miso hiding (update)
import           Miso.String (ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
import qualified Miso.Style as CSS
import           Miso.Style ((=:))

#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ startComponent app
  { events = M.insert "pointermove" False pointerEvents
  , subs = [ mouseSub HandlePointer ]
  }

-- | Component definition (uses 'defaultComponent' smart constructor)
app :: Component name Model Action
app = defaultComponent emptyModel updateModel viewModel

emptyModel :: Model
emptyModel = Model (0, 0)

updateModel :: Action -> Effect Model Action
updateModel (HandlePointer pointer) = modify update
  where
    update m = m { mouseCoords = client pointer }

data Action
  = HandlePointer PointerEvent

newtype Model
  = Model
  { mouseCoords :: (Double, Double)
  } deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel (Model (x, y)) =
    div_
        []
        [ svg_
            [ CSS.style_
              [ CSS.borderStyle "solid"
              , CSS.height "700px"
              , CSS.width "100%"
              ]
            , onPointerMove HandlePointer
            ]
            [ g_
                []
                [ ellipse_
                    [ cx_ $ ms x
                    , cy_ $ ms y
                    , CSS.style_
                        [ "fill" =: "yellow"
                        , "stroke" =: "purple"
                        , "stroke-width" =: "2"
                        ]
                    , rx_ "100"
                    , ry_ "100"
                    ]
                    []
                ]
            , text_
                [ x_ $ ms x
                , y_ $ ms y
                ]
                [text $ ms $ show (x, y)]
            ]
        ]
