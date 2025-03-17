{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map as M

import           Miso
import           Miso.String (MisoString, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ startApp app
  { events = M.insert "pointermove" False pointerEvents
  , subs = [ mouseSub HandlePointer ]
  }

-- | Application definition (uses 'defaultApp' smart constructor)
app :: App Model Action
app = defaultApp emptyModel updateModel viewModel Id

emptyModel :: Model
emptyModel = Model (0, 0)

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandlePointer pointer) model = noEff model { mouseCoords = coords pointer }
updateModel Id model = noEff model

data Action
  = HandlePointer PointerEvent
  | Id

newtype Model
  = Model
  { mouseCoords :: (Int, Int)
  } deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel (Model (x, y)) =
    div_
        []
        [ svg_
            [ style_ $
                M.fromList
                    [ ("border-style", "solid")
                    , ("height", "700px")
                    , ("width", "100%")
                    ]
            , onPointerMove HandlePointer
            ]
            [ g_
                []
                [ ellipse_
                    [ cx_ $ ms x
                    , cy_ $ ms y
                    , style_ svgStyle
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

svgStyle :: M.Map MisoString MisoString
svgStyle = M.fromList
  [ ("fill", "yellow")
  , ("stroke", "purple")
  , ("stroke-width", "2")
  ]
