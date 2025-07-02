-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Prelude hiding (unlines, rem)
-----------------------------------------------------------------------------
import           Miso hiding (media_)
import           Miso.Lens
import           Miso.String
import           Miso.Style hiding (ms)
-----------------------------------------------------------------------------
newtype Model = Model { _value :: Int }
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v
-----------------------------------------------------------------------------
value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = AddOne PointerEvent
  | SubtractOne PointerEvent
  | SayHelloWorld
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startComponent app
-----------------------------------------------------------------------------
app :: Component Model Action
app = (component (Model 0) updateModel viewModel)
  { events = pointerEvents
  , styles = [ Sheet sheet ]
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect Model Action
updateModel (AddOne event) = do
  value += 1
  io_ $ consoleLog (ms (show event))
updateModel (SubtractOne event) = do
  value -= 1
  io_ $ consoleLog (ms (show event))
updateModel SayHelloWorld =
  io_ (consoleLog "Hello World!")
-----------------------------------------------------------------------------
viewModel :: Model -> View Action
viewModel x = div_
  [ class_ "counter-container" ]
  [ h1_
    [ class_ "counter-title"
    ]
    [ "🍜 Miso counter"
    ]
  , div_
    [ class_ "counter-display"
    ]
    [ text (ms x)
    ]
  , div_
    [ class_ "buttons-container"
    ]
    [ button_
      [ onPointerDown AddOne
      , class_ "decrement-btn"
      ] [text "+"]
    , button_
      [ onPointerDown SubtractOne
      , class_ "increment-btn"
      ] [text "-"]
    ]
  ]
-----------------------------------------------------------------------------
sheet :: StyleSheet
sheet =
  sheet_
  [ selector_ ":root"
    [ "--primary-color" =: "#4a6bff"
    , "--primary-hover" =: "#3451d1"
    , "--secondary-color" =: "#ff4a6b"
    , "--secondary-hover" =: "#d13451"
    , "--background" =: "#f7f9fc"
    , "--text-color" =: "#333"
    , "--shadow" =: "0 4px 10px rgba(0, 0, 0, 0.1);"
    , "--transition" =: "all 0.3s ease;"
    ]
  , selector_ "body"
    [ fontFamily "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"
    , display "flex"
    , justifyContent "center"
    , alignItems "center"
    , height "100vh"
    , margin "0"
    , backgroundColor (var "background")
    , color (var "text-color")
    ]
  , selector_ ".counter-container"
    [ backgroundColor white
    , padding (rem 2)
    , borderRadius (px 12)
    , boxShadow "shadow"
    , textAlign "center"
    ]
  , selector_ ".counter-display"
    [ fontSize "5rem"
    , fontWeight "bold"
    , margin "1rem 0"
    , transition "var(--transition)"
    ]
  , selector_ ".buttons-container"
    [ display "flex"
    , gap "1rem"
    , justifyContent "center"
    , marginTop "1.5rem"
    ]
  , selector_ "button"
    [ fontSize "1.5rem"
    , width "3rem"
    , height "3rem"
    , border "none"
    , borderRadius "50%"
    , cursor "pointer"
    , transition "var(--transition)"
    , color white
    , display "flex"
    , alignItems "center"
    , justifyContent "center"
    ]
  , selector_ ".increment-btn"
    [ backgroundColor (var "primary-color")
    ]
  , selector_ ".increment-btn:hover"
    [ backgroundColor (var "primary-hover")
    , transform "translateY(-2px)"
    ]
  , selector_ ".decrement-btn"
    [ backgroundColor (var "secondary-color")
    ]
  , selector_ ".decrement-btn:hover"
    [ backgroundColor (var "secondary-hover")
    , transform "translateY(-2px)"
    ]
  , keyframes_ "pulse"
    [ pct 0 =:
      [ transform "scale(1)"
      ]
    , pct 50 =:
      [ transform "scale(1.1)"
      ]
    , pct 100 =:
      [ transform "scale(1)"
      ]
    ]
  , selector_ ".counter-display.animate"
    [ animation "pulse 0.3s ease"
    ]
  , media_ "(max-width: 480px)"
    [ ".counter-container" =:
      [ padding (rem 1.5)
      ]
    , ".counter-display" =:
      [ fontSize (rem 3)
      ]
    , "button" =:
      [ fontSize (rem 1.2)
      , width (rem 2.5)
      , width (rem 2.5)
      ]
    ]
  ]
-----------------------------------------------------------------------------
