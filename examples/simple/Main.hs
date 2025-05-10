{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell module declaration
module Main where

-- Miso framework import
import           Prelude hiding (unlines)

import           Miso
import           Miso.Lens
import           Miso.String

-- | Type synonym for an application model
newtype Model = Model { _value :: Int }
  deriving (Show, Eq)

instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v

value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }

-- | Sum type for application events
data Action
  = AddOne PointerEvent
  | SubtractOne PointerEvent
  | SayHelloWorld
  deriving (Show, Eq)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = run $ startComponent app
  { events = pointerEvents
  , styles = [ Style css ]
  }

-- | Component definition (uses 'defaultComponent' smart constructor)
app :: Component name Model Action
app = defaultComponent (Model 0) updateModel viewModel

-- | UpdateModels model, optionally introduces side effects
updateModel :: Action -> Effect Model Action
updateModel (AddOne event) = do
  value += 1
  io_ $ consoleLog (ms (show event))
updateModel (SubtractOne event) = do
  value -= 1
  io_ $ consoleLog (ms (show event))
updateModel SayHelloWorld =
  io_ (consoleLog "Hello World!")

-- | Constructs a virtual DOM from a model
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

css :: MisoString
css = unlines
  [ ":root {"
  , "--primary-color: #4a6bff;"
  , "--primary-hover: #3451d1;"
  , "--secondary-color: #ff4a6b;"
  , "--secondary-hover: #d13451;"
  , "--background: #f7f9fc;"
  , "--text-color: #333;"
  , "--shadow: 0 4px 10px rgba(0, 0, 0, 0.1);"
  , "--transition: all 0.3s ease;"
  , "}"
  , "body {"
  , "  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;"
  , "  display: flex;"
  , "  justify-content: center;"
  , "  align-items: center;"
  , "  height: 100vh;"
  , "  margin: 0;"
  , "  background-color: var(--background);"
  , "  color: var(--text-color);"
  , "}"
  , ".counter-container {"
  , "  background-color: white;"
  , "  padding: 2rem;"
  , "  border-radius: 12px;"
  , "  box-shadow: var(--shadow);"
  , "  text-align: center;"
  , "}"
  , ".counter-display {"
  , "  font-size: 5rem;"
  , "  font-weight: bold;"
  , "  margin: 1rem 0;"
  , "  transition: var(--transition);"
  , "}"
  , ".buttons-container {"
  , "  display: flex;"
  , "  gap: 1rem;"
  , "  justify-content: center;"
  , "  margin-top: 1.5rem;"
  , "}"
  , "button {"
  , "  font-size: 1.5rem;"
  , "  width: 3rem;"
  , "  height: 3rem;"
  , "  border: none;"
  , "  border-radius: 50%;"
  , "  cursor: pointer;"
  , "  transition: var(--transition);"
  , "  color: white;"
  , "  display: flex;"
  , "  align-items: center;"
  , "  justify-content: center;"
  , "}"
  , ".increment-btn {"
  , "  background-color: var(--primary-color);"
  , "}"
  , ".increment-btn:hover {"
  , "  background-color: var(--primary-hover);"
  , "  transform: translateY(-2px);"
  , "}"
  , ".decrement-btn {"
  , "  background-color: var(--secondary-color);"
  , "}"
  , ".decrement-btn:hover {"
  , "  background-color: var(--secondary-hover);"
  , "  transform: translateY(-2px);"
  , "}"
  , "@keyframes pulse {"
  , "  0% { transform: scale(1); }"
  , "  50% { transform: scale(1.1); }"
  , "  100% { transform: scale(1); }"
  , "}"
  , ".counter-display.animate {"
  , "  animation: pulse 0.3s ease;"
  , "}"
  , "@media (max-width: 480px) {"
  , "  .counter-container {"
  , "    padding: 1.5rem;"
  , "  }"
  , "  .counter-display {"
  , "    font-size: 3rem;"
  , "  }"
  , "  button {"
  , "    font-size: 1.2rem;"
  , "    width: 2.5rem;"
  , "    height: 2.5rem;"
  , "  }"
  , "}"
  ]

