-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

import Control.Monad.IO.Class

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

-- | Type synonym for an application model
type Model = ()

-- | Sum type for application events
data Action
  = SetupTextArea
  | SubmitForm
  | NoOp
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = runApp $ miso $ \_ -> App {..}
  where
    initialAction = NoOp
    model  = ()
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing
    logLevel = Off

updateModel :: Action -> () -> Effect Action ()
updateModel SetupTextArea m = m <# do
  d <- jsg "document"
  de <- d ^. js1 @MisoString "getElementById" "thetextarea"
  addEventListener de "keypress" (handleKeyPress m)

handleKeyPress :: () -> JSVal -> JSM ()
handleKeyPress m e = do
  shiftKey <- e ^. js @_ @Bool "shiftKey"
  enterKey <- e ^. js @_ @Int "keyCode"
  when (not shiftKey && enterKey == 13) $ do
    jsf @MisoString "preventDefault" ()
    updateModel SubmitForm m

updateModel SubmitForm m = () <# do
  putStrLn "Submitted Form!"

updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
  textarea_
    [ onCreated SetupTextArea
    , id_ "thetextarea"
    ] []
  , button_
    [ onClick SubmitForm
    ]
    [ text "submit"
    ]
