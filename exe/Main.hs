-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String hiding (splitAt, reverse, length)
import Data.List

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
data Model = Model { nums :: [Int], seed :: Int }
  deriving (Eq, Show)

-- | Sum type for application events
data Action
  = Before
  | BeforeLast
  | AfterFirst
  | Clear
  | After
  | Middle
  | NoOp
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Model [1..10] 11      -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Clear m = noEff m { nums = [] }
updateModel Before Model {..} = noEff $ Model (seed : nums) (seed + 1)
updateModel After Model  {..}  = noEff $ Model (nums ++ [seed]) (seed + 1)
updateModel Middle Model {..} = noEff $
  case splitAt (length nums `div` 2) nums of
    (ls,rs) ->
      Model (ls ++ [seed] ++ rs) (seed + 1)
updateModel BeforeLast Model {..} = noEff $
  case reverse nums of
    (z:zs) -> Model (reverse (z:seed:zs)) (seed + 1)
updateModel AfterFirst Model {..} = noEff $
  case nums of
    (x:xs) ->
      Model (x:seed:xs) (seed + 1)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] [
   button_ [ onClick Clear ] [ text "Clear" ]
 , button_ [ onClick Before ] [ text "Before" ]
 , button_ [ onClick After ] [ text "After" ]
 , button_ [ onClick BeforeLast ] [ text "Before Last" ]
 , button_ [ onClick AfterFirst ] [ text "After First" ]
 , button_ [ onClick Middle ] [ text "Middle" ]
 , text $ ms $ "Next seed: " <> show (seed m)
 , ul_ []
   [ liKeyed_ (toKey x) [] [ text $ ms (show x) ]
   | x <- nums m
   ]
 ]
