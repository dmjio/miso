{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Miso

default (MisoString)

data Action = Add | Sub

main :: IO ()
main = startApp 0 view update defaultSettings
  where
    update Add m = noEff ( m + 1 )
    update Sub m = noEff ( m - 1 )

view :: Int -> View Action
view x = div_ [] [
   button_ [ onClick Add ] [ text "+" ]
 , text $ show x
 , button_ [ onClick Sub ] [ text "-" ]
 ]
