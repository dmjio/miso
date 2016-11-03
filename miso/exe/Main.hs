{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String
import GHC.Generics
import GHCJS.Marshal

import Miso

data Action = Add | Sub
  deriving (Generic, Show)

instance ToJSVal Action
instance FromJSVal Action

main :: IO ()
main = startApp emptyModel view update defaultSettings
  where
    emptyModel :: Int
    emptyModel = 0

    update Add m = noEff ( m + 1 )
    update Sub m = noEff ( m - 1 )

view :: Int -> View Action
view x = div_ [] [
   button_ [ onClick Add ] [ text "+" ]
 , text $ fromString (show x)
 , button_ [ onClick Sub ] [ text "-" ]
 ]
