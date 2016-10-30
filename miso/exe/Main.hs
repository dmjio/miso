{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Miso
import GHCJS.Marshal
import Data.String
import Data.Proxy
import GHC.Generics

data Action = Add | Sub
  deriving (Generic, Show)

instance ToJSVal Action
instance FromJSVal Action

main :: IO ()
main = startApp emptyModel view update defaultSettings { events }
  where
    emptyModel :: Int
    emptyModel = 0

    update Add m = noEff ( m + 1 )
    update Sub m = noEff ( m - 1 )

    events :: Proxy '[ '( "click", 'False ) ]
    events = Proxy

view :: Int -> View Action
view x = div_ [] [
   button_ [ onClick Add ] [ text "+" ]
 , text $ fromString (show x)
 , button_ [ onClick Sub ] [ text "-" ]
 ]
