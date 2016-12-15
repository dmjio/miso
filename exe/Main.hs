{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Main where

import Miso
import Miso.Subscription.Mouse

main :: IO ()
main = startApp (Model (0,0)) view [ mouseSub toSub ] defaultEvents
  where
    toSub coords m = m { coords = coords }

data Model = Model {
      coords :: (Int, Int)
    } deriving Eq

view :: Model -> View Model
view Model{..} = div_ [] [
   text (show coords)
 ]

