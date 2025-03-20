{-# LANGUAGE OverloadedStrings #-}
module DevelMain (update) where

import Common (haskellMisoComponent, uriHome)
import Miso (startApp, run)
import Miso.String (MisoString)

import Rapid

update :: IO ()
update =
  rapid 0 $ \r ->
    restart r ("miso-client" :: MisoString) $
      run (startApp (haskellMisoComponent uriHome))
