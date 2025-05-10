{-# LANGUAGE OverloadedStrings #-}
module DevelMain (update) where

import Common (haskellMisoComponent, uriHome)
import Miso (startComponent, run)
import Miso.String (MisoString)

import Rapid

update :: IO ()
update =
  rapid 0 $ \r ->
    restart r ("miso-client" :: MisoString) $
      run (startComponent (haskellMisoComponent uriHome))
