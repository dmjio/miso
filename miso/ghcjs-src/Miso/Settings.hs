{-# LANGUAGE DataKinds #-}
module Miso.Settings
  ( defaultSettings
  ) where

import Miso.Types
import Data.Proxy
import Miso.Event

-- | Default settings for running a miso application
defaultSettings :: Settings DefaultEvents DefaultStepConfig action
defaultSettings = Settings defaultEvents (Proxy :: Proxy '[]) [] False
