{-# LANGUAGE DataKinds #-}
module Miso.Settings
  ( defaultSettings
  , emptySettings
  , debugSettings
  ) where

import Miso.Types
import Data.Proxy
import Miso.Event

-- | Default settings for running a miso application
defaultSettings :: Settings DefaultStepConfig action
defaultSettings = Settings defaultEvents (Proxy :: Proxy '[]) [] False

emptySettings :: Settings DefaultStepConfig action
emptySettings = Settings mempty (Proxy :: Proxy '[]) [] False

debugSettings :: Settings '[DebugActions] action
debugSettings = Settings defaultEvents (Proxy :: Proxy '[DebugActions]) [] False
