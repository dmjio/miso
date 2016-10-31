module Miso.Event where

-- | Since the server contains no DOM we cannot raise events, so we mock it.
data EventHandler a = EventHandler
