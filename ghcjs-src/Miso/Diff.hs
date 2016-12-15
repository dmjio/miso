module Miso.Diff ( diff ) where

import GHCJS.Foreign.Internal     hiding (Object)
import GHCJS.Types
import JavaScript.Object
import JavaScript.Object.Internal
import Miso.Html.Internal

diff :: Maybe (VTree a) -> Maybe (VTree a) -> IO ()
diff current new = do
  body <- getBody
  case (current, new) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) -> do
      diff' current' new' body
    (Nothing, Just (VTree new')) -> do
      diff' (Object jsNull) new' body
    (Just (VTree current'), Nothing) -> do
      diff' current' (Object jsNull) body

foreign import javascript unsafe "$r = document.body"
  getBody :: IO JSVal

foreign import javascript unsafe "diff($1, $2, $3);"
  diff'
    :: Object -- ^ current object
    -> Object -- ^ new object
    -> JSVal  -- ^ parent node
    -> IO ()
