module Miso.Isomorphic where

import JavaScript.Object ( Object )
import Miso.Html.Internal ( VTree (..) )

-- | Copies the DOM into the virutal DOM
copyDOMIntoVTree :: VTree a -> IO ()
copyDOMIntoVTree (VTree o) = copyDOMIntoVTree' o

foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree' :: Object -> IO ()
