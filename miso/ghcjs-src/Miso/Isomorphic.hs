module Miso.Isomorphic where

import Miso.Html.Internal ( VTree (..) )

foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree :: VTree a -> IO ()
