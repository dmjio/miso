module Miso.Lens.Binding where

import Miso.Lens
import Miso.Types

(--->) :: Lens parent field -> Lens child field -> Binding parent child
p ---> c = ParentToChild (_get p) (_set c)
(<---) :: Lens parent field -> Lens child field -> Binding parent child
p <--- c = ChildToParent (_set p) (_get c)
(<--->) :: Lens parent field -> Lens child field -> Binding parent child
p <---> c = Bidirectional (_get p) (_set p) (_get c) (_set c)
