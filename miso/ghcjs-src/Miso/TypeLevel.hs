{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
module Miso.TypeLevel where

-- | Remove duplicates from a type-level list.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Remove x xs)

-- | Remove element from a type-level list.
type family Remove x xs where
  Remove x '[]       = '[]
  Remove x (x ': ys) =      Remove x ys
  Remove x (y ': ys) = y ': Remove x ys
