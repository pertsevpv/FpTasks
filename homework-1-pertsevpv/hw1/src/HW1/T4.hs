module HW1.T4
  (
    tfoldr,
    treeToList,
  ) where

import           HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ res Leaf              = res
tfoldr f res (Branch _ l el r) = tfoldr f (f el $ tfoldr f res r) l

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
