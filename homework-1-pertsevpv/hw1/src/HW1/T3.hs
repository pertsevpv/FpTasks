module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                   = 0
tsize (Branch (sz, _) _ _ _) = sz

tdepth :: Tree a -> Int
tdepth Leaf                  = -1
tdepth (Branch (_, h) _ _ _) = h

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ l el r)
  | el == a = True
  | el > a = tmember a l
  | otherwise = tmember a r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a (Branch sz l el r)
  | el == a = Branch sz l el r
  | el > a = trotate $ mkBranch (tinsert a l) el r
  | otherwise = trotate $ mkBranch l el $ tinsert a r

tFromList :: Ord a => [a] -> Tree a
tFromList []       = Leaf
tFromList (x : xs) = foldr tinsert Leaf (x : xs)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l el r = Branch (tsize l + tsize r + 1, 1 + max (tdepth l) (tdepth r)) l el r

tbalanced :: Tree a -> Bool
tbalanced Leaf = True
tbalanced (Branch _ l _ r)
  | abs (tdepth l - tdepth r) > 1 = False
  | tbalanced l && tbalanced r = True
  | otherwise = False

trotate :: Tree a -> Tree a
trotate Leaf = Leaf
trotate tree@(Branch _ l el r)
  | tdepth r + 1 < tdepth l
      && tdepth (rightchild l) < tdepth (leftchild l) =
    srll tree
  | tdepth l + 1 < tdepth r
      && tdepth (leftchild r) < tdepth (rightchild r) =
    srrr tree
  | tdepth r + 1 < tdepth l
      && tdepth (rightchild l) > tdepth (leftchild l) =
    drlr tree
  | tdepth l + 1 < tdepth r
      && tdepth (leftchild r) > tdepth (rightchild r) =
    drrl tree
  | otherwise =
    mkBranch l el r

srll :: Tree a -> Tree a
srll Leaf = Leaf
srll (Branch _ l el r) =
  mkBranch
    (leftchild l)
    (tnode l)
    $ mkBranch (rightchild l) el r

srrr :: Tree a -> Tree a
srrr Leaf = Leaf
srrr (Branch _ l el r) =
  mkBranch
    (mkBranch l el $ leftchild r)
    (tnode r)
    $ rightchild r

drlr :: Tree a -> Tree a
drlr Leaf = Leaf
drlr (Branch _ l el r) =
  mkBranch
    (mkBranch (leftchild l) (tnode l) $ leftchild $ rightchild l)
    (tnode $ rightchild l)
    $ mkBranch (rightchild $ rightchild l) el r

drrl :: Tree a -> Tree a
drrl Leaf = Leaf
drrl (Branch _ l el r) =
  mkBranch
    (mkBranch l el $ leftchild $ leftchild r)
    (tnode $ leftchild r)
    $ mkBranch (rightchild $ leftchild r) (tnode r) $ rightchild r

leftchild :: Tree a -> Tree a
leftchild Leaf             = Leaf
leftchild (Branch _ l _ _) = l

rightchild :: Tree a -> Tree a
rightchild Leaf             = Leaf
rightchild (Branch _ _ _ r) = r

tnode :: Tree a -> a
tnode Leaf              = undefined
tnode (Branch _ _ el _) = el
