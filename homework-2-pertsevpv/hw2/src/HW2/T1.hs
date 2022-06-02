module HW2.T1
  ( Option (..),
    Pair (..),
    Quad (..),
    Annotated (..),
    Except (..),
    Prioritised (..),
    Stream (..),
    List (..),
    Fun (..),
    Tree (..),
    mapOption,
    mapPair,
    mapQuad,
    mapAnnotated,
    mapExcept,
    mapPrioritised,
    mapStream,
    mapList,
    mapFun,
    mapTree,
  )
where


data Option a
  = None
  | Some a


mapOption ::
  (a -> b) ->
  (Option a -> Option b)
mapOption f (Some a) = Some (f a)
mapOption _ None     = None

data Pair a
  = P a a   -- ^ Pair of two values

-- | Function that modify element in Pair a
mapPair ::
  (a -> b) ->         -- ^ modifying function
  (Pair a -> Pair b)  -- ^ Pair to modify and result
mapPair f (P a b) = P (f a) (f b)

data Quad a =
  Q a a a a -- ^ Quad of 4 elements

-- | Function that modify element in Quad a
mapQuad ::
  (a -> b) ->         -- ^ modifying function
  (Quad a -> Quad b)  -- ^ Quad to modify and result
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

data Annotated e a
  = a :# e  -- ^ Value and annotation

infix 0 :#

mapAnnotated ::
  (a -> b) ->                       -- ^ modifying function
  (Annotated e a -> Annotated e b)  -- ^ Annotated to modify and result
mapAnnotated f (a :# e) = f a :# e

data Except e a
  = Error e   -- ^ Error if failed
  | Success a deriving Show -- ^ Value if success
  
-- | Function that modify element in Except e a
mapExcept ::
  (a -> b) ->                 -- ^ modifying function
  (Except e a -> Except e b)  -- ^ Except to modify and result
mapExcept f (Success a) = Success (f a)
mapExcept _ (Error e)   = Error e

data Prioritised a
  = Low a     -- ^ Low Priority
  | Medium a  -- ^ Medium Priority
  | High a    -- ^ High Priority

mapPrioritised ::
  (a -> b) ->                       -- ^ modifying function
  (Prioritised a -> Prioritised b)  -- ^ Prioritised to modify and result
mapPrioritised f (Low a)    = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a)   = High (f a)

data Stream a
  = a :> Stream a -- ^ Value and rest of stream

infixr 5 :>

-- | Function that modify element in Stream a
mapStream ::
  (a -> b) ->             -- ^ modifying function
  (Stream a -> Stream b)  -- ^ Stream to modify and result
mapStream f (a :> as) = f a :> mapStream f as

data List a
  = Nil         -- ^ End of List
  | a :. List a -- ^ Value and rest of List

infixr 5 :.

-- | Function that modify element in List a
mapList ::
  (a -> b) ->         -- ^ modifying function
  (List a -> List b)  -- ^ List to modify and result
mapList _ Nil       = Nil
mapList f (a :. as) = f a :. mapList f as

newtype Fun i a =
  F (i -> a)  -- ^ Function from i to a

-- | Function that modify element in Fun a
mapFun ::
  (a -> b) ->           -- ^ modifying function
  (Fun i a -> Fun i b)  -- ^ Fun to modify and result
mapFun f (F g) = F (f . g)

data Tree a
  = Leaf                        -- ^ Leaf
  | Branch (Tree a) a (Tree a)  -- ^ Left subtree, value and right subtree


-- | Function that modify element in Tree a
mapTree ::
  (a -> b) ->         -- ^ modifying function
  (Tree a -> Tree b)  -- ^ Tree to modify and result
mapTree _ Leaf            = Leaf
mapTree f (Branch l el r) = Branch (mapTree f l) (f el) (mapTree f r)
