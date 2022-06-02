{-# LANGUAGE InstanceSigs #-}

module HW1.T7
  (
    ListPlus (..),
    Inclusive (..),
    DotString (..),
    Fun (..),
  ) where


data ListPlus a = a :+ ListPlus a | Last a deriving (Show)
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) :: ListPlus a -> ListPlus a -> ListPlus a
  (x :+ xs) <> list = x :+ (xs <> list)
  (Last l) <> list  = l :+ list

data Inclusive a b = This a | That b | Both a b deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) :: Inclusive a b -> Inclusive a b -> Inclusive a b
  (This a) <> (This b)     = This (a <> b)
  (This a) <> (That b)     = Both a b
  (That a) <> (This b)     = Both b a
  (That a) <> (That b)     = That (a <> b)
  (Both a b) <> (This c)   = Both (a <> c) b
  (Both a b) <> (That c)   = Both a (b <> c)
  (This a) <> (Both b c)   = Both (a <> b) c
  (That a) <> (Both b c)   = Both b (a <> c)
  (Both a b) <> (Both c d) = Both (a <> c) (b <> d)

newtype DotString = DS String deriving Show

instance Semigroup DotString where
  (<>) :: DotString -> DotString -> DotString
  (DS "") <> (DS "")     = DS ""
  (DS "") <> (DS str2)   = DS str2
  (DS str1) <> (DS "")   = DS str1
  (DS str1) <> (DS str2) = DS (str1 ++ "." ++ str2)


instance Monoid DotString where
  mempty :: DotString
  mempty = DS ""

newtype Fun a = F (a -> a)

instance  Semigroup (Fun a) where
  (<>) :: Fun a -> Fun a -> Fun a
  (F f) <> (F g) = F (f . g)

instance Monoid (Fun a) where
  mempty :: Fun a
  mempty = F id
