{-# LANGUAGE TypeOperators #-}

module HW0.T1 where

-- | isomorphism between a and b
data a <-> b = Iso (a -> b) (b -> a)

-- | from a <-> b isomorphism makes b <-> a
flipIso ::
 (a <-> b) -> 
 (b <-> a)
flipIso (Iso f g) = Iso g f

-- | from a <-> b return a -> b function
runIso ::
 (a <-> b) -> 
 (a -> b)
runIso (Iso f _) = f

-- | distributivity for Either
distrib ::
 Either a (b, c) -> 
 (Either a b, Either a c)
distrib (Right (b, c)) = (Right b, Right c)
distrib (Left e) = (Left e, Left e)

-- | associativity for Pairs
assocPair ::
 (a, (b, c)) <-> ((a, b), c)
assocPair = Iso f g
  where 
    f (a, (b, c)) = ((a, b), c)
    g ((a, b), c) = (a, (b, c))

-- | associativity for Eithers
assocEither :: 
 Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso f g 
  where
    f :: Either a (Either b c) -> Either (Either a b) c
    f (Left a) = Left (Left a)
    f (Right (Left b)) = Left (Right b) 
    f (Right (Right c)) = Right c
    g :: Either (Either a b) c -> Either a (Either b c)
    g (Left (Left a)) = Left a
    g (Left (Right b)) = Right (Left b)
    g (Right c) = Right (Right c)
    