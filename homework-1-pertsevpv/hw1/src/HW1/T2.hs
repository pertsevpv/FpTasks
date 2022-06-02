module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import           GHC.Natural (Natural)

data N = Z | S N deriving (Show)

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S b) = S $ nplus a b

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S b) = nplus a $ nmult a b

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp a b =
  case nsub a b of
    Just Z  -> EQ
    Nothing -> LT
    _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = nToNum a + 1

nEven, nOdd :: N -> Bool
nEven a = parityChecker a True
nOdd a = parityChecker a False

parityChecker :: N -> Bool -> Bool
parityChecker Z b     = b
parityChecker (S a) b = parityChecker a (not b)

ndiv :: N -> N -> N
ndiv _ Z = undefined
ndiv Z _ = Z
ndiv a b = divHelper (Just a) b Z
  where
    divHelper :: Maybe N -> N -> N -> N
    divHelper Nothing _ (S cnt) = cnt
    divHelper (Just a') b' cnt  = divHelper (nsub a' b') b' $ S cnt

nmod :: N -> N -> N
nmod _ Z = undefined
nmod a b = modHelper $ nsub a $ nmult b $ ndiv a b
  where
    modHelper :: Maybe N -> N
    modHelper (Just a') = a'
    modHelper Nothing   = Z
