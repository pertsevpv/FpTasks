module HW0.T5 where

import GHC.Natural (Natural)

-- | Church encoding
type Nat a = (a -> a) -> a -> a

-- | zero
nz ::
  Nat a
nz _ n = n

-- | succ
ns ::
 Nat a ->
 Nat a
ns n f x = f (n f x)

-- | addition
nplus ::
 Nat a ->
 Nat a ->
 Nat a
nplus a b f x = a f (b f x)

-- | multiplying
nmult ::
 Nat a ->
 Nat a ->
 Nat a
nmult a b f x = a (b f) x

-- | returns Church numeral from natural number
nFromNatural ::
 Natural ->
 Nat a
nFromNatural a = helper a nz
  where 
    helper 0 n = n
    helper a1 n = helper (a1 - 1) (ns n)

-- | returns num from Church numeral
nToNum ::
 Num a =>
 Nat a ->
 a
nToNum n = n (+ 1) 0
