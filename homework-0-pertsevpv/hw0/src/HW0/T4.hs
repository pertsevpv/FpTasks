module HW0.T4 where

import Data.Function (fix)
import GHC.Natural (Natural)

-- | repeat element as infinite list
repeat' ::
 a ->
 [a]
repeat' a = fix (a :)

-- | apply function to list
map' ::
 (a -> b) ->
 [a] ->
 [b]
map' =
  fix
    ( \rec f list -> case list of
        [] -> []
        (a : as) -> f a : rec f as
    )

-- | computes nth fibonacci number
fib ::
 Natural ->
 Natural
fib = fix (\f n -> if n <= 1 then n else f (n - 2) + f (n - 1))

-- | computes factorial of n
fac :: Natural -> Natural
fac = fix (\f n -> if n <= 1 then 1 else n * f (n - 1))
