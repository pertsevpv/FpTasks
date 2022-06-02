module HW0.T2 where

import           Data.Void (Void, absurd)

type Not a = a -> Void

-- | a -> !!a
doubleNeg ::
  a ->
  Not (Not a) -- (a -> Void) -> Void
doubleNeg a f = f a

-- | !!!a -> !a
reduceTripleNeg ::
  Not (Not (Not a)) ->  -- ((a -> Void) -> Void) -> Void
  Not a                 -- (a -> Void)
reduceTripleNeg = (. doubleNeg)
