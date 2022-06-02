module HW1.T6
  ( mcat,
    epart,
  )
where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr mcatHelper mempty
  where
    mcatHelper :: Monoid a => Maybe a -> a -> a
    mcatHelper Nothing res    = res
    mcatHelper (Just val) res = val <> res

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr epartHelper (mempty, mempty)
  where
    epartHelper :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
    epartHelper (Left a) (ares, b)  = (a <> ares, b)
    epartHelper (Right b) (a, bres) = (a, b <> bres)
