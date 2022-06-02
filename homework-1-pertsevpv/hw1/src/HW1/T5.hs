module HW1.T5
  ( splitOn,
    joinWith,
  )
where

import           GHC.Base (NonEmpty (..))


splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr splitOnHelper ([] :| [])
  where
    splitOnHelper cur (x :| xs)
      | sep == cur = [] :| (x : xs)
      | otherwise = (cur : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith del (x :| xs) = x ++ foldr joinWithHelper [] xs
  where
    joinWithHelper res list = del : (res ++ list)
