{-# LANGUAGE RankNTypes #-}

module HW2.T3 where

import HW2.T1

-- | all functions below take F ( F a ) and return F a

joinOption ::
 Option (Option a) ->
 Option a
joinOption (Some a) = a
joinOption None = None

joinExcept ::
 Except e (Except e a) ->
 Except e a
joinExcept (Success a) = a
joinExcept (Error e) = Error e

joinAnnotated ::
 Semigroup e => Annotated e (Annotated e a) ->
 Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1)

joinList ::
 List (List a) ->
 List a
joinList Nil = Nil
joinList (Nil :. b) = joinList b
joinList (a :. Nil) = a
joinList ((a :. Nil) :. b) = a :. joinList b
joinList ((a :. as) :. b) = a :. joinList (as :. b)

joinFun ::
 Fun i (Fun i a) ->
 Fun i a
joinFun (F f) = F helper
  where
    helper i = case f i of
      (F g) -> g i
