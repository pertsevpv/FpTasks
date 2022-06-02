{-# LANGUAGE RankNTypes #-}

module HW2.T4 where

import qualified Control.Monad
import           HW2.T1

newtype State s a = S {runS :: s -> Annotated s a}

-- | maps function to left side of Annotated s a in State s a
mapState ::
  (a -> b) ->   -- ^ map function
  State s a ->  -- ^ state to map
  State s b
mapState f (S st) = S helper
  where
    helper s = case st s of
      (a :# ss) -> f a :# ss

-- | wrap some value a into State s a
wrapState ::
  a ->      -- ^ some value
  State s a
wrapState a = S (a :#)

-- | joins nested States
joinState ::
  State s (State s a) ->  -- ^ nested states
  State s a
joinState (S st) = S helper
  where
    helper s = case st s of
      ((S r) :# ss) -> r ss

-- | modifies State with some function
modifyState ::
  (s -> s) -> -- ^ modifying function
  State s ()
modifyState f = S (\x -> () :# f x)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)


data Prim a -- ^ functions of one or two arguments
  = Add a a -- ^ addition
  | Sub a a -- ^ subtraction
  | Mul a a -- ^ multiplying
  | Div a a -- ^ division
  | Abs a   -- ^ absolute value
  | Sgn a   -- ^ sign 

data Expr
  = Val Double      -- ^ double value
  | Op (Prim Expr)  -- ^ some operation

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

-- | function that takes some expression and returns its result
eval ::
  Expr ->                     -- | expression
  State [Prim Double] Double
eval (Op (Abs x)) = do
  l <- eval x
  modifyState (Abs l :)
  pure (abs l)
eval (Op (Sgn x)) = do
  l <- eval x
  modifyState (Sgn l :)
  pure (signum l)
eval (Op op) = do
  l <- eval (leftOp op)
  r <- eval (rightOp op)
  modifyState (getConstructor op l r :)
  pure (getOp op l r)
eval (Val v) = pure v

-- | takes some operation and returns its left argument
leftOp ::
  Prim p -> -- ^ some operation
  p
leftOp (Add x _) = x
leftOp (Sub x _) = x
leftOp (Mul x _) = x
leftOp (Div x _) = x
leftOp _         = undefined

-- | takes some operation and returns its right argument
rightOp ::
  Prim p -> -- ^ some operation
  p
rightOp (Add _ y) = y
rightOp (Sub _ y) = y
rightOp (Mul _ y) = y
rightOp (Div _ y) = y
rightOp _         = undefined

-- | takes some operation and returns its function
getOp ::
  Fractional a1 =>
  Prim a2 ->
  a1 ->
  a1 ->
  a1
getOp (Add _ _) x y = x + y
getOp (Sub _ _) x y = x - y
getOp (Mul _ _) x y = x * y
getOp (Div _ _) x y = x / y
getOp _ _ _         = undefined

-- | takes some operation and returns its constructor
getConstructor ::
  Prim a1 ->  -- ^ some operation
  a2 ->
  a2 ->
  Prim a2
getConstructor (Add _ _) = Add
getConstructor (Sub _ _) = Sub
getConstructor (Mul _ _) = Mul
getConstructor (Div _ _) = Div
getConstructor _         = undefined
